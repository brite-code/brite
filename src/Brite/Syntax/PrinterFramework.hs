-- Framework for printing tree like data structures. There are a number of places where we might
-- need a printing framework such as:
--
-- * For the Brite AST itself.
-- * For debugging various internal data structures.
-- * For compiled JavaScript code.
--
-- As such we maintain a printing framework which uses the same algorithm popularized by the
-- [JavaScript framework Prettier][1] which in turn was taken from [Phillip Wadler’s paper “A
-- prettier printer”][2].
--
-- A lot of this implementation is either inspired or directly lifted from [Prettier][1] or
-- [Wadler’s paper][2]. Particularly a lot of the command names are taken from [Prettier][1].
--
-- [1]: https://prettier.io
-- [2]: http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.PrinterFramework
  ( Document
  , group
  , forceBreak
  , text
  , rawText
  , indent
  , indent1
  , line
  , softline
  , hardline
  , linePrefix
  , lineSuffix
  , ifBreak
  , ifBreakElse
  , printDocument
  ) where

import Brite.Syntax.Tokens (utf16Length)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

-- A text document with the ability to be pretty-printed.
data Document
  = Empty
  | Concat Document Document
  | Choice Document Document
  | Group Document
  | Text Text
  | RawText Text.Builder
  | Indent Int Document
  | Line
  | LinePrefix Document
  | LineSuffix Document
  | ForceBreak
  deriving (Show)

-- Documents may be added to each other.
instance Semigroup Document where
  x <> Empty = x
  Empty <> y = y
  x <> y = Concat x y

-- Documents may be empty.
instance Monoid Document where
  mempty = Empty

-- A grouped document attempts to layout its contents on a single line if the contents fit within
-- the maximum width the document is being printed at.
group :: Document -> Document
group = Group

-- Always forces the group to break. If this command is inside of a group it will never fit on
-- one line.
forceBreak :: Document
forceBreak = Choice ForceBreak Empty

-- Adds some raw text to the document.
text :: Text -> Document
text = Text

-- Inserts some raw text into the document. Always inserts a new line afterwards so we don’t have to
-- measure the text.
rawText :: Text.Builder -> Document
rawText = RawText

-- Adds a level of indentation to the document. (Two spaces.)
indent :: Document -> Document
indent = Indent 2

-- Adds one space of indentation to the document.
indent1 :: Document -> Document
indent1 = Indent 1

-- Adds a new line to the document. If we are attempting to print the document on one line then we
-- convert this to a single space.
--
-- The line will use the indentation level of the very next printed text. Not the current
-- indentation level of this line’s context.
line :: Document
line = Choice (Text " ") Line

-- Adds a new line to the document. If we are attempting to print the document on one line then we
-- convert this to an empty document.
--
-- The line will use the indentation level of the very next printed text. Not the current
-- indentation level of this line’s context.
softline :: Document
softline = Choice Empty Line

-- Adds a new line to the document. This will prevent any group it is in from being printed on
-- one line.
--
-- The line will use the indentation level of the very next printed text. Not the current
-- indentation level of this line’s context.
hardline :: Document
hardline = Line

-- Prints the document either on the current line if it just started or the beginning of the next
-- new line.
--
-- Line prefixes are always followed by a new line. This is so we don’t have to layout the text
-- before this command again.
--
-- The line prefix will take the indentation level of the line at the location where the line
-- was printed.
linePrefix :: Document -> Document
linePrefix = LinePrefix

-- Prints the document at the end of the very next line.
lineSuffix :: Document -> Document
lineSuffix = LineSuffix

-- Only renders the document if we are rendering in break mode.
ifBreak :: Document -> Document
ifBreak x = Choice Empty x

-- Only renders the document if we are rendering in break mode. If we are in flat mode then we
-- render the second document.
ifBreakElse :: Document -> Document -> Document
ifBreakElse x y = Choice y x

-- The current mode in which a document is being printed.
data Mode = Break | Flat deriving (Show)

-- The state needed for our layout process.
data LayoutState = LayoutState
  -- The desired maximum width of our finished document. If a flattened group exceeds this width the
  -- group will not be flattened and instead  will be broken onto multiple lines. Measured in UTF-16
  -- encoded code units.
  { layoutMaxWidth :: Int
  -- The current length of the line we are laying out. Measured in UTF-16 encoded code units.
  , layoutWidth :: Int
  -- When we see a line document we don’t immediately insert a line. Instead we defer the line
  -- insertion for later. Specifically the first text we see. This is because we want to use the
  -- indentation level of the first text on our line instead of using the indentation level where
  -- the line was defined.
  --
  -- We implement this as a function so line prefixes and suffixes can also be deferred.
  , layoutDeferredLine :: Maybe (Int -> Layout -> Layout)
  }

-- Produces the best possible layout for a document where all groups which can fit on a single line
-- are indeed put together on a single line. Takes some layout state and an execution stack.
--
--As long as the execution stack is not empty we will have more document to process. The stack items
-- contain the layout mode, the current level of indentation, and the document to be rendered.
layout :: LayoutState -> [(Mode, Int, Document)] -> Layout

-- When our execution stack is empty return an empty layout. If we’ve deferred a line add it now
-- with zero indentation.
layout (LayoutState { layoutMaxWidth = _, layoutWidth = _, layoutDeferredLine = l }) [] =
  case l of
    Nothing -> EmptyLayout
    Just f -> f 0 EmptyLayout

-- Skip empty documents.
layout state ((_, _, Empty) : stack) = layout state stack

-- Add both documents in a concatenation to our execution stack.
layout state ((m, i, Concat x y) : stack) = layout state ((m, i, x) : (m, i, y) : stack)

-- Choose one of the two documents based on our printing mode.
layout state ((Flat, i, Choice x _) : stack) = layout state ((Flat, i, x) : stack)
layout state ((Break, i, Choice _ y) : stack) = layout state ((Break, i, y) : stack)

-- Groups in break mode attempt to layout their document on a single line using flat mode. If the
-- grouped document does not fit on a single line then we render the group in break mode.
--
-- If there is a new line in the flattened group then it automatically fails to fit in a single
-- line. However, when we see a new line outside of the flattened group we know to accept the
-- flattened layout.
--
-- Force the deferred line to be added before processing our group. Otherwise we won’t be able to
-- correctly measure the grouped content.
layout state ((Flat, i, Group x) : stack) = layout state ((Flat, i, x) : stack)
layout state@(LayoutState { layoutDeferredLine = Nothing }) ((Break, i, Group x) : stack) =
  let l1 = layout state ((Flat, i, x) : stack) in
    if fits (layoutMaxWidth state - layoutWidth state) l1 then l1
    else layout state ((Break, i, x) : stack)
  where
    -- Of course empty fits!
    fits _ EmptyLayout = True
    -- Measure the length of the text to determine if it fits on this line.
    fits k (TextLayout m t l) = fitsText k m t l
    -- If there is a new line in flat mode we can’t fit the document. Otherwise we’ve reached the
    -- end of the line and our flattened document fits!
    fits _ (LineLayout Flat _ _ _) = False
    fits _ (LineLayout Break _ _ _) = True
    -- Raw text always ends in a new line so behave like a new line. We do this so we can avoid
    -- measuring the raw text data. That way raw text can be represented as an opaque `Text.Builder`
    -- instead of strict `Text`.
    fits _ (RawTextLayout Flat _ _) = False
    fits _ (RawTextLayout Break _ _) = True
    -- Line prefix and line suffix layouts don’t affect our measurements.
    fits k (LinePrefixLayout _ l) = fits k l
    fits k (LineSuffixLayout _ l) = fits k l
    -- Force break layouts never fit.
    fits _ (ForceBreakLayout _) = False

    -- Iterate through the text to see if it fits on one line.
    --
    -- Remember that we use UTF-16 to measure length because that is the encoding specified for
    -- character widths by the Language Server Protocol (LSP) specification.
    fitsText k m t0 l =
      case Text.uncons t0 of
        -- If `r` is less than 0 we’ve run out of space. Our text does not fit.
        _ | k < 0 -> False
        -- If there is a new line in the text then whether or not we fit depends on the mode.
        Just ('\n', _) -> case m of { Flat -> False; Break -> True }
        Just ('\r', _) -> case m of { Flat -> False; Break -> True }
        -- Subtract the character length and continue.
        Just (c, t1) -> fitsText (k - utf16Length c) m t1 l
        -- This text fits, but there’s still more layout to measure.
        Nothing -> fits k l

-- Add text to the document. To compute the new position:
--
-- * If the text is one line add the length of our text to the current position.
-- * If the text is multiple lines set the current position to the length of our last line.
--
-- Remember that we use UTF-16 to measure length because that is the encoding specified for
-- character widths by the Language Server Protocol (LSP) specification.
layout oldState@(LayoutState { layoutDeferredLine = Nothing }) ((m, _, Text t0) : stack) =
  let newState = oldState { layoutWidth = measure (layoutWidth oldState) t0 } in
    TextLayout m t0 (layout newState stack)
  where
    -- Measure the length of our text until we either reach the text’s end or a new line. If we
    -- reach a new line then use `measureBackwards` to measure the length of the last line.
    measure k1 t1 =
      case Text.uncons t1 of
        Nothing -> k1
        Just ('\n', t2) -> measureBackwards 0 t2
        Just ('\r', t2) -> measureBackwards 0 t2
        Just (c, t2) -> measure (k1 + utf16Length c) t2

    -- Iterate in reverse through the text until we reach the first new line to measure the length
    -- of the last line.
    measureBackwards k1 t1 =
      case Text.unsnoc t1 of
        Nothing -> k1
        Just (_, '\n') -> k1
        Just (_, '\r') -> k1
        Just (t2, c) -> measureBackwards (k1 + utf16Length c) t2

-- Add raw text to the document. Raw text always ends in a new line. Which is important because it
-- means we don’t need to measure the text.
layout oldState@(LayoutState { layoutDeferredLine = Nothing }) ((m, i, RawText t) : stack) =
  let newState = oldState { layoutWidth = 0, layoutDeferredLine = Just (LineLayout m i) } in
    RawTextLayout m t (layout newState stack)

-- Add some indentation to the document.
layout state ((m, i, Indent n x) : stack) = layout state ((m, i + n, x) : stack)

-- Add a line to the document. Set the current character length to the line indentation level.
layout oldState@(LayoutState { layoutDeferredLine = Nothing }) ((m, i, Line) : stack) =
  let newState = oldState { layoutWidth = 0, layoutDeferredLine = Just (LineLayout m i) } in
    layout newState stack

-- If there is a deferred line then we want to add it, but the deferred line should have
-- no indentation since the line will be empty.
layout oldState@(LayoutState { layoutDeferredLine = Just f }) ((m, i, Line) : stack) =
  let newState = oldState { layoutWidth = 0, layoutDeferredLine = Just (LineLayout m i) } in
    f 0 (layout newState stack)

-- Layout the line prefix independently of the rest of our document execution stack. In
-- `printLayout` we will add the proper indentation level to our line prefix. Always layout the line
-- prefix in flat mode since we don’t have a proper current character value.
layout state ((_, _, LinePrefix x) : stack) =
  let
    prefixState = LayoutState { layoutMaxWidth = layoutMaxWidth state, layoutWidth = 0, layoutDeferredLine = Nothing }
    prefix = LinePrefixLayout (layout prefixState [(Flat, 0, x)])
  in
    case layoutDeferredLine state of
      Nothing -> prefix (layout state stack)
      Just f ->
        let newState = state { layoutDeferredLine = Just (\i y -> f i (prefix y)) } in
          layout newState stack

-- Layout the line suffix independently of the rest of our document execution stack. In
-- `printLayout` we will add the proper indentation level to our line suffix. Always layout the line
-- suffix in flat mode since we don’t have a proper current character value.
layout state ((_, _, LineSuffix x) : stack) =
  let
    suffixState = LayoutState { layoutMaxWidth = layoutMaxWidth state, layoutWidth = 0, layoutDeferredLine = Nothing }
    suffix = LineSuffixLayout (layout suffixState [(Flat, 0, x)])
  in
    case layoutDeferredLine state of
      Nothing -> suffix (layout state stack)
      Just f ->
        let newState = state { layoutDeferredLine = Just (\i y -> f i (suffix y)) } in
          layout newState stack

-- Add a force break layout in flat mode.
layout state ((Flat, _, ForceBreak) : stack) = ForceBreakLayout (layout state stack)
layout state ((Break, _, ForceBreak) : stack) = layout state stack

-- If we forced our deferred line to be added (like in the `Text` match above) then we add that
-- line here.
layout oldState@(LayoutState { layoutDeferredLine = Just f }) stack@((_, i, _) : _) =
  let newState = oldState { layoutWidth = i + layoutWidth oldState, layoutDeferredLine = Nothing } in
    f i (layout newState stack)

-- An intermediate representation of the document we are printing. Whereas `Document` is a tree,
-- `Layout` is a linked list which makes it linear. All groups and indentation levels have already
-- been resolved. All that’s left is to print the layout to text.
data Layout
  = EmptyLayout
  | TextLayout Mode Text Layout
  | RawTextLayout Mode Text.Builder Layout
  | LineLayout Mode Int Int Layout
  | LinePrefixLayout Layout Layout
  | LineSuffixLayout Layout Layout
  | ForceBreakLayout Layout
  deriving (Show)

-- Prints the layout to text.
printLayout :: Layout -> Text.Builder
printLayout = loop 0 (0, 0, mempty) mempty (\_ -> mempty)
  where
    -- Ok, so I (Caleb) realize that Haskell isn’t the easiest language to read. Especially code
    -- using advanced Haskell features. But this loop specifically is pretty gnarly. I’m going to
    -- try my best to explain it. Both to programmers wishing to make bug fixes to this print loop
    -- and to myself in the future.
    --
    -- This function would be pretty straightforward if we didn’t have to deal with line prefixes
    -- and suffixes. But we do have to deal with prefixes and suffixes so we’re forced to build our
    -- text line by line and then add the line to our full text.
    --
    -- Three `Text.Builder` parameters on this function are dedicated to building the current line.
    --
    -- The plain `Text.Builder` (third parameter) is the main content for the line. Every time we
    -- add plain text it goes here.
    --
    -- `(Int, Int, Text.Builder)` builds the line prefix. It holds two integers. Both from
    -- `LineLayout`. The first is the indentation level where we added the `LineLayout`. The second
    -- is the indentation level where the first `TextLayout` after the `LineLayout` was added. We
    -- print the text following a `LineLayout` at the second indentation but we print line prefixes
    -- at the first indentation. We carry around both integers since we need to compute
    -- the difference.
    --
    -- Here’s the problem we are trying to solve:
    --
    -- ```ite
    -- do {
    --   // Hello, world!
    -- }
    -- ```
    --
    -- The above code is printed from the document:
    --
    -- ```hs
    -- text "do {"
    --   <> indent softline
    --   <> linePrefix (text "// Hello, world!")
    --   <> text "}"
    -- ```
    --
    -- We print the line prefix with the comment _outside_ of the `indent`. However, the line we are
    -- prefixing is inside the `indent`! The `LineLayout` added by our `softline` will be
    -- `LineLayout Break 2 0`. Since we add the line inside an `indent` we set the first integer to
    -- two. But the first text we add to the document is `}` with an indentation of zero.
    --
    -- `(Int -> Text.Builder)` builds the line suffix. We won’t know the indentation level for our
    -- line suffix until we add the next line. So we build the line suffix in a function. When we
    -- add the next line we will call the line suffix function with the correct indentation level.
    --
    -- The first parameter which is an `Int` is interesting. Notice how the declaration for
    -- `LinePrefixLayout` is `LinePrefixLayout Layout Layout`? (Same for `LineSuffixLayout`.) That
    -- is the line prefix is encoded as a `Layout`. This means to print a line prefix/suffix we need
    -- to recursively call `printLayout`. However, when we create the prefix/suffix layout in the
    -- `layout` function above we set the indentation level to zero since we don’t know the
    -- indentation we’ll want to use until we are here in `printLayout`. So this first parameter is
    -- the “offset” we’ll add to all our indentations.

    loop :: Int -> (Int, Int, Text.Builder) -> Text.Builder -> (Int -> Text.Builder) -> Layout -> Text.Builder

    -- If we’ve reached the end of our layout add the current prefix, content, and suffix together.
    loop _ (_, _, prefix) content suffix EmptyLayout = prefix <> content <> suffix 0

    -- Add text to our current content and recurse.
    loop offset prefix content suffix (TextLayout _ t l) =
      loop offset prefix (content <> Text.Builder.fromText t) suffix l

    -- Add raw text to our current content and recurse.
    loop offset prefix content suffix (RawTextLayout _ t l) =
      loop offset prefix (content <> t) suffix l

    -- If we’ve reached a new line then add the prefix, content, and suffix we’ve been building to
    -- the document. The suffix will use the indentation level of the line itself. We create a new
    -- line with the new indentation level. Recurse with empty builders for prefix, content,
    -- and suffix.
    loop offset (_, _, prefix) content suffix (LineLayout _ i1 i2 l) =
      prefix <> content <> suffix i1
        <> printLine (offset + i2)
        <> loop offset (i1, i2, mempty) mempty (\_ -> mempty) l

    -- Layout the new prefix and add it to the current prefix. Then recurse.
    loop offset (i1, i2, prefix) content suffix (LinePrefixLayout prefixLayout l) =
      let
        newPrefix =
          -- The previous new line was printed with `i2` as the indentation level, but we want this
          -- line to be printed with `i1` as the indentation level. So take the difference and add
          -- that many spaces.
          printSpaces (max 0 (i1 - i2))
            -- Layout the prefix with an offset of `i1`.
            <> loop (offset + i1) (0, 0, mempty) mempty mempty prefixLayout
            -- Add a new line at the level of `i2`.
            <> printLine (offset + i2)
      in
        loop offset (i1, i2, prefix <> newPrefix) content suffix l

    -- Layout the new suffix (when the indentation level is available) and add it to the current
    -- suffix. Then recurse.
    loop offset prefix content suffix (LineSuffixLayout suffixLayout l) =
      loop offset prefix content (\i ->
        suffix i <> loop (offset + i) (0, 0, mempty) mempty mempty suffixLayout) l

    -- Force breaks don’t affect the printing of layout.
    loop offset prefix content suffix (ForceBreakLayout l) = loop offset prefix content suffix l

    -- Utilities for printing new lines at the provided level of indentation.
    printLine i = Text.Builder.singleton '\n' <> printSpaces i
    printSpaces i = Text.Builder.fromText (Text.replicate i " ")

-- Prints the document at the specified maximum width.
printDocument :: Int -> Document -> Text.Builder
printDocument maxWidth rootDocument =
  printLayout (layout initialState [(Break, 0, rootDocument)])
  where
    initialState = LayoutState
      { layoutMaxWidth = maxWidth
      , layoutWidth = 0
      , layoutDeferredLine = Nothing
      }
