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
  , text
  , rawText
  , indent
  , indent1
  , line
  , softline
  , hardline
  , lineSuffix
  , ifBreak
  , ifFlat
  , ifBreakElse
  , printDocument
  , printCompactDocument
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
  | LineSuffix Document
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

-- Prints the document at the end of the very next line.
lineSuffix :: Document -> Document
lineSuffix = LineSuffix

-- Only renders the document if we are rendering in break mode.
ifBreak :: Document -> Document
ifBreak x = Choice Empty x

-- Only renders the document if we are rendering in flat mode.
ifFlat :: Document -> Document
ifFlat x = Choice x Empty

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
  -- When we see a line document we insert a line, but we don’t insert indentation. We wait until we
  -- reach the first text or group document to insert indentation. That way we indent at the text’s
  -- level of indentation instead of the line’s indentation level.
  , layoutLine :: Bool
  -- A list of all the documents to be rendered at the end of a line. This list is in reverse order!
  , layoutLineSuffix :: [Document]
  }

-- The initial layout state.
initialState :: Int -> LayoutState
initialState maxWidth = LayoutState
  { layoutMaxWidth = maxWidth
  , layoutWidth = 0
  , layoutLine = False
  , layoutLineSuffix = []
  }

-- Produces the best possible layout for a document where all groups which can fit on a single line
-- are indeed put together on a single line. Takes some layout state and an execution stack.
--
-- As long as the execution stack is not empty we will have more document to process. The stack
-- items contain the layout mode, the current level of indentation, and the document to be rendered.
layout :: LayoutState -> [(Mode, Int, Document)] -> Layout

-- When our execution stack is empty and we have no more line suffix documents return an
-- empty layout.
layout (LayoutState { layoutLineSuffix = [] }) [] = EmptyLayout

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
-- Force the deferred indentation to be added before processing our group. Otherwise we won’t be
-- able to correctly measure the grouped content.
layout state ((Flat, i, Group x) : stack) = layout state ((Flat, i, x) : stack)
layout state@(LayoutState { layoutLine = False }) ((Break, i, Group x) : stack) =
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
    fits _ (LineLayout Flat _) = False
    fits _ (LineLayout Break _) = True
    -- Raw text always ends in a new line so behave like a new line. We do this so we can avoid
    -- measuring the raw text data. That way raw text can be represented as an opaque `Text.Builder`
    -- instead of strict `Text`.
    fits _ (RawTextLayout Flat _ _) = False
    fits _ (RawTextLayout Break _ _) = True
    -- Subtract the indentation level from the remaining characters and see if we have any more room
    -- on this line. If we don’t then return false. Otherwise recurse.
    fits k (IndentLayout j l) = let n = k - j in if n < 0 then False else fits n l

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
layout oldState@(LayoutState { layoutLine = False }) ((m, _, Text t0) : stack) =
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
layout state@(LayoutState { layoutLine = False }) ((m, i, RawText t) : stack) =
  RawTextLayout m t (layout (state { layoutWidth = 0 }) ((m, i, Line) : stack))

-- Add some indentation to the document.
layout state ((m, i, Indent n x) : stack) = layout state ((m, i + n, x) : stack)

-- Add a line to the document. Set the current character length to the line indentation level. If
-- `layoutLine` is true in old state then we won’t add indentation! This is intentional. If a line
-- immediately follows another line then that means we have an empty line which should have
-- no indentation.
--
-- Also make sure that all line suffix documents have been added to the layout before adding the
-- new line.
layout oldState@(LayoutState { layoutLineSuffix = [] }) ((m, _, Line) : stack) =
  let newState = oldState { layoutWidth = 0, layoutLine = True } in
    LineLayout m (layout newState stack)

-- Add the line suffix to our state. When we add a new line the line suffix documents will be added
-- to our stack.
layout oldState ((_, _, LineSuffix x) : stack) =
  let newState = oldState { layoutLineSuffix = x : layoutLineSuffix oldState } in
    layout newState stack

-- If we forced our deferred indentation to be added (like in the `Text` match above) then we add
-- that indentation here. If there is no indentation we don’t add an `IndentLayout`.
layout oldState@(LayoutState { layoutLine = True }) stack@((_, 0, _) : _) =
  let newState = oldState { layoutLine = False } in
    layout newState stack
layout oldState@(LayoutState { layoutLine = True }) stack@((_, i, _) : _) =
  let newState = oldState { layoutWidth = i + layoutWidth oldState, layoutLine = False } in
    IndentLayout i (layout newState stack)

-- If we forced the layout line suffix to be empty (like in the `Line` match above) then we process
-- the line suffix documents here. Remember that `layoutLineSuffix` is ordered in reverse!
layout oldState@(LayoutState { layoutLineSuffix = suffix@(_ : _) }) oldStack@((m, i, _) : _) =
  let
    newState = oldState { layoutLineSuffix = [] }
    newStack = foldl (\s x -> (m, i, x) : s) oldStack suffix
  in
    layout newState newStack

-- If we forced the layout line suffix to be empty (like in the empty stack match above) then we
-- process the line suffix documents here. Remember that `layoutLineSuffix` is ordered in reverse!
layout oldState@(LayoutState { layoutLineSuffix = suffix@(_ : _) }) [] =
  let
    newState = oldState { layoutLineSuffix = [] }
    newStack = foldl (\s x -> (Break, 0, x) : s) [] suffix
  in
    layout newState newStack

-- An intermediate representation of the document we are printing. Whereas `Document` is a tree,
-- `Layout` is a linked list which makes it linear. All groups and indentation levels have already
-- been resolved. All that’s left is to print the layout to text.
data Layout
  = EmptyLayout
  | TextLayout Mode Text Layout
  | RawTextLayout Mode Text.Builder Layout
  | IndentLayout Int Layout
  | LineLayout Mode Layout
  deriving (Show)

-- Prints the layout to text.
printLayout :: Layout -> Text.Builder
printLayout EmptyLayout = mempty
printLayout (TextLayout _ t l) = Text.Builder.fromText t <> printLayout l
printLayout (RawTextLayout _ t l) = t <> printLayout l
printLayout (IndentLayout i l) = Text.Builder.fromText (Text.replicate i " ") <> printLayout l
printLayout (LineLayout _ l) = Text.Builder.singleton '\n' <> printLayout l

-- Prints the document at the specified maximum width.
printDocument :: Int -> Document -> Text.Builder
printDocument maxWidth document =
  printLayout (layout (initialState maxWidth) [(Break, 0, document)])

-- Prints the document as compactly as possible. Hard line breaks are still accepted, but everything
-- we can fit onto one line we will.
printCompactDocument :: Document -> Text.Builder
printCompactDocument document =
  -- Max width shouldn’t matter in flat mode.
  let maxWidth = 0 in
    printLayout (layout (initialState maxWidth) [(Flat, 0, document)])
