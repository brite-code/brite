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
forceBreak = tryFlat Line Empty

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
line :: Document
line = tryFlat (Text " ") Line

-- Adds a new line to the document. If we are attempting to print the document on one line then we
-- convert this to an empty document.
softline :: Document
softline = tryFlat Empty Line

-- Adds a new line to the document. This will prevent any group it is in from being printed on
-- one line.
hardline :: Document
hardline = Line

-- Prints the document either on the current line if it just started or the beginning of the next
-- new line.
--
-- Line prefixes are always followed by a new line. This is so we don’t have to layout the text
-- before this command again.
linePrefix :: Document -> Document
linePrefix = LinePrefix

-- Prints the document at the end of the very next line.
lineSuffix :: Document -> Document
lineSuffix = LineSuffix

-- If we are printing in flat mode then the first document will be used. If we are printing in break
-- mode then the second document will be used.
tryFlat :: Document -> Document -> Document
tryFlat = Choice

-- Only renders the document if we are rendering in break mode.
ifBreak :: Document -> Document
ifBreak x = Choice Empty x

-- Only renders the document if we are rendering in break mode. If we are in flat mode then we
-- render the second document.
ifBreakElse :: Document -> Document -> Document
ifBreakElse x y = Choice y x

-- The current mode in which a document is being printed.
data Mode = Break | Flat

-- Produces the best possible layout for a document where all groups which can fit on a single line
-- are indeed put together on a single line. The parameters are:
--
-- * `w` which represents the desired maximum width of our finished document. If a flattened group
--   exceeds this width it will not be flattened and instead will be broken onto multiple lines.
-- * `k` which represents the current length of the line we are laying out.
-- * `stack` which represents our execution stack. As long as this stack is not empty we will have
--   more document to process. The stack items contain the layout mode, the current level of
--   indentation, and the document to be rendered.
layout :: Int -> Int -> [(Mode, Int, Document)] -> Layout
layout _ _ [] = EmptyLayout
layout w k ((_, _, Empty) : z) = layout w k z

-- Add both documents in a concatenation to our execution stack.
layout w k ((m, i, Concat x y) : z) = layout w k ((m, i, x) : (m, i, y) : z)

-- Choose one of the two documents based on our printing mode.
layout w k ((Flat, i, Choice x _) : z) = layout w k ((Flat, i, x) : z)
layout w k ((Break, i, Choice _ y) : z) = layout w k ((Break, i, y) : z)

-- Groups in break mode attempt to layout their document on a single line using flat mode. If the
-- grouped document does not fit on a single line then we render the group in break mode.
--
-- If there is a new line in the flattened group then it automatically fails to fit in a single
-- line. However, when we see a new line outside of the flattened group we know to accept the
-- flattened layout.
layout w k ((Flat, i, Group x) : z) = layout w k ((Flat, i, x) : z)
layout w k ((Break, i, Group x) : z) =
  let l1 = layout w k ((Flat, i, x) : z) in
    if fits (w - k) l1 then l1 else layout w k ((Break, i, x) : z)
  where
    -- Of course empty fits!
    fits _ EmptyLayout = True
    -- Measure the length of the text to determine if it fits on this line.
    fits r (TextLayout m t l) = fitsText r m t l
    -- If there is a new line in flat mode we can’t fit the document. Otherwise we’ve reached the
    -- end of the line and our flattened document fits!
    fits _ (LineLayout Flat _ _) = False
    fits _ (LineLayout Break _ _) = True
    -- Raw text always ends in a new line so behave like a new line. We do this so we can avoid
    -- measuring the raw text data. That way raw text can be represented as an opaque `Text.Builder`
    -- instead of strict `Text`.
    fits _ (RawTextLayout Flat _ _ _) = False
    fits _ (RawTextLayout Break _ _ _) = True
    -- Line prefix and line suffix layouts don’t affect our measurements.
    fits r (LinePrefixLayout _ l) = fits r l
    fits r (LineSuffixLayout _ l) = fits r l

    -- Iterate through the text to see if it fits on one line.
    --
    -- Remember that we use UTF-16 to measure length because that is the encoding specified for
    -- character widths by the Language Server Protocol (LSP) specification.
    fitsText r m t0 l =
      case Text.uncons t0 of
        -- If `r` is less than 0 we’ve run out of space. Our text does not fit.
        _ | r < 0 -> False
        -- If there is a new line in the text then whether or not we fit depends on the mode.
        Just ('\n', _) -> case m of { Flat -> False; Break -> True }
        Just ('\r', _) -> case m of { Flat -> False; Break -> True }
        -- Subtract the character length and continue.
        Just (c, t1) -> fitsText (r - utf16Length c) m t1 l
        -- This text fits, but there’s still more layout to measure.
        Nothing -> fits r l

-- Add text to the document. To compute the new position:
--
-- * If the text is one line add the length of our text to the current position.
-- * If the text is multiple lines set the current position to the length of our last line.
--
-- Remember that we use UTF-16 to measure length because that is the encoding specified for
-- character widths by the Language Server Protocol (LSP) specification.
layout w k0 ((m, _, Text t0) : z) = TextLayout m t0 (layout w (measure k0 t0) z)
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
-- means we don’t need to measure the text. Set the current character length to the line
-- indentation level.
layout w _ ((m, i, RawText t) : z) = RawTextLayout m t i (layout w i z)

-- Add some indentation to the document.
layout w k ((m, i, Indent n x) : z) = layout w k ((m, i + n, x) : z)

-- Add a line to the document. Set the current character length to the line indentation level.
layout w _ ((m, i, Line) : z) = LineLayout m i (layout w i z)

-- Layout the line prefix independently of the rest of our document execution stack. Set the
-- current character and current indentation both to 0. In `printLayout` we will add the proper
-- indentation level to our line prefix. Always layout the line prefix in flat mode since we don’t
-- have a proper current character value.
layout w k ((_, _, LinePrefix x) : z) = LinePrefixLayout (layout w 0 [(Flat, 0, x)]) (layout w k z)

-- Layout the line suffix independently of the rest of our document execution stack. Set the
-- current character and current indentation both to 0. In `printLayout` we will add the proper
-- indentation level to our line suffix. Always layout the line suffix in flat mode since we don’t
-- have a proper current character value.
layout w k ((_, _, LineSuffix x) : z) = LineSuffixLayout (layout w 0 [(Flat, 0, x)]) (layout w k z)

-- An intermediate representation of the document we are printing. Whereas `Document` is a tree,
-- `Layout` is a linked list which makes it linear. All groups and indentation levels have already
-- been resolved. All that’s left is to print the layout to text.
data Layout
  = EmptyLayout
  | TextLayout Mode Text Layout
  | RawTextLayout Mode Text.Builder Int Layout
  | LineLayout Mode Int Layout
  | LinePrefixLayout Layout Layout
  | LineSuffixLayout Layout Layout

-- Prints the layout out to text.
printLayout :: Layout -> Text.Builder
printLayout = loop 0 (0, mempty) mempty mempty
  where
    loop _ (j, t1) t2 t3 EmptyLayout = t1 <> t2 <> t3 j
    loop i t1 t2 t3 (TextLayout _ t l) = loop i t1 (t2 <> Text.Builder.fromText t) t3 l
    loop i t1 t2 t3 (RawTextLayout m t j l) = loop i t1 (t2 <> t) t3 (LineLayout m j l)
    loop i (_, t1) t2 t3 (LineLayout _ j l) =
      t1 <> t2 <> t3 j <> printLine (i + j) <> loop i (j, mempty) mempty (\_ -> mempty) l
    loop i (j, t1) t2 t3 (LinePrefixLayout l1 l2) =
      loop i (j, t1 <> loop (i + j) (0, mempty) mempty mempty l1 <> printLine (i + j)) t2 t3 l2
    loop i t1 t2 t3 (LineSuffixLayout l1 l2) =
      loop i t1 t2 (\j -> t3 j <> loop (i + j) (0, mempty) mempty mempty l1) l2

-- Prints a single line at the current level of indentation.
printLine :: Int -> Text.Builder
printLine i = Text.Builder.singleton '\n' <> Text.Builder.fromText (Text.replicate i " ")

-- Prints the document at the specified maximum width.
printDocument :: Int -> Document -> Text.Builder
printDocument maxWidth rootDocument = printLayout (layout maxWidth 0 [(Break, 0, rootDocument)])
