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
-- Specifically, we’ve adapted most of the code here from Section 7 of [Phillip’s paper][2].
--
-- [1]: https://prettier.io/
-- [2]: http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.PrinterFramework
  ( Document
  , nest
  , text
  , line
  , softline
  , hardline
  , lineSuffix
  , group
  , shamefullyUngroup
  , printDocument
  ) where

import Brite.Syntax.Tokens (utf16Length)
import qualified Data.Text as T -- NOTE: We use `T.unsnoc` to count the length. The strict version is much more efficient.
import qualified Data.Text.Lazy.Builder as B

-- The document data structure is a tree which will be pretty printed at a specified text width.
data Document
  -- No content.
  = Empty
  -- Adds two documents together.
  | Concat Document Document
  -- Adds indentation to the document.
  | Nest Int Document
  -- Raw text in the document.
  | Text T.Text
  -- Adds a new line to the document at the current indentation level. There are a couple different
  -- behaviors for lines specified by `LineKind`.
  | Line LineKind
  -- Prints the document at the end of the next line.
  | LineSuffix Document
  -- Fails the layout “fit” function. Used by hardline to force the group onto multiple lines.
  | FailFit
  -- Tries to fit the document on one line.
  | Group Document

data LineKind = Space | NoSpace | Hard

-- Adds two documents together.
instance Semigroup Document where
  x <> Empty = x
  Empty <> y = y
  x <> y = Concat x y

-- Documents may be empty.
instance Monoid Document where
  mempty = Empty

-- Adds indentation to the document.
nest :: Int -> Document -> Document
nest = Nest

-- Adds raw text to the document.
text :: T.Text -> Document
text = Text

-- Specify a line break. If an expression fits on one line, the line break will be replaced with a
-- space. Line breaks always indent the next line with the current level of indentation.
line :: Document
line = Line Space

-- Specify a line break. The difference from `line` is that if the expression fits on one line, it
-- will be replaced with nothing.
softline :: Document
softline = Line NoSpace

-- Specify a line break that is always included in the output, no matter if the expression fits on
-- one line or not.
hardline :: Document
hardline = Line Hard

-- Prints the document at the end of the next line.
lineSuffix :: Document -> Document
lineSuffix = LineSuffix

-- Mark a group of items which the printer should try to fit on one line. If the printer can’t fit
-- the document on one line then
group :: Document -> Document
group x = Group x

-- If the provided document is immediately grouped then we remove the group. Otherwise we return the
-- document. Usually this is not a good idea! Which is why the name is prefixed with “shamefully”.
-- Calling this function breaks the group abstraction. Preferably one wouldn’t need to ungroup and
-- instead only group once where needed.
shamefullyUngroup :: Document -> Document
shamefullyUngroup (Group x) = x
shamefullyUngroup x = x

-- Flattens the document by removing newlines.
flatten :: Document -> Document
flatten Empty = Empty
flatten (Concat x y) = Concat (flatten x) (flatten y)
flatten (Nest _ x) = flatten x
flatten (Text t) = Text t
flatten (Line Space) = Text " "
flatten (Line NoSpace) = Text ""
flatten (Line Hard) = FailFit
flatten (LineSuffix x) = LineSuffix x
flatten FailFit = FailFit
flatten (Group x) = flatten x

-- The layout of a document prepared for printing.
data Layout
  = LayoutEmpty
  | LayoutText T.Text Layout
  | LayoutLine Int Layout
  | LayoutFailFit

-- Prints our document to a text builder.
layout :: Layout -> B.Builder
layout LayoutEmpty = mempty
layout (LayoutText t x) = B.fromText t <> layout x
layout (LayoutLine i x) = B.singleton '\n' <> B.fromText (T.replicate i " ") <> layout x
layout LayoutFailFit = mempty

-- Picks the best layout for a document.
best :: Int -> Int -> Document -> Layout
best w k x = be w k [(0, x)] []

-- Takes as parameters:
--
-- 1. The width.
-- 2. The current line length.
-- 3. The execution stack of items containing the current indentation and a document.
-- 4. The line suffix stack to be added to the main stack at the next new line.
--
-- We process the execution stack and produce the best layout from that stack.
be :: Int -> Int -> [(Int, Document)] -> [Document] -> Layout
-- If our stack is empty we have nothing.
be _ _ [] [] = LayoutEmpty
be w k [] suffix = be w k (reverse (map ((,) 0) suffix)) []
-- Skip empty documents in our stack.
be w k ((_, Empty) : stack) suffix = be w k stack suffix
-- Unwrap concat documents into two stack entries.
be w k ((i, Concat x y) : stack) suffix = be w k ((i, x) : (i, y) : stack) suffix
-- Nesting increases the indentation for the nested document in the execution stack. The indentation
-- only matters for newlines.
be w k ((i, Nest j x) : stack) suffix = be w k ((i + j, x) : stack) suffix
-- Text is added to the layout and increases the current line length.
be w k ((_, Text t) : stack) suffix = LayoutText t (be w (k + lastLineLength t) stack suffix)
-- Lines are added to the layout. Then we continue with the execution stack setting the current line
-- length to the amount of indentation.
be w _ ((i, Line _) : stack) [] = LayoutLine i (be w i stack [])
-- If our suffix is not empty then add the suffix to the stack right before the new line at the
-- line’s indentation. Process this new stack.
be w k stack@((i, Line _) : _) suffix = be w k (reverseAppend (map ((,) i) suffix) stack) []
  where
    reverseAppend [] l2 = l2
    reverseAppend (a : l1) l2 = reverseAppend l1 (a : l2)
-- Add a line suffix document to our line suffix stack.
be w k ((_, LineSuffix x) : stack) suffix = be w k stack (x : suffix)
-- If we reach a `FailFit` document then stop layout and return `LayoutFailFit`.
be _ _ ((_, FailFit) : _) _ = LayoutFailFit
-- Tries to layout the grouped document on one line. If that fails then we layout group on
-- multiple lines.
--
-- NOTE: This will be inefficient in strict code. It depends on laziness to stop evaluation when we
-- know the first iteration won’t fit on our line.
be w k ((i, Group x) : stack) suffix =
  better w k
    (be w k ((i, flatten x) : stack) suffix)
    (be w k ((i, x) : stack) suffix)

-- Checks to see if the first `Layout` fits in the remaining space on our line. If it does we use
-- it. Otherwise we use the second `Layout`.
better :: Int -> Int -> Layout -> Layout -> Layout
better w k x y =
  if fits (w - k) x then x else y

-- Checks to see if the first line of `Layout` fits in the provided space.
fits :: Int -> Layout -> Bool
fits w _ | w < 0 = False
fits _ LayoutEmpty = True
fits w (LayoutText t x) = fitsText w t x
fits _ (LayoutLine _ _) = True
fits _ LayoutFailFit = False

-- Checks to see if the text fits in the provided space. We use the same method of counting as we do
-- for ranges. (UTF-16 character length which is specified by the [LSP][1].)
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
fitsText :: Int -> T.Text -> Layout -> Bool
fitsText w _ _ | w < 0 = False
fitsText w t1 z =
  case T.uncons t1 of
    -- We only really run into new lines here in a block comment. Always fail the fit function when
    -- we find one.
    Just ('\n', _) -> False
    Just ('\r', _) -> False
    -- Subtract the UTF-16 length of this character as that’s what’s specified by the LSP spec.
    Just (c, t2) -> fitsText (w - utf16Length c) t2 z
    -- Yay! We’ve reached the end of the text and we still fit. Continue by recursing into `fits`.
    Nothing -> fits w z

-- Gets the number of characters in the last line of text. We use the same method of counting as we
-- do for ranges. (UTF-16 character length which is specified by the [LSP][1].)
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
lastLineLength :: T.Text -> Int
lastLineLength = loop 0
  where
    -- Start by looping forwards. `T.uncons` is more efficiently implemented then `T.unsnoc`. If we
    -- hit a new line then instead try again but loop backwards this time.
    loop n t1 =
      case T.uncons t1 of
        Just ('\n', t2) -> loopBack 0 t2
        Just ('\r', t2) -> loopBack 0 t2
        Just (c, t2) -> loop (n + utf16Length c) t2
        Nothing -> n

    -- We discovered that our text is multiple lines long, but we only want the length of the last
    -- line. So loop backwards from the end of text to the last new line.
    loopBack n t1 =
      case T.unsnoc t1 of
        Just (_, '\n') -> n
        Just (_, '\r') -> n
        Just (t2, c) -> loopBack (n + utf16Length c) t2
        Nothing -> n

-- Prints the document at the specified width.
printDocument :: Int -> Document -> B.Builder
printDocument w x =
  layout (best w 0 x)
