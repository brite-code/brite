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
  , group
  , printDocument
  ) where

import qualified Data.Text as T
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
  -- Either a line or a space if we can fit the document on one line.
  | Line
  -- Tries to fit the document on one line.
  | Group Document

-- Adds two documents together.
instance Semigroup Document where
  (<>) = Concat

-- Documents may be empty.
instance Monoid Document where
  mempty = Empty

-- Adds indentation to the document.
nest :: Int -> Document -> Document
nest = Nest

-- Adds raw text to the document.
text :: T.Text -> Document
text = Text

-- Adds a newline to the document. Becomes a single space when flattened.
line :: Document
line = Line

-- Tries to fit the document on one line. If that fails then we print the document on
-- multiple lines.
group :: Document -> Document
group x = Group x

-- Flattens the document by removing newlines.
flatten :: Document -> Document
flatten Empty = Empty
flatten (Concat x y) = Concat (flatten x) (flatten y)
flatten (Nest i x) = Nest i (flatten x)
flatten x@(Text _) = x
flatten Line = Text " "
flatten (Group x) = flatten x

-- The layout of a document prepared for printing.
data Layout
  = LayoutEmpty
  | LayoutText T.Text Layout
  | LayoutLine Int Layout

-- Prints our document to a text builder.
layout :: Layout -> B.Builder
layout LayoutEmpty = mempty
layout (LayoutText t x) = B.fromText t <> layout x
layout (LayoutLine i x) = B.singleton '\n' <> B.fromText (T.replicate i " ") <> layout x

-- Picks the best layout for a document.
best :: Int -> Int -> Document -> Layout
best w k x = be w k [(0, x)]

-- Takes the width, current line length, and execution stack of items containing the indentation and
-- a document. We process the execution stack and produce the best layout from that stack.
--
-- Normally I (Caleb) don’t like short names, but these names were lifted straight from
-- Wadler’s algorithm.
be :: Int -> Int -> [(Int, Document)] -> Layout
-- If our stack is empty we have nothing.
be _ _ [] = LayoutEmpty
-- Skip empty documents in our stack.
be w k ((_, Empty) : z) = be w k z
-- Unwrap concat documents into two stack entries.
be w k ((i, Concat x y) : z) = be w k ((i, x) : (i, y) : z)
-- Nesting increases the indentation for the nested document in the execution stack. The indentation
-- only matters for newlines.
be w k ((i, Nest j x) : z) = be w k ((i + j, x) : z)
-- Text is added to the layout and increases the current line length.
be w k ((_, Text t) : z) = LayoutText t (be w (k + T.length t) z)
-- Lines are added to the layout and we continue with the execution stack setting the current line
-- length to the amount of indentation.
be w _ ((i, Line) : z) = LayoutLine i (be w i z)
-- Tries to layout the grouped document on one line. If that fails then we layout group on
-- multiple lines.
--
-- NOTE: This will be inefficient in strict code. It depends on laziness to short-circuit full
-- iteration of `z` twice.
be w k ((i, Group x) : z) =
  better w k
    (be w k ((i, flatten x) : z))
    (be w k ((i, x) : z))

-- Checks to see if the first `Layout` fits in the remaining space on our line. If it does we use
-- it. Otherwise we use the second `Layout`.
better :: Int -> Int -> Layout -> Layout -> Layout
better w k x y =
  if fits (w - k) x then x else y

-- Checks to see if the first line of `Layout` fits in the provided space.
fits :: Int -> Layout -> Bool
fits w _ | w < 0 = False
fits _ LayoutEmpty = True
fits w (LayoutText t x) = fits (w - T.length t) x
fits _ (LayoutLine _ _) = True

-- Prints the document at the specified width.
printDocument :: Int -> Document -> B.Builder
printDocument w x =
  layout (best w 0 x)
