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
  , indent
  , line
  , softline
  , hardline
  , lineSuffix
  , shamefullyUngroup
  , printDocument
  ) where

import Brite.Syntax.Tokens (utf16Length)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

-- A text document with the ability to be pretty-printed.
data Document
  = Empty
  | Concat Document Document
  | Choice Document Document
  | Group Document
  | Text T.Text
  | Indent Document
  | Line
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

-- Adds some raw text to the document.
text :: T.Text -> Document
text = Text

-- Adds a level of indentation to the document.
indent :: Document -> Document
indent = Indent

-- Adds a new line to the document. If we are attempting to print the document on one line then we
-- convert this to a single space.
line :: Document
line = ifFlat (Text " ") Line

-- Adds a new line to the document. If we are attempting to print the document on one line then we
-- convert this to an empty document.
softline :: Document
softline = ifFlat Empty Line

-- Adds a new line to the document. This will prevent any group it is in from being printed on
-- one line.
hardline :: Document
hardline = Line

-- Prints the document at the end of the very next line.
lineSuffix :: Document -> Document
lineSuffix = LineSuffix

-- If we are printing in flat mode then the first document will be used. If we are printing in break
-- mode then the second document will be used.
ifFlat :: Document -> Document -> Document
ifFlat = Choice

-- If the provided document is immediately grouped then we remove the group. Otherwise we return the
-- document. Usually this is not a good idea! Which is why the name is prefixed with “shamefully”.
-- Calling this function breaks the group abstraction. Preferably one wouldn’t need to ungroup and
-- instead only group once where needed.
shamefullyUngroup :: Document -> Document
shamefullyUngroup (Group x) = x
shamefullyUngroup x = x

-- The current mode in which a document is being printed.
data Mode = Flat | Break

data LayoutState = LayoutState
  -- The current character position measured in UTF-16 encoded code units as specified
  -- by the LSP.
  { layoutCharacter :: Int
  -- Line suffix documents. This list is reverse ordered.
  , layoutLineSuffix :: [Document]
  }

-- Prints the document at the specified maximum width.
printDocument :: Int -> Document -> B.Builder
printDocument maxWidth rootDocument = layout initialState [(Break, 0, rootDocument)]
  where
    initialState = LayoutState
      { layoutCharacter = 0
      , layoutLineSuffix = []
      }

    -- Responsible for laying out a document. Takes as parameters:
    --
    -- * State for our layout process.
    -- * An execution stack of documents. We build our document by processing every item on the
    --   stack. Each stack item contains the mode in which we print the document, the current level
    --   of indentation, and the document to be printed.
    layout :: LayoutState -> [(Mode, Int, Document)] -> B.Builder

    -- We are done processing our stack!
    layout (LayoutState { layoutLineSuffix = [] }) [] = mempty

    -- If there are some line suffix documents remaining add them to our stack and process them.
    layout s [] =
      layout (s { layoutLineSuffix = [] }) (map ((,,) Break 0) (reverse (layoutLineSuffix s)))

    -- Skip empty documents in the stack.
    layout s ((_, _, Empty) : stack) = layout s stack

    -- Add both documents of a concatenation to the stack.
    layout s ((m, i, Concat x y) : stack) = layout s ((m, i, x) : (m, i, y) : stack)

    -- Make a choice depending on the mode. `Flat` chooses the left and `Break` chooses the right.
    layout s ((Flat, i, Choice x _) : stack) = layout s ((Flat, i, x) : stack)
    layout s ((Break, i, Choice _ y) : stack) = layout s ((Break, i, y) : stack)

    -- Flat groups continue to flatten their document. It is break groups which are interesting.
    -- Break mode groups attempt to flatten their document.
    layout s ((Flat, i, Group x) : stack) = layout s ((Flat, i, x) : stack)

    -- A break mode group attempts to flatten its document using the `tryLayout` function to see if
    -- it will fit on one line. If it does not then we layout the group using our break mode.
    layout s ((Break, i, Group x) : stack) =
      case tryLayout s ((Flat, i, x) : stack) of
        Just t -> t
        Nothing -> layout s ((Break, i, x) : stack)

    -- Add text to the document. If the text is a single line then we add the UTF-16 encoded code
    -- point length to the position to get our new position. If the text is multiple lines we set
    -- the new position to the UTF-16 length of the last line.
    --
    -- Remember that we use UTF-16 to measure length because that is the encoding specified for
    -- character widths by the Language Server Protocol (LSP) specification.
    layout s ((_, _, Text t0) : stack) =
      let k = loop (layoutCharacter s) t0 in
        B.fromText t0 <> layout (s { layoutCharacter = k }) stack
      where
        -- Measure the length of our text until we either reach the text’s end or a new line. If we
        -- reach a new line then use `loopBack` to measure the length of the last line.
        loop k t1 =
          case T.uncons t1 of
            Nothing -> k
            Just ('\n', t2) -> loopBack 0 t2
            Just ('\r', t2) -> loopBack 0 t2
            Just (c, t2) -> loop (k + utf16Length c) t2

        -- Iterate in reverse through the text until we reach the first new line to measure the
        -- length of the last line.
        loopBack k t1 =
          case T.unsnoc t1 of
            Nothing -> k
            Just (_, '\n') -> k
            Just (_, '\r') -> k
            Just (t2, c) -> loopBack (k + utf16Length c) t2

    -- Add some indentation.
    layout s ((m, i, Indent x) : stack) = layout s ((m, i + 2, x) : stack)

    -- Add a new line at the current level of indentation. If there are some line suffix documents
    -- then add them to the execution stack and process them.
    layout (LayoutState { layoutLineSuffix = [] }) ((_, i, Line) : stack) =
      let s = LayoutState { layoutCharacter = i, layoutLineSuffix = [] } in
        B.singleton '\n' <> B.fromText (T.replicate i " ") <> layout s stack
    layout s stack@((m, i, Line) : _) =
      layout (s { layoutLineSuffix = [] }) $
        foldl (flip (:)) stack (map ((,,) m i) (layoutLineSuffix s))

    -- Add line suffix documents to the suffix stack.
    layout s ((_, _, LineSuffix x) : stack) =
      layout (s { layoutLineSuffix = x : layoutLineSuffix s }) stack

    -- Attempts to layout a flat mode document.
    --
    -- * Returns `Nothing` if `k` exceeds `maxWidth`.
    -- * Returns `Nothing` if we find a new line in a flat mode document.
    -- * Returns `Just` if the first line fits in `maxWidth`.
    tryLayout :: LayoutState -> [(Mode, Int, Document)] -> Maybe B.Builder

    -- If our line has exceeded `maxWidth` return `Nothing`.
    tryLayout s _ | layoutCharacter s > maxWidth = Nothing

    -- If we have reached the end of the stack we’re ok!
    tryLayout s [] = Just (layout s [])

    -- Skip empty documents in the stack.
    tryLayout s ((_, _, Empty) : stack) = tryLayout s stack

    -- Add both documents of a concatenation to the stack.
    tryLayout s ((m, i, Concat x y) : stack) = tryLayout s ((m, i, x) : (m, i, y) : stack)

    -- Make a choice depending on the mode. `Flat` chooses the left and `Break` chooses the right.
    tryLayout s ((Flat, i, Choice x _) : stack) = tryLayout s ((Flat, i, x) : stack)
    tryLayout s ((Break, i, Choice _ y) : stack) = tryLayout s ((Break, i, y) : stack)

    -- Attempt to layout a group in the current specified mode. We don’t attempt to flatten break
    -- mode documents. Instead we lay them out as-is.
    tryLayout s ((m, i, Group x) : stack) = tryLayout s ((m, i, x) : stack)

    -- Add text to the document. If the document is a single-line we make sure it does not exceed
    -- `maxWidth`. If it does then we return `Nothing`. If the document is multi-line and we are in
    -- flat mode we return `Nothing` since new lines are not allowed in flat mode. If the document
    -- is multi-line and we are in break mode then return `Just` and continue the layout since our
    -- document fits!
    --
    -- Remember that we use UTF-16 to measure length because that is the encoding specified for
    -- character widths by the Language Server Protocol (LSP) specification.
    tryLayout s ((m, _, Text t0) : stack) =
      (B.fromText t0 <>) <$> loop (layoutCharacter s) t0
      where
        -- Measure the length of our text until we either reach the text’s end or a new line. If we
        -- reach a new line and we are in break mode then use `loopBack` to measure the length of
        -- the last line.
        loop k t1 =
          case T.uncons t1 of
            _ | k > maxWidth -> Nothing -- Optimization: Stop iterating if we’ve surpassed `maxWidth`.
            Nothing -> tryLayout (s { layoutCharacter = k }) stack
            Just ('\n', t2) -> case m of { Flat -> Nothing; Break -> loopBack 0 t2 }
            Just ('\r', t2) -> case m of { Flat -> Nothing; Break -> loopBack 0 t2 }
            Just (c, t2) -> loop (k + utf16Length c) t2

        -- Iterate in reverse through the text until we reach the first new line to measure the
        -- length of the last line.
        loopBack k t1 =
          case T.unsnoc t1 of
            Nothing -> Just (layout (s { layoutCharacter = k }) stack)
            Just (_, '\n') -> Just (layout (s { layoutCharacter = k }) stack)
            Just (_, '\r') -> Just (layout (s { layoutCharacter = k }) stack)
            Just (t2, c) -> loopBack (k + utf16Length c) t2

    -- Add some indentation.
    tryLayout s ((m, i, Indent x) : stack) = tryLayout s ((m, i + 2, x) : stack)

    -- If we are in flat mode and we find a new line then fail `tryLayout` by returning `Nothing`.
    -- We may not have new lines in flat mode. If we are in break mode then add a new line and
    -- return `Just` since our line that we are attempting to layout did not exceed `maxWidth`!
    tryLayout _ ((Flat, _, Line) : _) = Nothing
    tryLayout s stack@((Break, _, Line) : _) = Just (layout s stack)

    -- Add line suffix documents to the suffix stack.
    tryLayout s ((_, _, LineSuffix x) : stack) =
      tryLayout (s { layoutLineSuffix = x : layoutLineSuffix s }) stack
