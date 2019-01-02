{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.PrinterFramework2
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

-- Prints the document at the specified maximum width.
printDocument :: Int -> Document -> B.Builder
printDocument maxWidth rootDocument = layout 0 [(Break, 0, rootDocument)] []
  where
    -- Responsible for laying out a document. Takes as parameters:
    --
    -- * The current character position measured in UTF-16 encoded code units as specified
    --   by the LSP.
    -- * An execution stack of documents. We build our document by processing every item on the
    --   stack. Each stack item contains the mode in which we print the document, the current level
    --   of indentation, and the document to be printed.
    -- * Line suffix documents. This list is reverse ordered.
    layout :: Int -> [(Mode, Int, Document)] -> [Document] -> B.Builder

    -- We are done processing our stack!
    layout _ [] [] = mempty

    -- If there are some line suffix documents remaining add them to our stack and process them.
    layout k [] suffix = layout k (map ((,,) Break 0) (reverse suffix)) []

    -- Skip empty documents in the stack.
    layout k ((_, _, Empty) : stack) suffix = layout k stack suffix

    -- Add both documents of a concatenation to the stack.
    layout k ((m, i, Concat x y) : stack) suffix = layout k ((m, i, x) : (m, i, y) : stack) suffix

    -- Make a choice depending on the mode. `Flat` chooses the left and `Break` chooses the right.
    layout k ((Flat, i, Choice x _) : stack) suffix = layout k ((Flat, i, x) : stack) suffix
    layout k ((Break, i, Choice _ y) : stack) suffix = layout k ((Break, i, y) : stack) suffix

    -- Flat groups continue to flatten their document. It is break groups which are interesting.
    -- Break mode groups attempt to flatten their document.
    layout k ((Flat, i, Group x) : stack) suffix = layout k ((Flat, i, x) : stack) suffix

    -- A break mode group attempts to flatten its document using the `tryLayout` function to see if
    -- it will fit on one line. If it does not then we layout the group using our break mode.
    layout k ((Break, i, Group x) : stack) suffix =
      case tryLayout k ((Flat, i, x) : stack) suffix of
        Just t -> t
        Nothing -> layout k ((Break, i, x) : stack) suffix

    -- Add text to the document. If the text is a single line then we add the UTF-16 encoded code
    -- point length to the position to get our new position. If the text is multiple lines we set
    -- the new position to the UTF-16 length of the last line.
    --
    -- Remember that we use UTF-16 to measure length because that is the encoding specified for
    -- character widths by the Language Server Protocol (LSP) specification.
    layout k0 ((_, _, Text t0) : stack) suffix = B.fromText t0 <> loop k0 t0
      where
        -- Measure the length of our text until we either reach the text’s end or a new line. If we
        -- reach a new line then use `loopBack` to measure the length of the last line.
        loop k t1 =
          case T.uncons t1 of
            Nothing -> layout k stack suffix
            Just ('\n', t2) -> loopBack 0 t2
            Just ('\r', t2) -> loopBack 0 t2
            Just (c, t2) -> loop (k + utf16Length c) t2

        -- Iterate in reverse through the text until we reach the first new line to measure the
        -- length of the last line.
        loopBack k t1 =
          case T.unsnoc t1 of
            Nothing -> layout k stack suffix
            Just (_, '\n') -> layout k stack suffix
            Just (_, '\r') -> layout k stack suffix
            Just (t2, c) -> loopBack (k + utf16Length c) t2

    -- Add some indentation.
    layout k ((m, i, Indent x) : stack) suffix = layout k ((m, i + 2, x) : stack) suffix

    -- Add a new line at the current level of indentation. If there are some line suffix documents
    -- then add them to the execution stack and process them.
    layout _ ((_, i, Line) : stack) [] =
      B.singleton '\n' <> B.fromText (T.replicate i " ") <> layout i stack []
    layout k stack@((m, i, Line) : _) suffix =
      layout k (foldl (flip (:)) stack (map ((,,) m i) suffix)) []

    -- Add line suffix documents to the suffix stack.
    layout k ((_, _, LineSuffix x) : stack) suffix = layout k stack (x : suffix)

    -- Attempts to layout a flat mode document.
    --
    -- * Returns `Nothing` if `k` exceeds `maxWidth`.
    -- * Returns `Nothing` if we find a new line in a flat mode document.
    -- * Returns `Just` if the first line fits in `maxWidth`.
    tryLayout :: Int -> [(Mode, Int, Document)] -> [Document] -> Maybe B.Builder

    -- If our line has exceeded `maxWidth` return `Nothing`.
    tryLayout k _ _ | k > maxWidth = Nothing

    -- If we have reached the end of the stack we’re ok!
    tryLayout k [] suffix = Just (layout k [] suffix)

    -- Skip empty documents in the stack.
    tryLayout k ((_, _, Empty) : stack) suffix = tryLayout k stack suffix

    -- Add both documents of a concatenation to the stack.
    tryLayout k ((m, i, Concat x y) : stack) suffix =
      tryLayout k ((m, i, x) : (m, i, y) : stack) suffix

    -- Make a choice depending on the mode. `Flat` chooses the left and `Break` chooses the right.
    tryLayout k ((Flat, i, Choice x _) : stack) suffix = tryLayout k ((Flat, i, x) : stack) suffix
    tryLayout k ((Break, i, Choice _ y) : stack) suffix = tryLayout k ((Break, i, y) : stack) suffix

    -- Attempt to layout a group in the current specified mode. We don’t attempt to flatten break
    -- mode documents. Instead we lay them out as-is.
    tryLayout k ((m, i, Group x) : stack) suffix = tryLayout k ((m, i, x) : stack) suffix

    -- Add text to the document. If the document is a single-line we make sure it does not exceed
    -- `maxWidth`. If it does then we return `Nothing`. If the document is multi-line and we are in
    -- flat mode we return `Nothing` since new lines are not allowed in flat mode. If the document
    -- is multi-line and we are in break mode then return `Just` and continue the layout since our
    -- document fits!
    --
    -- Remember that we use UTF-16 to measure length because that is the encoding specified for
    -- character widths by the Language Server Protocol (LSP) specification.
    tryLayout k0 ((m, _, Text t0) : stack) suffix = (B.fromText t0 <>) <$> loop k0 t0
      where
        -- Measure the length of our text until we either reach the text’s end or a new line. If we
        -- reach a new line and we are in break mode then use `loopBack` to measure the length of
        -- the last line.
        loop k t1 =
          case T.uncons t1 of
            _ | k > maxWidth -> Nothing -- Optimization: Stop iterating if we’ve surpassed `maxWidth`.
            Nothing -> tryLayout k stack suffix
            Just ('\n', t2) -> case m of { Flat -> Nothing; Break -> loopBack 0 t2 }
            Just ('\r', t2) -> case m of { Flat -> Nothing; Break -> loopBack 0 t2 }
            Just (c, t2) -> loop (k + utf16Length c) t2

        -- Iterate in reverse through the text until we reach the first new line to measure the
        -- length of the last line.
        loopBack k t1 =
          case T.unsnoc t1 of
            Nothing -> Just (layout k stack suffix)
            Just (_, '\n') -> Just (layout k stack suffix)
            Just (_, '\r') -> Just (layout k stack suffix)
            Just (t2, c) -> loopBack (k + utf16Length c) t2

    -- Add some indentation.
    tryLayout k ((m, i, Indent x) : stack) suffix = tryLayout k ((m, i + 2, x) : stack) suffix

    -- If we are in flat mode and we find a new line then fail `tryLayout` by returning `Nothing`.
    -- We may not have new lines in flat mode. If we are in break mode then add a new line and
    -- return `Just` since our line that we are attempting to layout did not exceed `maxWidth`!
    tryLayout _ ((Flat, _, Line) : _) _ = Nothing
    tryLayout k stack@((Break, _, Line) : _) suffix = Just (layout k stack suffix)

    -- Add line suffix documents to the suffix stack.
    tryLayout k ((_, _, LineSuffix x) : stack) suffix = tryLayout k stack (x : suffix)

