-- Responsible for turning a Brite source text into a format which may be parsed. This includes
-- tokenizing the document and determining positions.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Token
  ( Token(..)
  , TokenKind(..)
  , unexpectedToken
  , EndToken(..)
  , endTokenRange
  , Number(..)
  , BinaryInteger(..)
  , Trivia(..)
  , Newline(..)
  , Comment(..)
  , TriviaSide(..)
  , isTriviaWhitespace
  , tokenSource
  , endTokenSource
  , tokensTrimmedSource
  , removeTrailingSpaces
  , debugTokens
  ) where

import Brite.Diagnostic
import Brite.Syntax.Glyph
import Brite.Syntax.Identifier
import Brite.Syntax.Range
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder

-- A token is a more semantic unit for describing Brite source code documents than a character.
-- Through the tokenization of a document we add meaning by parsing low-level code elements like
-- identifiers, numbers, strings, comments, and glyphs.
--
-- * The leading trivia of a token is all of the trivia which comes before the token which has not
--   already been parsed. Comments, spaces, and newlines.
--
-- * The trailing trivia of a token is all of the trivia after a token _up until the first new
--   line_. If a token is trailed by a line comment then that is part of the trailing trivia along
--   with the newline which immediately follows but nothing else!
data Token = Token
  { tokenRange :: Range
  , tokenKind :: TokenKind
  , tokenLeadingTrivia :: [Trivia]
  , tokenTrailingTrivia :: [Trivia]
  } deriving (Show)

-- The kind of a token.
data TokenKind
  = Glyph Glyph
  | IdentifierToken Identifier
  | Number Number
  | UnexpectedChar Char
  deriving (Show)

-- The parser ran into a token it did not recognize.
unexpectedToken :: DiagnosticMonad m => Token -> ExpectedSyntax -> m Diagnostic
unexpectedToken token expected = unexpectedSyntax (tokenRange token) unexpected expected
  where
    unexpected = case tokenKind token of
      Glyph glyph -> ActualGlyph glyph
      IdentifierToken _ -> ActualIdentifier
      UnexpectedChar c -> ActualChar c

-- The last token in a document. An end token has the position at which the document ended and all
-- the trivia between the last token and the ending.
data EndToken = EndToken
  { endTokenPosition :: Position
  , endTokenTrivia :: [Trivia]
  }

-- Gets the range covered by the end token. Starts and ends at the end token position.
endTokenRange :: EndToken -> Range
endTokenRange (EndToken { endTokenPosition = p }) = Range p p

-- A number token. A number in Brite source code could be written in a few different ways:
--
-- * Integer: `42`
-- * Binary integer: `0b1101`
-- * Hexadecimal integer: `0xFFF`
-- * Floating point: `3.1415`, `1e2`
--
-- For floating point numbers we use the same syntax as the [JSON specification][1] with the one
-- modification that we permit leading zeroes.
--
-- We never parse a negative sign as part of our number syntax. Instead we use a negative operator
-- in our language syntax.
--
-- [1]: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
data Number
  -- `0b1101`
  = BinaryNumber BinaryInteger
  -- If we failed to parse a number then we add an `ErrorNumber` token which contains the diagnostic
  -- that failed and the raw text we attempted to parse as a number.
  | ErrorNumber {- TODO: Diagnostic -} Text
  deriving (Show)

-- An integer written in binary form.
data BinaryInteger = BinaryInteger
  -- Was the “b” after `0` uppercase or lowercase?
  { binaryIntegerLowercaseB :: Bool
  -- Non-empty binary integer source code written in zeroes and ones.
  , binaryIntegerRaw :: Text
  -- The integer value of our binary number.
  , binaryIntegerValue :: Integer
  } deriving (Show)

-- Pieces of Brite syntax which (usually) don’t affect program behavior. Like comments or spaces.
data Trivia
  -- Contiguous space characters (` `).
  = Spaces Int
  -- Contiguous tab characters (`\t`). The Brite formatter prefers spaces to tabs, but since tabs
  -- are common enough we’ll add a special case for them in `Trivia`.
  | Tabs Int
  -- Contiguous newlines. The Brite formatter prefers line feeds (`\n`) to other forms
  -- of newlines.
  | Newlines Newline Int
  -- Some developer comment about the source code.
  | Comment Comment
  -- Other whitespace characters which we optimize such as obscure Unicode whitespace characters
  -- like U+00A0.
  | OtherWhitespace Char
  deriving (Show)

-- `\n`, `\r`, and `\r\n`
data Newline = LF | CR | CRLF deriving (Show)

data Comment
  -- `// ...` does not include the newline that ends the comment. Does include the `//` characters.
  = LineComment Text
  -- `/* ... */` does include the `/*` and `*/` characters.
  --
  -- If the boolean is true then we reached the end of the file before finding `*/`.
  --
  -- NOTE: Maybe we should remove support for block comments? They complicate printing and parsing
  -- and are not used for documentation. At least warn when we see a block comment?
  | BlockComment Text Bool
  deriving (Show)

-- The side on which trivia is attached to a token. Either the leading or trailing side.
data TriviaSide = Leading | Trailing

-- Is this trivia whitespace?
isTriviaWhitespace :: Trivia -> Bool
isTriviaWhitespace (Spaces _) = True
isTriviaWhitespace (Tabs _) = True
isTriviaWhitespace (Newlines _ _) = True
isTriviaWhitespace (Comment _) = False
isTriviaWhitespace (OtherWhitespace _) = True

-- Gets the source code that a token was parsed from.
tokenSource :: Token -> Text.Builder
tokenSource token =
  mconcat (map triviaSource (tokenLeadingTrivia token))
    <> Text.Builder.fromText (tokenKindSource (tokenKind token))
    <> mconcat (map triviaSource (tokenTrailingTrivia token))

-- Gets the source code that a token kind was parsed from.
tokenKindSource :: TokenKind -> Text
tokenKindSource (Glyph g) = glyphText g
tokenKindSource (IdentifierToken i) = identifierText i
tokenKindSource (UnexpectedChar c) = Text.singleton c

-- Gets the source code that an end token was parsed from.
endTokenSource :: EndToken -> Text.Builder
endTokenSource endToken = mconcat (map triviaSource (endTokenTrivia endToken))

-- Gets the source code that a trivia was parsed from.
triviaSource :: Trivia -> Text.Builder
triviaSource (Spaces n) = Text.Builder.fromText (Text.replicate n " ")
triviaSource (Tabs n) = Text.Builder.fromText (Text.replicate n "\t")
triviaSource (Newlines LF n) = Text.Builder.fromText (Text.replicate n "\n")
triviaSource (Newlines CR n) = Text.Builder.fromText (Text.replicate n "\r")
triviaSource (Newlines CRLF n) = Text.Builder.fromText (Text.replicate n "\r\n")
triviaSource (Comment (LineComment comment)) = Text.Builder.fromText "//" <> Text.Builder.fromText comment
triviaSource (Comment (BlockComment comment True)) = Text.Builder.fromText "/*" <> Text.Builder.fromText comment <> Text.Builder.fromText "*/"
triviaSource (Comment (BlockComment comment False)) = Text.Builder.fromText "/*" <> Text.Builder.fromText comment
triviaSource (OtherWhitespace c) = Text.Builder.singleton c

-- Builds source code from from the provided tokens removing whitespace at the beginning of the
-- code, ending of the code, and immediately before a new line (trailing whitespace).
tokensTrimmedSource :: [Token] -> Text.Builder
tokensTrimmedSource = sourceStart
  where
    trimStart t =
      let trivia = dropWhile isTriviaWhitespace (tokenLeadingTrivia t) in
        t { tokenLeadingTrivia = trivia }

    trimEnd t =
      let trivia = reverse (dropWhile isTriviaWhitespace (reverse (tokenTrailingTrivia t))) in
        t { tokenTrailingTrivia = trivia }

    sourceStart [] = source []
    sourceStart (t : ts) = source (trimStart t : ts)

    source [] = mempty
    source [t] = tokenTrimmedSource (trimEnd t)
    source (t : ts) = tokenTrimmedSource t <> source ts

-- Gets the source code that a token was parsed from. Trimming all whitespace that comes
-- immediately before a new line.
tokenTrimmedSource :: Token -> Text.Builder
tokenTrimmedSource token =
  triviaTrimmedSource (tokenLeadingTrivia token)
    <> Text.Builder.fromText (tokenKindTrimmedSource (tokenKind token))
    <> triviaTrimmedSource (tokenTrailingTrivia token)

-- Gets the source code that a token kind was parsed from. Trimming all whitespace that comes
-- immediately before a new line.
--
-- NOTE: This function is basically the same as `tokenKindSource`. However, in the future when we
-- add string literals then we will need to trim trailing whitespace inside string literals.
tokenKindTrimmedSource :: TokenKind -> Text
tokenKindTrimmedSource (t@(Glyph _)) = tokenKindSource t
tokenKindTrimmedSource (t@(IdentifierToken _)) = tokenKindSource t
tokenKindTrimmedSource (t@(UnexpectedChar _)) = tokenKindSource t

-- Gets the source code that a list of trivia was parsed from. Trimming all whitespace that comes
-- immediately before a new line.
triviaTrimmedSource :: [Trivia] -> Text.Builder
triviaTrimmedSource = loop mempty mempty
  where
    loop source space [] = source <> space

    -- Add whitespace to the second accumulator argument.
    loop source space (t@(Spaces _) : ts) = loop source (space <> triviaSource t) ts
    loop source space (t@(Tabs _) : ts) = loop source (space <> triviaSource t) ts
    loop source space (t@(OtherWhitespace _) : ts) = loop source (space <> triviaSource t) ts

    -- Don’t add the space trivia immediately preceding a new line.
    loop source _ (t@(Newlines _ _) : ts) = loop (source <> triviaSource t) mempty ts

    -- Trim trailing whitespace from line comments.
    loop source space (Comment (LineComment comment) : ts) =
      let
        newSource =
          source <> space <> Text.Builder.fromText "//"
            <> Text.Builder.fromText (Text.dropWhileEnd isSpace comment)
      in
        loop newSource mempty ts

    -- Trim trailing whitespace from block comments.
    --
    -- We also automatically fix missing block comment endings. Even though this isn’t explicitly
    -- a part of what `triviaTrimmedSource` is described as doing.
    loop source space (Comment (BlockComment comment _) : ts) =
      let
        newSource =
          source <> space <> Text.Builder.fromText "/*"
            <> removeTrailingSpaces comment <> Text.Builder.fromText "*/"
      in
        loop newSource mempty ts

-- Removes trailing spaces which come before a new line. Trailing spaces at the end of a string are
-- left in place.
removeTrailingSpaces :: Text -> Text.Builder
removeTrailingSpaces t =
  let (t1, t2) = Text.span (\c -> c /= '\n' && c /= '\r') t in
    if Text.null t2 then
      Text.Builder.fromText t
    else
      Text.Builder.fromText (Text.dropWhileEnd isSpace t1)
        <> Text.Builder.singleton (Text.head t2)
        <> removeTrailingSpaces (Text.tail t2)

-- Debug a stream of tokens.
debugTokens :: [Token] -> EndToken -> Text.Builder
debugTokens tokens endToken = mconcat (map debugToken tokens) <> debugEndToken endToken

debugToken :: Token -> Text.Builder
debugToken (Token r k lt tt) =
  mconcat (map (debugTrivia Leading) lt)
    <> Text.Builder.fromLazyText (Text.Lazy.justifyLeft 10 ' ' (Text.Builder.toLazyText (debugRange r)))
    <> Text.Builder.fromText "| "
    <> Text.Builder.fromText content
    <> Text.Builder.singleton '\n'
    <> mconcat (map (debugTrivia Trailing) tt)
  where
    content = case k of
      Glyph glyph -> Text.snoc (Text.append "Glyph `" (glyphText glyph)) '`'
      IdentifierToken identifier -> Text.snoc (Text.append "Identifier `" (identifierText identifier)) '`'
      UnexpectedChar c -> Text.snoc (Text.snoc "Unexpected `" c) '`'

debugEndToken :: EndToken -> Text.Builder
debugEndToken (EndToken p lt) =
  mconcat (map (debugTrivia Leading) lt)
    <> Text.Builder.fromLazyText (Text.Lazy.justifyLeft 10 ' ' (Text.Builder.toLazyText (debugPosition p)))
    <> Text.Builder.fromText "| End\n"

debugTrivia :: TriviaSide -> Trivia -> Text.Builder
debugTrivia side trivia =
  (case side of { Leading -> "+"; Trailing -> "-" })
    <> Text.Builder.fromText (Text.replicate 9 " ")
    <> Text.Builder.fromText "| "
    <> content
    <> Text.Builder.singleton '\n'
  where
    content =
      case trivia of
        Spaces n -> Text.Builder.fromText "Spaces " <> Text.Builder.decimal n
        Tabs n -> Text.Builder.fromText "Tabs " <> Text.Builder.decimal n
        Newlines LF n -> Text.Builder.fromText "Newlines LF " <> Text.Builder.decimal n
        Newlines CR n -> Text.Builder.fromText "Newlines CR " <> Text.Builder.decimal n
        Newlines CRLF n -> Text.Builder.fromText "Newlines CRLF " <> Text.Builder.decimal n
        Comment (LineComment _) -> Text.Builder.fromText "LineComment"
        Comment (BlockComment _ True) -> Text.Builder.fromText "BlockComment"
        Comment (BlockComment _ False) -> Text.Builder.fromText "BlockComment (unterminated)"
        OtherWhitespace c -> Text.Builder.fromText "OtherWhitespace `" <> Text.Builder.singleton c <> Text.Builder.singleton '`'
