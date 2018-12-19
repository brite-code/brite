-- Responsible for turning a Brite source text into a format which may be parsed. This includes
-- tokenizing the document and determining positions.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Source
  ( Position(..)
  , initialPosition
  , Range(..)
  , Identifier
  , identifierText
  , Keyword(..)
  , keywordText
  , Token(..)
  , TokenKind(..)
  , Glyph(..)
  , glyphText
  , TokenList(..)
  , tokenize
  , debugPosition
  , debugRange
  , debugTokens
  ) where

import Data.Bits ((.&.))
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Text.ICU.Char (property, Bool_(XidStart, XidContinue))

-- A position between two characters in a Brite source code document. The encoding of a position is
-- based on the [Language Server Protocol][1].
--
-- * Positions are expressed with a zero-based line and character offset.
-- * A new line is started at one of the sequences `\n`, `\r\n`, and `\r`.
-- * Character offsets are determined by the number of code units in the characters UTF-16 encoding.
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
data Position = Position { positionLine :: !Int, positionCharacter :: !Int }
  deriving (Eq)

-- The position at the start of a document.
initialPosition :: Position
initialPosition = Position 0 0

-- Get the number of UTF-16 code units in one Unicode code point.
--
-- [Description of UTF-16 encoding.](https://en.wikipedia.org/wiki/UTF-16)
utf16Length :: Char -> Int
utf16Length c = if n .&. 0xFFFF == n then 1 else 2
  where n = fromEnum c

-- A range in a Brite source code document represents the space between two characters. The end
-- position is exclusive. A `Range` is comparable to a selection in an editor while a `Position` is
-- comparable to a cursor in an editor.
--
-- Ranges are also based on the [Language Server Protocol][1].
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
data Range = Range { rangeStart :: Position, rangeEnd :: Position }

-- A name written in a Brite program. Brite identifiers follow the [Unicode Identifier
-- Specification][1] including the optional underscore (`_`) character.
--
-- Some strings which are valid identifier syntax are reserved as keywords to disambiguate parsing.
-- We try to reserve the minimum number of keywords possible.
--
-- We could only have keywords in certain positions. For instance, only reserve the keyword `fun`
-- when in an expression context. However, this introduces a potentially confusing rule. It also
-- means, in this example, code transformations could not easily make expression identifiers out of
-- pattern identifiers.
--
-- [1]: http://www.unicode.org/reports/tr31
newtype Identifier = Identifier T.Text

-- Get the text representation of an identifier.
identifierText :: Identifier -> T.Text
identifierText (Identifier t) = t

-- Is this character the start of an identifier?
isIdentifierStart :: Char -> Bool
isIdentifierStart c =
  isAsciiLower c || isAsciiUpper c || c == '_'
    || (c > '\x7f' && property XidStart c)

-- Does this character continue an identifier?
isIdentifierContinue :: Char -> Bool
isIdentifierContinue c =
  isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'
    || (c > '\x7f' && property XidContinue c)

-- Some word that is valid identifier syntax but we reserve for the purpose of parsing.
data Keyword
  = Hole -- `_`
  | True_
  | False_
  | Let
  | If
  | Else
  | Do
  deriving (Eq)

-- Tries to convert a text value into a keyword. Returns `Just` if the text value is a keyword.
-- Returns `Nothing` if the text value is not a keyword.
keyword :: T.Text -> Maybe Keyword
keyword t =
  case t of
    "_" -> Just Hole
    "true" -> Just True_
    "false" -> Just False_
    "let" -> Just Let
    "if" -> Just If
    "else" -> Just Else
    "do" -> Just Do
    _ -> Nothing

-- Gets the raw text for a keyword.
keywordText :: Keyword -> T.Text
keywordText Hole = "_"
keywordText True_ = "true"
keywordText False_ = "false"
keywordText Let = "let"
keywordText If = "if"
keywordText Else = "else"
keywordText Do = "do"

-- A token is a more semantic unit for describing Brite source code documents than a character.
-- Through the tokenization of a document we add meaning by parsing low-level code elements like
-- identifiers, numbers, strings, comments, and glyphs.
data Token = Token
  { tokenRange :: Range
  , tokenKind :: TokenKind
  , tokenLeadingTrivia :: ()
  , tokenTrailingTrivia :: ()
  }

-- The kind of a token.
data TokenKind
  = Glyph Glyph
  | IdentifierToken Identifier
  | UnexpectedChar Char

-- A glyph represents some constant sequence of characters that is used in Brite syntax.
data Glyph
  = Keyword Keyword
  -- `{`
  | BraceLeft
  -- `}`
  | BraceRight
  -- `,`
  | Comma
  -- `.`
  | Dot
  -- `=`
  | Equals
  -- `(`
  | ParenLeft
  -- `)`
  | ParenRight
  -- `;`
  | Semicolon
  -- `/`
  | Slash
  deriving (Eq)

-- Gets the text representation of a glyph.
glyphText :: Glyph -> T.Text
glyphText (Keyword k) = keywordText k
glyphText BraceLeft = "{"
glyphText BraceRight = "}"
glyphText Comma = ","
glyphText Dot = "."
glyphText Equals = "="
glyphText ParenLeft = "("
glyphText ParenRight = ")"
glyphText Semicolon = ";"
glyphText Slash = "/"

-- A lazy list of tokens. Customized to include some extra token and end data.
data TokenList
  -- The next token in the list along with the token’s range.
  = NextToken Token TokenList
  -- The end of the token list. Includes the position of the source text’s end.
  | EndToken Position

-- Turn Unicode text into a lazy list of tokens and their associated ranges.
tokenize :: Position -> T.Text -> TokenList
tokenize position0 text0 =
  if T.null text0 then EndToken position0 else
  case T.head text0 of
    -- Ignore whitespace.
    ' ' -> tokenize position1 text1

    -- Parse some glyphs.
    '{' -> NextToken (Token range1 (Glyph BraceLeft) () ()) (tokenize position1 text1)
    '}' -> NextToken (Token range1 (Glyph BraceRight) () ()) (tokenize position1 text1)
    ',' -> NextToken (Token range1 (Glyph Comma) () ()) (tokenize position1 text1)
    '.' -> NextToken (Token range1 (Glyph Dot) () ()) (tokenize position1 text1)
    '=' -> NextToken (Token range1 (Glyph Equals) () ()) (tokenize position1 text1)
    '(' -> NextToken (Token range1 (Glyph ParenLeft) () ()) (tokenize position1 text1)
    ')' -> NextToken (Token range1 (Glyph ParenRight) () ()) (tokenize position1 text1)
    ';' -> NextToken (Token range1 (Glyph Semicolon) () ()) (tokenize position1 text1)

    -- Ignore newlines (`\n`).
    '\n' ->
      let
        position2 = position0 { positionCharacter = 0, positionLine = positionLine position0 + 1 }
      in
        tokenize position2 text1

    -- Ignore newlines (`\r` and `\r\n`). If we have the sequence `\r\n` we only want to count one
    -- newline and not two.
    '\r' ->
      let
        position2 = position0 { positionCharacter = 0, positionLine = positionLine position0 + 1 }
        text2 = if T.null text1 || T.head text1 /= '\n' then text1 else T.tail text1
      in
        tokenize position2 text2

    -- Parse an identifier.
    --
    -- The identifier we create is represented as a `T.Text`. This text value is linked to the
    -- source document it was created from. This means we don’t need to allocate new memory for
    -- every identifier but also means our source document may live longer than expected.
    c | isIdentifierStart c ->
      let
        (identifier, text2) = T.span isIdentifierContinue text0
        n = T.foldl (\acc c' -> acc + utf16Length c') 0 identifier
        position2 = position0 { positionCharacter = positionCharacter position0 + n }
        range2 = Range position0 position2
        kind = case keyword identifier of
          Just k -> Glyph (Keyword k)
          Nothing -> IdentifierToken (Identifier identifier)
      in
        NextToken (Token range2 kind () ()) (tokenize position2 text2)

    -- Parse a single line comment. Single line comments ignore all characters until the
    -- next newline.
    '/' | not (T.null text1) && T.head text1 == '/' ->
      let
        (n, text2) = loop 2 (T.tail text1)
        position2 = position0 { positionCharacter = positionCharacter position0 + n }
      in
        tokenize position2 text2
      where
        loop n t =
          case T.uncons t of
            Nothing -> (n, t)
            Just ('\n', _) -> (n, t)
            Just ('\r', _) -> (n, t)
            Just (c, t') -> loop (n + utf16Length c) t'

    -- Parse a multi-line comment. Multi-line comments ignore all characters until the character
    -- sequence `*/`.
    '/' | not (T.null text1) && T.head text1 == '*' ->
      let
        position2 = position0 { positionCharacter = positionCharacter position0 + 2 }
      in
        uncurry tokenize (loop position2 (T.tail text1))
      where
        loop p t =
          case T.uncons t of
            Nothing -> (p, t)
            Just ('*', t') | not (T.null t') && T.head t' == '/' ->
              let p' = p { positionCharacter = positionCharacter p + 2 } in
              (p', T.tail t')
            Just ('\n', t') ->
              let p' = p { positionCharacter = 0, positionLine = positionLine p + 1 } in
              loop p' t'
            Just ('\r', t') ->
              let p' = p { positionCharacter = 0, positionLine = positionLine p + 1 } in
              loop p' (if T.null t' || T.head t' /= '\n' then t' else T.tail t')
            Just (c, t') ->
              let p' = p { positionCharacter = positionCharacter p + utf16Length c } in
              loop p' t'

    -- Parse the slash glyph.
    '/' -> NextToken (Token range1 (Glyph Slash) () ()) (tokenize position1 text1)

    -- Ignore whitespace.
    c | isSpace c -> tokenize position1 text1

    -- Unexpected character.
    c ->
      let
        position2 = position0 { positionCharacter = positionCharacter position0 + utf16Length c }
        range2 = Range position0 position2
      in
        NextToken (Token range2 (UnexpectedChar c) () ()) (tokenize position2 text1)

    where
      position1 = position0 { positionCharacter = positionCharacter position0 + 1 }
      range1 = Range position0 position1
      text1 = T.tail text0

-- Debug a position.
debugPosition :: Position -> B.Builder
debugPosition (Position line character) =
  B.decimal line <> B.singleton ':' <> B.decimal character

-- Debug a range of characters.
debugRange :: Range -> B.Builder
debugRange (Range start end) =
  debugPosition start <> B.singleton '-' <> debugPosition end

-- Builds a text value we can use to debug a token list.
debugTokens :: TokenList -> L.Text
debugTokens tokens = B.toLazyText (debugTokens' tokens)

debugTokens' :: TokenList -> B.Builder

debugTokens' (NextToken (Token r k () ()) ts) =
  B.fromLazyText (L.justifyLeft 10 ' ' (B.toLazyText (debugRange r)))
    <> B.fromText "| "
    <> B.fromText token
    <> B.singleton '\n'
    <> debugTokens' ts
  where
    token = case k of
      Glyph glyph -> T.snoc (T.append "Glyph `" (glyphText glyph)) '`'
      IdentifierToken (Identifier identifier) -> T.snoc (T.append "Identifier `" identifier) '`'
      UnexpectedChar c -> T.snoc (T.snoc "Unexpected `" c) '`'

debugTokens' (EndToken p) =
  B.fromLazyText (L.justifyLeft 10 ' ' (B.toLazyText (debugPosition p)))
    <> B.fromText "| End\n"
