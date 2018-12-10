-- Responsible for turning a Brite source text into a format which may be parsed. This includes
-- tokenizing the document and determining positions.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Source
  ( Position(..)
  , initialPosition
  , Range(..)
  , Identifier
  , identifier
  , Keyword(..)
  , Token(..)
  , Glyph(..)
  , TokenList(..)
  , tokenize
  ) where

import Data.Bits ((.&.))
import Data.Char
import Data.Maybe
import qualified Data.Text as T
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

instance Show Position where
  show (Position line character) = show line ++ ":" ++ show character

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
  deriving (Eq)

instance Show Range where
  show (Range start end) = if start == end then show start else show start ++ "-" ++ show end

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
  deriving (Eq, Show)

-- Tries to create a new identifier. Returns `Just` if the identifier is valid and `Nothing` if
-- the identifier is invalid.
identifier :: T.Text -> Maybe Identifier
identifier t = if ok then Just (Identifier t) else Nothing
  where
    ok =
      not (T.null t)
        && isIdentifierStart (T.head t)
        && T.all isIdentifierContinue (T.tail t)
        && isNothing (keyword t)

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
  deriving (Eq, Show)

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

-- A token is a more semantic unit for describing Brite source code documents than a character.
-- Through the tokenization of a document we add meaning by parsing low-level code elements like
-- identifiers, numbers, strings, comments, and glyphs.
data Token
  = GlyphToken Glyph
  | IdentifierToken Identifier
  | UnexpectedChar Char
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- A lazy list of tokens. Customized to include some extra token and end data.
data TokenList
  -- The next token in the list along with the token’s range.
  = NextToken Range Token TokenList
  -- The end of the token list. Includes the position of the source text’s end.
  | EndToken Position

-- Turn Unicode text into a lazy list of tokens and their associated ranges.
tokenize :: Position -> T.Text -> TokenList
tokenize position0 text0 =
  if T.null text0 then EndToken position0 else
  case T.head text0 of
    '{' -> NextToken range1 (GlyphToken BraceLeft) (tokenize position1 text1)
    '}' -> NextToken range1 (GlyphToken BraceRight) (tokenize position1 text1)
    ',' -> NextToken range1 (GlyphToken Comma) (tokenize position1 text1)
    '.' -> NextToken range1 (GlyphToken Dot) (tokenize position1 text1)
    '=' -> NextToken range1 (GlyphToken Equals) (tokenize position1 text1)
    '(' -> NextToken range1 (GlyphToken ParenLeft) (tokenize position1 text1)
    ')' -> NextToken range1 (GlyphToken ParenRight) (tokenize position1 text1)
    ';' -> NextToken range1 (GlyphToken Semicolon) (tokenize position1 text1)
    '/' -> NextToken range1 (GlyphToken Slash) (tokenize position1 text1)

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

    -- Ignore whitespace (shortcut).
    ' ' -> tokenize position1 text1

    -- Parse an identifier.
    --
    -- The identifier we create is represented as a `T.Text`. This text value is linked to the
    -- source document it was created from. This means we don’t need to allocate new memory for
    -- every identifier but also means our source document may live longer than expected.
    c | isIdentifierStart c ->
      let
        (identifier, text2) = T.span isIdentifierContinue text0
        n = T.foldl (\n c -> n + utf16Length c) 0 identifier
        position2 = position0 { positionCharacter = positionCharacter position0 + n }
        range2 = Range position0 position2
        token = case keyword identifier of
          Just keyword -> GlyphToken (Keyword keyword)
          Nothing -> IdentifierToken (Identifier identifier)
      in
        NextToken range2 token (tokenize position2 text2)

    -- Ignore whitespace.
    c | isSpace c -> tokenize position1 text1

    -- Unexpected character.
    c ->
      let
        position2 = position0 { positionCharacter = positionCharacter position0 + utf16Length c }
        range2 = Range position0 position2
      in
        NextToken range2 (UnexpectedChar c) (tokenize position2 text1)

    where
      position1 = position0 { positionCharacter = positionCharacter position0 + 1 }
      range1 = Range position0 position1
      text1 = T.tail text0
