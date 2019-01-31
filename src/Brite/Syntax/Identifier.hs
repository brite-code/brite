{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Identifier
  ( Identifier
  , unsafeIdentifier
  , identifierText
  , isIdentifierStart
  , isIdentifierContinue
  , Keyword(..)
  , textKeyword
  , keywordText
  ) where

import Data.Char
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ICU.Char (property, Bool_(XidStart, XidContinue))

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
newtype Identifier = Identifier Text
  deriving (Eq, Ord, Hashable)

instance Show Identifier where
  show (Identifier t) = Text.unpack t

-- Unsafely creates an identifier without checking if it is in the correct format.
unsafeIdentifier :: Text -> Identifier
unsafeIdentifier = Identifier

-- Get the text representation of an identifier.
identifierText :: Identifier -> Text
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
  | True'
  | False'
  | Void
  | Let
  | If
  | Else
  | Do
  | Fun
  | Return
  | Loop
  | Break
  deriving (Eq, Show)

-- Tries to convert a text value into a keyword. Returns `Just` if the text value is a keyword.
-- Returns `Nothing` if the text value is not a keyword.
textKeyword :: Text -> Maybe Keyword
textKeyword t =
  case t of
    "_" -> Just Hole
    "true" -> Just True'
    "false" -> Just False'
    "void" -> Just Void
    "let" -> Just Let
    "if" -> Just If
    "else" -> Just Else
    "do" -> Just Do
    "fun" -> Just Fun
    "return" -> Just Return
    "loop" -> Just Loop
    "break" -> Just Break
    _ -> Nothing

-- Gets the raw text for a keyword.
keywordText :: Keyword -> Text
keywordText Hole = "_"
keywordText True' = "true"
keywordText False' = "false"
keywordText Void = "void"
keywordText Let = "let"
keywordText If = "if"
keywordText Else = "else"
keywordText Do = "do"
keywordText Fun = "fun"
keywordText Return = "return"
keywordText Loop = "loop"
keywordText Break = "break"
