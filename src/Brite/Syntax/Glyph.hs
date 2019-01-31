{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Glyph
  ( Glyph(..)
  , glyphText
  ) where

import Brite.Syntax.Identifier (Keyword, keywordText)
import Data.Text (Text)

-- A glyph represents some constant sequence of characters that is used in Brite syntax.
data Glyph
  = Keyword Keyword
  -- `&`
  | Ampersand
  -- `&&`
  | AmpersandDouble
  -- `->`
  | Arrow
  -- `*`
  | Asterisk
  -- `!`
  | Bang
  -- `|`
  | Bar
  -- `||`
  | BarDouble
  -- `{`
  | BraceLeft
  -- `}`
  | BraceRight
  -- `[`
  | BracketLeft
  -- `]`
  | BracketRight
  -- `^`
  | Caret
  -- `:`
  | Colon
  -- `,`
  | Comma
  -- `.`
  | Dot
  -- `=`
  | Equals_
  -- `==`
  | EqualsDouble
  -- `!=`
  | EqualsNot
  -- `>`
  | GreaterThan_
  -- `>=`
  | GreaterThanOrEqual_
  -- `<`
  | LessThan_
  -- `<=`
  | LessThanOrEqual_
  -- `-`
  | Minus
  -- `(`
  | ParenLeft
  -- `)`
  | ParenRight
  -- `%`
  | Percent
  -- `+`
  | Plus
  -- `;`
  | Semicolon
  -- `/`
  | Slash
  deriving (Eq, Show)

-- Gets the text representation of a glyph.
glyphText :: Glyph -> Text
glyphText (Keyword k) = keywordText k
glyphText Ampersand = "&"
glyphText AmpersandDouble = "&&"
glyphText Arrow = "->"
glyphText Asterisk = "*"
glyphText Bang = "!"
glyphText Bar = "|"
glyphText BarDouble = "||"
glyphText BraceLeft = "{"
glyphText BraceRight = "}"
glyphText BracketLeft = "["
glyphText BracketRight = "]"
glyphText Caret = "^"
glyphText Colon = ":"
glyphText Comma = ","
glyphText Dot = "."
glyphText Equals_ = "="
glyphText EqualsDouble = "=="
glyphText EqualsNot = "!="
glyphText GreaterThan_ = ">"
glyphText GreaterThanOrEqual_ = ">="
glyphText LessThan_ = "<"
glyphText LessThanOrEqual_ = "<="
glyphText Minus = "-"
glyphText ParenLeft = "("
glyphText ParenRight = ")"
glyphText Percent = "%"
glyphText Plus = "+"
glyphText Semicolon = ";"
glyphText Slash = "/"
