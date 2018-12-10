module Brite.Source.Token
  ( Token(..)
  , Glyph(..)
  ) where

data Token
  = GlyphToken Glyph
  -- | IdentifierToken Identifier
  -- | NumberToken Number
  -- | UnexpectedToken

data Glyph
  -- = KeywordGlyph Keyword
  -- `{`
  = BraceLeft
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
