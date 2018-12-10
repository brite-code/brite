{-# LANGUAGE ExistentialQuantification #-}

module Brite.Source.Parser
  () where

import Brite.Source.AST
import Brite.Source.Token
import Control.Applicative
import Control.Monad
import Data.Functor

data Parser a
  = TerminalParser (Token -> Maybe a)
  | EmptyParser a
  | SequenceParser (SP a)

data SP a = forall b. SP (Parser (b -> a)) (Parser b)

instance Functor Parser where
  fmap f (TerminalParser p) = TerminalParser (fmap f . p)
  fmap f (EmptyParser a) = EmptyParser (f a)
  fmap f (SequenceParser (SP p q)) = SequenceParser (SP (fmap (\g b -> f (g b)) p) q)

instance Applicative Parser where
  pure = EmptyParser
  p <*> q = SequenceParser (SP p q)

glyph :: Glyph -> Parser Token
glyph _ = error "unimplemented"

-- bindingStatement :: Parser Statement
-- bindingStatement =
--   BindingStatement
--     <$> glyph Let
--     <*> pattern
--     <*> glyph Equals
--     <*> expression
--     <*> optional (glyph Semicolon)

expression :: Parser Expression
expression = error "unimplemented"
