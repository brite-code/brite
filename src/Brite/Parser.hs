module Brite.Parser
  ( statement
  ) where

import Brite.AST
import Brite.Parser.Framework4
import Brite.Source
import Control.Applicative hiding (optional)

statement :: Parser Statement
statement = fmap build . retry $
  bindingStatement
    <|> (ExpressionStatement <$> tryExpression)
  where
    build (Left e) = ErrorStatement e Nothing
    build (Right s) = s

bindingStatement :: Parser Statement
bindingStatement =
  build
    <$> keyword Let
    <*> pattern
    <*> retry (glyph Equals)
    <*> expression
  where
    build _ p (Right _) x = BindingStatement p x
    build _ p (Left e) x = ErrorStatement e (Just (BindingStatement p x))

tryExpression :: Parser Expression
tryExpression =
  variableExpression

expression :: Parser Expression
expression = build <$> retry tryExpression
  where
    build (Left e) = ErrorExpression e Nothing
    build (Right x) = x

variableExpression :: Parser Expression
variableExpression = uncurry VariableExpression <$> identifier

pattern :: Parser Pattern
pattern = fmap build . retry $
  variablePattern
  where
    build (Left e) = ErrorPattern e Nothing
    build (Right p) = p

variablePattern :: Parser Pattern
variablePattern = uncurry VariablePattern <$> identifier
