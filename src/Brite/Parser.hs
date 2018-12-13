module Brite.Parser
  ( statement
  ) where

import Brite.AST
import Brite.Parser.Framework
import Brite.Source

statement :: Parser Statement
statement =
  (ExpressionStatement <$> expression)
    <|> bindingStatement

bindingStatement :: Parser Statement
bindingStatement =
  build
    <$> keyword Let
    <*> pattern
    <*> glyph Equals
    <*> expression
  where
    build (Right _) p (Right _) x = BindingStatement p x
    build (Left e) p _ x = ErrorStatement e (Just (BindingStatement p x))
    build _ p (Left e) x = ErrorStatement e (Just (BindingStatement p x))

expression :: Parser Expression
expression = variableExpression

variableExpression :: Parser Expression
variableExpression = build <$> identifier
  where
    build (Right (r, n)) = VariableExpression r n
    build (Left e) = ErrorExpression e Nothing

pattern :: Parser Pattern
pattern = variablePattern

variablePattern :: Parser Pattern
variablePattern = build <$> identifier
  where
    build (Right (r, n)) = VariablePattern r n
    build (Left e) = ErrorPattern e Nothing
