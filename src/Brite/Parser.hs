module Brite.Parser
  () where

import Brite.AST
import Brite.Parser.Framework
import Brite.Source

bindingStatement :: Parser Statement
bindingStatement =
  build
    <$> keyword Let
    <*> pattern
    <*> glyph Equals
    <*> expression
  where
    build (Success _) p (Success _) x = BindingStatement p x
    build (Recover e _) p _ x = ErrorStatement e (Just (BindingStatement p x))
    build (Failure e) p _ x = ErrorStatement e (Just (BindingStatement p x))
    build (Success _) p (Recover e _) x = ErrorStatement e (Just (BindingStatement p x))
    build (Success _) p (Failure e) x = ErrorStatement e (Just (BindingStatement p x))

expression :: Parser Expression
expression = variableExpression

variableExpression :: Parser Expression
variableExpression = build <$> identifier
  where
    build (Success (r, n)) = VariableExpression r n
    build (Recover e (r, n)) = ErrorExpression e (Just (VariableExpression r n))
    build (Failure e) = ErrorExpression e Nothing

pattern :: Parser Pattern
pattern = variablePattern

variablePattern :: Parser Pattern
variablePattern = build <$> identifier
  where
    build (Success (r, n)) = VariablePattern r n
    build (Recover e (r, n)) = ErrorPattern e (Just (VariablePattern r n))
    build (Failure e) = ErrorPattern e Nothing
