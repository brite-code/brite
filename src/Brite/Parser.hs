module Brite.Parser
  ( bindingStatement
  ) where

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
    build (Right _) p (Right _) x = BindingStatement p x
    build (Left (e, _)) p _ x = ErrorStatement e (Just (BindingStatement p x))
    build _ p (Left (e, _)) x = ErrorStatement e (Just (BindingStatement p x))

expression :: Parser Expression
expression = variableExpression

variableExpression :: Parser Expression
variableExpression = build <$> identifier
  where
    build (Right (r, n)) = VariableExpression r n
    build (Left (e, m)) = ErrorExpression e (fmap (build . Right) m)

pattern :: Parser Pattern
pattern = variablePattern

variablePattern :: Parser Pattern
variablePattern = build <$> identifier
  where
    build (Right (r, n)) = VariablePattern r n
    build (Left (e, m)) = ErrorPattern e (fmap (build . Right) m)
