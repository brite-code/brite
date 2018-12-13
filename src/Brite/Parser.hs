module Brite.Parser
  ( statement
  ) where

import Brite.AST
import Brite.Parser.Framework
import Brite.Diagnostics
import Brite.Source
import Control.Applicative hiding (optional)

statement :: Parser Statement
statement = fmap build . retry $
  tryBindingStatement
    <|> (ExpressionStatement <$> tryExpression <* optional (tryGlyph Semicolon))
    <|> unexpected ExpectedStatement
  where
    build (Left e) = ErrorStatement e Nothing
    build (Right s) = s

tryBindingStatement :: Parser Statement
tryBindingStatement =
  build
    <$> tryKeyword Let
    <*> pattern
    <*> glyph Equals
    <*> expression
    <*> optional (tryGlyph Semicolon)
  where
    -- We have an annotation here so that Haskell tells us if the type changes on any of our
    -- wildcards. For instance, if a type changes to `Either` then we’d want to use
    -- `ErrorStatement`s when we have a `Left`.
    build :: Range -> Pattern -> Either Diagnostic Range -> Expression -> Maybe Range -> Statement
    build _ p (Right _) x _ = BindingStatement p x
    build _ p (Left e) x _ = ErrorStatement e (Just (BindingStatement p x))

expression :: Parser Expression
expression = build <$> retry (tryExpression <|> unexpected ExpectedExpression)
  where
    build (Left e) = ErrorExpression e Nothing
    build (Right x) = x

tryExpression :: Parser Expression
tryExpression =
  tryVariableExpression

tryVariableExpression :: Parser Expression
tryVariableExpression = uncurry VariableExpression <$> tryIdentifier

pattern :: Parser Pattern
pattern = fmap build . retry $
  tryVariablePattern
    <|> unexpected ExpectedPattern
  where
    build (Left e) = ErrorPattern e Nothing
    build (Right p) = p

tryVariablePattern :: Parser Pattern
tryVariablePattern = uncurry VariablePattern <$> tryIdentifier
