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

tryConstant :: Parser (Range, Constant)
tryConstant =
  tryBooleanTrue
    <|> tryBooleanFalse

tryBooleanTrue :: Parser (Range, Constant)
tryBooleanTrue = build <$> tryKeyword True_
  where build r = (r, BooleanConstant True)

tryBooleanFalse :: Parser (Range, Constant)
tryBooleanFalse = build <$> tryKeyword False_
  where build r = (r, BooleanConstant False)

expression :: Parser Expression
expression = build <$> retry (tryExpression <|> unexpected ExpectedExpression)
  where
    build (Left e) = Expression (diagnosticRange e) (ErrorExpression e Nothing)
    build (Right x) = x

tryExpression :: Parser Expression
tryExpression =
  tryVariableExpression
    <|> tryWrappedExpression
    <|> tryConstantExpression

tryConstantExpression :: Parser Expression
tryConstantExpression = build <$> tryConstant
  where build (r, c) = Expression r (ConstantExpression c)

tryVariableExpression :: Parser Expression
tryVariableExpression = build <$> tryIdentifier
  where build (r, n) = Expression r (VariableExpression n)

tryWrappedExpression :: Parser Expression
tryWrappedExpression =
  build
    <$> tryGlyph ParenLeft
    <*> expression
    <*> glyph ParenRight
  where
    build r1 x (Right r2) = Expression (Range (rangeStart r1) (rangeEnd r2)) (WrappedExpression x)
    build r1 x (Left e) =
      let
        range = Range (rangeStart r1) (rangeEnd (expressionRange x))
      in
        Expression range (ErrorExpression e (Just (Expression range (WrappedExpression x))))

pattern :: Parser Pattern
pattern = fmap build . retry $
  tryVariablePattern
    <|> unexpected ExpectedPattern
  where
    build (Left e) = Pattern (diagnosticRange e) (ErrorPattern e Nothing)
    build (Right p) = p

tryVariablePattern :: Parser Pattern
tryVariablePattern = build <$> tryIdentifier
  where build (r, n) = Pattern r (VariablePattern n)
