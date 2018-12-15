module Brite.Parser
  ( parse
  ) where

import Brite.AST
import Brite.Parser.Framework
import Brite.Diagnostics
import Brite.Source
import Control.Applicative hiding (optional, many)

-- Parses a Brite module from a stream of tokens.
parse :: TokenList -> DiagnosticWriter Module
parse tokens = fromRight <$> runParser (Module <$> many tryStatement) tokens
  where
    fromRight (Right x) = x
    fromRight (Left _) = error "Unexpected failure."

tryStatement :: Parser Statement
tryStatement =
  tryBindingStatement
    <|> tryExpressionStatement
    <|> unexpected ExpectedStatement

tryExpressionStatement :: Parser Statement
tryExpressionStatement =
  build
    <$> tryExpression
    <*> optional (tryGlyph Semicolon)
  where
    build x Nothing = Statement (expressionRange x) (ExpressionStatement x)
    build x (Just end) =
      Statement (Range (rangeStart (expressionRange x)) (rangeEnd end)) (ExpressionStatement x)

tryBindingStatement :: Parser Statement
tryBindingStatement =
  build
    <$> tryKeyword Let
    <*> pattern
    <*> glyph Equals
    <*> expression
    <*> optional (tryGlyph Semicolon)
  where
    build start p (Right _) x (Just end) =
      Statement (Range (rangeStart start) (rangeEnd end)) (BindingStatement p x)
    build start p (Left e) x (Just end) =
      let range = Range (rangeStart start) (rangeEnd end) in
      Statement range (ErrorStatement e (Just (BindingStatement p x)))
    build start p (Right _) x Nothing =
      Statement (Range (rangeStart start) (rangeEnd (expressionRange x))) (BindingStatement p x)
    build start p (Left e) x Nothing =
      let range = Range (rangeStart start) (rangeEnd (expressionRange x)) in
      Statement range (ErrorStatement e (Just (BindingStatement p x)))

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

-- Ordered by frequency. Parsers that are more likely to match go first.
tryExpression :: Parser Expression
tryExpression =
  tryVariableExpression
    <|> tryWrappedExpression
    <|> tryConstantExpression
    -- <|> tryBlockStatement

tryConstantExpression :: Parser Expression
tryConstantExpression = build <$> tryConstant
  where build (r, c) = Expression r (ConstantExpression c)

tryVariableExpression :: Parser Expression
tryVariableExpression = build <$> tryIdentifier
  where build (r, n) = Expression r (VariableExpression n)

-- tryBlockStatement :: Parser Expression
-- tryBlockStatement =
--   build
--     <$> tryKeyword Do
--     <*> glyph BraceLeft
--     <*> many tryStatement
--     <*> glyph BraceRight
--   where
--     build' range statements = Expression range (BlockExpression (Block statements))
--     build start (Right _) statements (Right end) =
--       build' (Range (rangeStart start) (rangeEnd end)) statements
--     build start (Left error) statements (Right end) =
--       let range = Range (rangeStart start) (rangeEnd end) in
--       ErrorExpression range (Just (build' range statements))
--     build start (Right _) statements (Left error) =

tryWrappedExpression :: Parser Expression
tryWrappedExpression =
  build
    <$> tryGlyph ParenLeft
    <*> expression
    <*> glyph ParenRight
  where
    build start x (Right end) =
      Expression (Range (rangeStart start) (rangeEnd end)) (WrappedExpression x)
    build start x (Left e) =
      let range = Range (rangeStart start) (rangeEnd (expressionRange x)) in
      Expression range (ErrorExpression e (Just (WrappedExpression x)))

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
