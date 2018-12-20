module Brite.Parser
  ( parse
  ) where

import Brite.AST
import Brite.Parser.Framework
import Brite.Diagnostics
import Brite.Source

-- Parses a Brite module from a stream of tokens.
parse :: TokenStream -> DiagnosticWriter Module
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

block :: Parser (Either (Diagnostic, Block) Block)
block =
  build
    <$> glyph BraceLeft
    <*> many tryStatement
    <*> glyph BraceRight
  where
    build (Right start) statements (Right end) =
      let range = Range (rangeStart start) (rangeEnd end) in
      Right (Block range statements)

    build (Left err) statements (Right end) =
      let
        start = if null statements then end else statementRange (head statements)
        range = Range (rangeStart start) (rangeEnd end)
      in
        Left (err, Block range statements)

    build (Right start) statements (Left err) =
      let
        end = if null statements then start else statementRange (last statements)
        range = Range (rangeStart start) (rangeEnd end)
      in
        Left (err, Block range statements)

    build (Left err1) statements (Left err2) =
      if null statements then
        let range = Range (rangeStart (diagnosticRange err1)) (rangeEnd (diagnosticRange err2)) in
        Left (err1, Block range [])
      else
        let
          start = statementRange (head statements)
          end = statementRange (last statements)
          range = Range (rangeStart start) (rangeEnd end)
        in
          Left (err1, Block range statements)

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
tryPrimaryExpression :: Parser Expression
tryPrimaryExpression =
  tryVariableExpression
    <|> tryConditionalExpression
    <|> tryWrappedExpression
    <|> tryConstantExpression
    <|> tryBlockExpression

tryExpression :: Parser Expression
tryExpression =
  build <$> tryPrimaryExpression <*> many tryExpressionExtension
  where
    build x [] = x
    build x (ext : exts) = build (buildOne x ext) exts

    buildOne x (PropertyExpressionExtension p) =
      let range = Range (rangeStart (expressionRange x)) (rangeEnd (nameRange p)) in
      Expression range (PropertyExpression x p)

    buildOne x (ErrorExpressionExtension err) =
      Expression (expressionRange x) (ErrorExpression err (Just (expressionNode x)))

tryConstantExpression :: Parser Expression
tryConstantExpression = build <$> tryConstant
  where build (r, c) = Expression r (ConstantExpression c)

tryVariableExpression :: Parser Expression
tryVariableExpression = build <$> tryIdentifier
  where build (r, n) = Expression r (VariableExpression n)

tryConditionalExpression :: Parser Expression
tryConditionalExpression =
  build
    <$> tryKeyword If
    <*> expression
    <*> block
    <*> optional (tryKeyword Else *> block)
  where
    build start test (Right consequent) Nothing =
      let range = Range (rangeStart start) (rangeEnd (blockRange consequent)) in
      Expression range (ConditionalExpression test consequent Nothing)
    build start test (Left (err, consequent)) Nothing =
      let range = Range (rangeStart start) (rangeEnd (blockRange consequent)) in
      Expression range (ErrorExpression err (Just (ConditionalExpression test consequent Nothing)))

    build start test (Right consequent) (Just (Right alternate)) =
      let range = Range (rangeStart start) (rangeEnd (blockRange alternate)) in
      Expression range (ConditionalExpression test consequent (Just alternate))
    build start test (Right consequent) (Just (Left (err, alternate))) =
      let range = Range (rangeStart start) (rangeEnd (blockRange alternate)) in
      Expression range (ErrorExpression err (Just (ConditionalExpression test consequent (Just alternate))))
    build start test (Left (err, consequent)) (Just (Right alternate)) =
      let range = Range (rangeStart start) (rangeEnd (blockRange alternate)) in
      Expression range (ErrorExpression err (Just (ConditionalExpression test consequent (Just alternate))))
    build start test (Left (err, consequent)) (Just (Left (_, alternate))) =
      let range = Range (rangeStart start) (rangeEnd (blockRange alternate)) in
      Expression range (ErrorExpression err (Just (ConditionalExpression test consequent (Just alternate))))

tryBlockExpression :: Parser Expression
tryBlockExpression = build <$> tryKeyword Do <*> block
  where
    build start (Right b) =
      let range = Range (rangeStart start) (rangeEnd (blockRange b)) in
      Expression range (BlockExpression b)

    build start (Left (err, b)) =
      let range = Range (rangeStart start) (rangeEnd (blockRange b)) in
      Expression range (ErrorExpression err (Just (BlockExpression b)))

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
      let
        range = Range (rangeStart start) (rangeEnd (expressionRange x))
      in
        Expression range (ErrorExpression e (Just (WrappedExpression x)))

data ExpressionExtension
  = PropertyExpressionExtension Name
  | ErrorExpressionExtension Diagnostic

tryExpressionExtension :: Parser ExpressionExtension
tryExpressionExtension =
  tryPropertyExpressionExtension
    <|> unexpected ExpectedExpressionExtension

tryPropertyExpressionExtension :: Parser ExpressionExtension
tryPropertyExpressionExtension =
  build <$> tryGlyph Dot <*> identifier
  where
    build _ (Right (range, ident)) = PropertyExpressionExtension (Name range ident)
    build _ (Left err) = ErrorExpressionExtension err

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
