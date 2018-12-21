module Brite.Parser
  ( parse
  ) where

import Brite.AST
import Brite.Parser.Framework
import Brite.Diagnostics
import Brite.Source

-- Parses a Brite module from a stream of tokens.
parse :: TokenStream -> DiagnosticWriter Module
parse tokens = fromRight <$> runParser module_ tokens
  where
    fromRight (Right x) = x
    fromRight (Left _) = error "Unexpected failure."

module_ :: Parser Module
module_ = Module <$> many tryStatement <*> end

tryStatement :: Parser Statement
tryStatement =
  tryBindingStatement
    <|> tryExpressionStatement
    <|> unexpected ExpectedStatement

tryExpressionStatement :: Parser Statement
tryExpressionStatement = ExpressionStatement <$> tryExpression <*> optional (tryGlyph Semicolon)

tryBindingStatement :: Parser Statement
tryBindingStatement =
  BindingStatement
    <$> tryKeyword Let
    <*> pattern
    <*> glyph Equals
    <*> expression
    <*> optional (tryGlyph Semicolon)

block :: Parser Block
block = Block <$> glyph BraceLeft <*> many tryStatement <*> glyph BraceRight

tryConstant :: Parser Constant
tryConstant = tryBooleanTrue <|> tryBooleanFalse

tryBooleanTrue :: Parser Constant
tryBooleanTrue = BooleanConstant True <$> tryKeyword True_

tryBooleanFalse :: Parser Constant
tryBooleanFalse = BooleanConstant False <$> tryKeyword False_

expression :: Parser (Recover Expression)
expression = retry (tryExpression <|> unexpected ExpectedExpression)

-- Ordered by frequency. Parsers that are more likely to match go first.
tryPrimaryExpression :: Parser Expression
tryPrimaryExpression =
  tryVariableExpression
    <|> tryConditionalExpression
    <|> tryWrappedExpression
    <|> tryConstantExpression
    <|> tryBlockExpression

tryExpression :: Parser Expression
tryExpression = foldl ExpressionExtension <$> tryPrimaryExpression <*> many tryExpressionExtension

tryConstantExpression :: Parser Expression
tryConstantExpression = ConstantExpression <$> tryConstant

tryVariableExpression :: Parser Expression
tryVariableExpression = VariableExpression . uncurry Name <$> tryIdentifier

tryConditionalExpression :: Parser Expression
tryConditionalExpression =
  ConditionalExpression
    <$> tryKeyword If
    <*> expression
    <*> block
    <*> optional (ConditionalExpressionAlternate <$> tryKeyword Else <*> block)

tryBlockExpression :: Parser Expression
tryBlockExpression = BlockExpression <$> tryKeyword Do <*> block

tryWrappedExpression :: Parser Expression
tryWrappedExpression = WrappedExpression <$> tryGlyph ParenLeft <*> expression <*> glyph ParenRight

tryExpressionExtension :: Parser ExpressionExtension
tryExpressionExtension =
  tryPropertyExpressionExtension
    <|> unexpected ExpectedExpression

tryPropertyExpressionExtension :: Parser ExpressionExtension
tryPropertyExpressionExtension =
  PropertyExpressionExtension <$> tryGlyph Dot <*> (fmap (uncurry Name) <$> identifier)

pattern :: Parser (Recover Pattern)
pattern = retry $
  tryVariablePattern
    <|> unexpected ExpectedPattern

tryVariablePattern :: Parser Pattern
tryVariablePattern = VariablePattern . uncurry Name <$> tryIdentifier
