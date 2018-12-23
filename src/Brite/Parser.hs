module Brite.Parser
  ( parse
  ) where

import Brite.AST
import Brite.Parser.Framework
import Brite.Diagnostics
import Brite.Source

-- Parses a Brite module from a stream of tokens.
parse :: TokenStream -> DiagnosticWriter Module
parse tokens = uncurry Module <$> runParser (many tryStatement) tokens

name :: Parser (Recover Name)
name = retry tryName

tryName :: TryParser Name
tryName = uncurry Name <$> tryIdentifier

tryStatement :: TryParser Statement
tryStatement =
  tryBindingStatement
    <|> tryExpressionStatement
    <|> unexpected ExpectedStatement

tryExpressionStatement :: TryParser Statement
tryExpressionStatement =
  ExpressionStatement <$> tryFullExpression <&> optional (tryGlyph Semicolon)

tryBindingStatement :: TryParser Statement
tryBindingStatement =
  BindingStatement
    <$> tryKeyword Let
    <&> pattern
    <&> glyph Equals
    <&> expression
    <&> optional (tryGlyph Semicolon)

block :: Parser Block
block = Block <$> glyph BraceLeft <*> many tryStatement <*> glyph BraceRight

tryFunction :: TryParser Function
tryFunction =
  Function
    <$> tryKeyword Fun
    <&> optional tryName
    <&> glyph ParenLeft
    <&> commaList tryPattern
    <&> glyph ParenRight
    <&> block

tryConstant :: TryParser Constant
tryConstant = tryBooleanTrue <|> tryBooleanFalse

tryBooleanTrue :: TryParser Constant
tryBooleanTrue = BooleanConstant True <$> tryKeyword True_

tryBooleanFalse :: TryParser Constant
tryBooleanFalse = BooleanConstant False <$> tryKeyword False_

expression :: Parser (Recover Expression)
expression = retry tryExpression

-- Ordered by frequency. Parsers that are more likely to match go first.
tryPrimaryExpression :: TryParser Expression
tryPrimaryExpression =
  tryVariableExpression
    <|> tryFunctionExpression
    <|> tryConditionalExpression
    <|> tryWrappedExpression
    <|> tryConstantExpression
    <|> tryBlockExpression

tryFullExpression :: TryParser Expression
tryFullExpression =
  foldl ExpressionExtension <$> tryPrimaryExpression <&> many tryExpressionExtension

tryExpression :: TryParser Expression
tryExpression = tryFullExpression <|> unexpected ExpectedExpression

tryConstantExpression :: TryParser Expression
tryConstantExpression = ConstantExpression <$> tryConstant

tryVariableExpression :: TryParser Expression
tryVariableExpression = VariableExpression <$> tryName

tryFunctionExpression :: TryParser Expression
tryFunctionExpression = FunctionExpression <$> tryFunction

tryConditionalExpression :: TryParser Expression
tryConditionalExpression =
  ConditionalExpression
    <$> tryKeyword If
    <&> expression
    <&> block
    <&> optional (ConditionalExpressionAlternate <$> tryKeyword Else <&> block)

tryBlockExpression :: TryParser Expression
tryBlockExpression = BlockExpression <$> tryKeyword Do <&> block

tryWrappedExpression :: TryParser Expression
tryWrappedExpression = WrappedExpression <$> tryGlyph ParenLeft <&> expression <&> glyph ParenRight

tryExpressionExtension :: TryParser ExpressionExtension
tryExpressionExtension =
  tryPropertyExpressionExtension
    <|> tryCallExpressionExtension
    <|> unexpected ExpectedExpression

tryPropertyExpressionExtension :: TryParser ExpressionExtension
tryPropertyExpressionExtension =
  PropertyExpressionExtension <$> tryGlyph Dot <&> name

tryCallExpressionExtension :: TryParser ExpressionExtension
tryCallExpressionExtension =
  CallExpressionExtension
    <$> tryGlyphOnSameLine ParenLeft
    <&> commaList tryExpression
    <&> glyph ParenRight

pattern :: Parser (Recover Pattern)
pattern = retry tryPattern

tryPattern :: TryParser Pattern
tryPattern =
  tryVariablePattern
    <|> tryHolePattern
    <|> unexpected ExpectedPattern

tryVariablePattern :: TryParser Pattern
tryVariablePattern = VariablePattern <$> tryName

tryHolePattern :: TryParser Pattern
tryHolePattern = HolePattern <$> tryKeyword Hole
