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
    <|> tryReturnStatement
    <|> tryBreakStatement
    <|> unexpected ExpectedStatement

tryExpressionStatement :: TryParser Statement
tryExpressionStatement =
  ExpressionStatement <$> tryExpression <&> semicolon

tryBindingStatement :: TryParser Statement
tryBindingStatement =
  BindingStatement
    <$> tryKeyword Let
    <&> pattern
    <&> glyph Equals_
    <&> expression
    <&> semicolon

tryReturnStatement :: TryParser Statement
tryReturnStatement =
  ReturnStatement
    <$> tryKeyword Return
    <&> optionalOnSameLine tryExpression
    <&> semicolon

tryBreakStatement :: TryParser Statement
tryBreakStatement =
  BreakStatement
    <$> tryKeyword Break
    <&> optionalOnSameLine tryExpression
    <&> semicolon

semicolon :: Parser (Maybe (Recover Token))
semicolon = optional (tryGlyph Semicolon)

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

-- Ordered roughly by frequency. Parsers that are more likely to match go first.
tryPrimaryExpression :: TryParser Expression
tryPrimaryExpression =
  tryVariableExpression
    <|> tryFunctionExpression
    <|> tryConditionalExpression
    <|> tryWrappedExpression
    <|> tryConstantExpression
    <|> tryBlockExpression
    <|> tryLoopExpression
    <|> unexpected ExpectedExpression

tryConstantExpression :: TryParser Expression
tryConstantExpression = ConstantExpression <$> tryConstant

tryVariableExpression :: TryParser Expression
tryVariableExpression = VariableExpression <$> tryName

tryFunctionExpression :: TryParser Expression
tryFunctionExpression = FunctionExpression <$> tryFunction

tryConditionalExpression :: TryParser Expression
tryConditionalExpression = ConditionalExpression <$> tryConditionalExpressionIf

tryConditionalExpressionIf :: TryParser ConditionalExpressionIf
tryConditionalExpressionIf =
  ConditionalExpressionIf
    <$> tryKeyword If
    <&> expression
    <&> block
    <&> optional tryConditionalExpressionElse

tryConditionalExpressionElse :: TryParser ConditionalExpressionElse
tryConditionalExpressionElse = flip ($) <$> tryKeyword Else <&> elseIf
  where
    elseIf =
      tryOnce
        (flip ConditionalExpressionElseIf <$> tryConditionalExpressionIf)
        (flip ConditionalExpressionElse <$> block)

tryBlockExpression :: TryParser Expression
tryBlockExpression = BlockExpression <$> tryKeyword Do <&> block

tryLoopExpression :: TryParser Expression
tryLoopExpression = LoopExpression <$> tryKeyword Loop <&> block

tryWrappedExpression :: TryParser Expression
tryWrappedExpression = WrappedExpression <$> tryGlyph ParenLeft <&> expression <&> glyph ParenRight

trySecondaryExpression :: TryParser Expression
trySecondaryExpression =
  foldl ExpressionExtension <$> tryPrimaryExpression <&> many trySecondaryExpressionExtension

trySecondaryExpressionExtension :: TryParser ExpressionExtension
trySecondaryExpressionExtension =
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

tryUnaryExpression :: TryParser Expression
tryUnaryExpression =
  not_
    <|> negative
    <|> positive
    <|> trySecondaryExpression
  where
    operand = retry tryUnaryExpression
    not_ = (UnaryExpression Not) <$> tryGlyph Bang <&> operand
    negative = (UnaryExpression Negative) <$> tryGlyph Minus <&> operand
    positive = (UnaryExpression Positive) <$> tryGlyph Plus <&> operand

tryBinaryExpression :: TryParser Expression
tryBinaryExpression = build <$> tryBinaryExpressionOperand <&> many tryBinaryExpressionExtension
  where
    build x [] = x
    build x (ext : exts) =
      let p = binaryExpressionExtensionPrecedence ext in
        build (insert x p ext) exts

    insert x p ext =
      case x of
        BinaryExpression l1 (Ok (BinaryExpressionExtension op t l2)) | p < binaryOperatorPrecedence op ->
          BinaryExpression l1 (Ok (BinaryExpressionExtension op t
            (Ok (BinaryExpression l2 ext))))
        BinaryExpression l1 (Recover ts e (BinaryExpressionExtension op t l2)) | p < binaryOperatorPrecedence op ->
          BinaryExpression l1 (Recover ts e (BinaryExpressionExtension op t
            (Ok (BinaryExpression l2 ext))))
        _ ->
          BinaryExpression (Ok x) ext

tryBinaryExpressionOperand :: TryParser Expression
tryBinaryExpressionOperand = tryUnaryExpression

tryBinaryExpressionExtension :: TryParser BinaryExpressionExtension
tryBinaryExpressionExtension =
  add
    <|> subtract_
    <|> multiply
    <|> divide
    <|> remainder
    <|> exponent_
    <|> equals
    <|> notEquals
    <|> lessThan
    <|> lessThanOrEqual
    <|> greaterThan
    <|> greaterThanOrEqual
    <|> unexpected ExpectedExpression
  where
    make = BinaryExpressionExtension
    operand = retry tryBinaryExpressionOperand
    add = make Add <$> tryGlyph Plus <&> operand
    subtract_ = make Subtract <$> tryGlyph Minus <&> operand
    multiply = make Multiply <$> tryGlyph Asterisk <&> operand
    divide = make Divide <$> tryGlyph Slash <&> operand
    remainder = make Remainder <$> tryGlyph Percent <&> operand
    exponent_ = make Exponent <$> tryGlyph Caret <&> operand
    equals = make Equals <$> tryGlyph EqualsDouble <&> operand
    notEquals = make NotEquals <$> tryGlyph EqualsNot <&> operand
    lessThan = make LessThan <$> tryGlyph LessThan_ <&> operand
    lessThanOrEqual = make LessThanOrEqual <$> tryGlyph LessThanOrEqual_ <&> operand
    greaterThan = make GreaterThan <$> tryGlyph GreaterThan_ <&> operand
    greaterThanOrEqual = make GreaterThanOrEqual <$> tryGlyph GreaterThanOrEqual_ <&> operand

-- The precedence level of an operator.
data Precedence
  = FatalPrecedence
  | Exponentiation
  | Multiplicative
  | Additive
  | Relational
  | Equality
  deriving (Eq, Ord)

-- Gets the precedence level of a binary operator.
binaryOperatorPrecedence :: BinaryOperator -> Precedence
binaryOperatorPrecedence Exponent = Exponentiation
binaryOperatorPrecedence Multiply = Multiplicative
binaryOperatorPrecedence Divide = Multiplicative
binaryOperatorPrecedence Remainder = Multiplicative
binaryOperatorPrecedence Add = Additive
binaryOperatorPrecedence Subtract = Additive
binaryOperatorPrecedence LessThan = Relational
binaryOperatorPrecedence LessThanOrEqual = Relational
binaryOperatorPrecedence GreaterThan = Relational
binaryOperatorPrecedence GreaterThanOrEqual = Relational
binaryOperatorPrecedence Equals = Equality
binaryOperatorPrecedence NotEquals = Equality

-- Gets the precedence level of a binary expression extension.
binaryExpressionExtensionPrecedence :: Recover BinaryExpressionExtension -> Precedence
binaryExpressionExtensionPrecedence (Ok (BinaryExpressionExtension op _ _)) = binaryOperatorPrecedence op
binaryExpressionExtensionPrecedence (Recover _ _ (BinaryExpressionExtension op _ _)) = binaryOperatorPrecedence op
binaryExpressionExtensionPrecedence (Fatal _ _) = FatalPrecedence

tryExpression :: TryParser Expression
tryExpression = tryBinaryExpression

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
