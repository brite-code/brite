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

-- Ordered roughly by frequency. Parsers that are more likely to match go first.
tryPrimaryExpression :: TryParser Expression
tryPrimaryExpression =
  tryVariableExpression
    <|> tryObjectExpression
    <|> tryFunctionExpression
    <|> tryConditionalExpression
    <|> tryConstantExpression
    <|> tryWrappedExpression
    <|> tryVariantExpression
    <|> tryMatchExpression
    <|> tryBlockExpression
    <|> tryLoopExpression
    <|> unexpected ExpectedExpression

tryConstantExpression :: TryParser Expression
tryConstantExpression = ConstantExpression <$> tryConstant

tryVariableExpression :: TryParser Expression
tryVariableExpression = VariableExpression <$> tryName

tryFunctionExpression :: TryParser Expression
tryFunctionExpression = FunctionExpression <$> tryFunction

tryObjectExpression :: TryParser Expression
tryObjectExpression =
  ObjectExpression
    <$> tryGlyph BraceLeft
    <&> commaList tryObjectExpressionProperty
    <&> optional tryObjectExpressionExtension
    <&> glyph BraceRight

tryObjectExpressionProperty :: TryParser ObjectExpressionProperty
tryObjectExpressionProperty =
  ObjectExpressionProperty
    <$> tryName
    <&> optional tryObjectExpressionPropertyValue

tryObjectExpressionPropertyValue :: TryParser ObjectExpressionPropertyValue
tryObjectExpressionPropertyValue = ObjectExpressionPropertyValue <$> tryGlyph Colon <&> expression

tryObjectExpressionExtension :: TryParser ObjectExpressionExtension
tryObjectExpressionExtension =
  ObjectExpressionExtension
    <$> tryGlyph Bar
    <&> expression

tryVariantExpression :: TryParser Expression
tryVariantExpression =
  VariantExpression
    <$> tryGlyph Dot
    <&> name
    <&> optional tryVariantExpressionElements

tryVariantExpressionElements :: TryParser VariantExpressionElements
tryVariantExpressionElements =
  VariantExpressionElements
    <$> tryGlyph ParenLeft
    <&> commaList tryExpression
    <&> glyph ParenRight

tryConditionalExpression :: TryParser Expression
tryConditionalExpression = ConditionalExpression <$> tryConditionalExpressionIf

tryConditionalExpressionIf :: TryParser ConditionalExpressionIf
tryConditionalExpressionIf =
  ConditionalExpressionIf
    <$> tryKeyword If
    <&> testExpression
    <&> block
    <&> optional tryConditionalExpressionElse
  where
    -- For the conditional expression’s test (`if test {}`) we disallow all expressions that start
    -- with a left brace (`{`). This includes object literal expressions (`{p: E}`) or any
    -- expression that extends an object literal expression (like `{p: E}.p`). Notably this does not
    -- include wrapped object literal expressions (`({p: E})`).
    --
    -- Why do we do this? Why do we disallow perfectly valid syntax? To improve error recovery
    -- behavior. If we allow object literals in the test expression then `if {}` is interpreted as
    -- a conditional testing an object literal instead of a conditional with no test expression.
    --
    -- We believe that a user writing the invalid syntax `if {}` is _significantly_ more common then
    -- the a user writing valid syntax with an object literal test expression `if {} {}`.
    --
    -- If we ever decide that this was a bad tradeoff then we can remove this special case without
    -- breaking any valid code. Which is nice. But on the other hand, we would never be able to
    -- add this special case without breaking valid code.
    --
    -- NOTE: If this ever confuses people we can improve the error message to suggest wrapping an
    -- object test expression.
    --
    -- NOTE: This special case is an argument _against_ adding a pipeline operator. Since a pipeline
    -- operator might encourage test expressions that start with `{`. For example:
    -- `if {p: E} |> doSomething() {}`.
    --
    -- NOTE: Rust makes a very similar tradeoff when it comes to structs in conditionals. Consider:
    -- `if Foo { i: 42 } { ... }`. Rust assumes that `{ i: 42 }` is a block and not struct
    -- properties. The programmer must wrap their struct for the expression to make sense:
    -- `if (Foo { i: 42 }) { ... }`.
    testExpression = retry (unexpectedGlyph ExpectedExpression BraceLeft tryExpression)

tryConditionalExpressionElse :: TryParser ConditionalExpressionElse
tryConditionalExpressionElse = flip ($) <$> tryKeyword Else <&> elseIf
  where
    elseIf =
      tryOnce
        (flip ConditionalExpressionElseIf <$> tryConditionalExpressionIf)
        (flip ConditionalExpressionElse <$> block)

tryMatchExpression :: TryParser Expression
tryMatchExpression =
  MatchExpression
    <$> tryKeyword Match
    <&> expression
    <&> glyph BraceLeft
    <&> many tryMatchExpressionCase
    <&> glyph BraceRight

tryMatchExpressionCase :: TryParser MatchExpressionCase
tryMatchExpressionCase =
  MatchExpressionCase
    <$> tryPattern
    <&> glyph Arrow
    <&> block

tryBlockExpression :: TryParser Expression
tryBlockExpression = BlockExpression <$> tryKeyword Do <&> block

tryLoopExpression :: TryParser Expression
tryLoopExpression = LoopExpression <$> tryKeyword Loop <&> block

tryWrappedExpression :: TryParser Expression
tryWrappedExpression = WrappedExpression <$> tryGlyph ParenLeft <&> expression <&> glyph ParenRight

trySecondaryExpression :: TryParser Expression
trySecondaryExpression =
  foldl ExpressionExtra <$> tryPrimaryExpression <&> many trySecondaryExpressionExtra

trySecondaryExpressionExtra :: TryParser ExpressionExtra
trySecondaryExpressionExtra =
  tryPropertyExpressionExtra
    <|> tryCallExpressionExtra
    <|> unexpected ExpectedExpression

tryPropertyExpressionExtra :: TryParser ExpressionExtra
tryPropertyExpressionExtra =
  PropertyExpressionExtra <$> tryGlyph Dot <&> name

tryCallExpressionExtra :: TryParser ExpressionExtra
tryCallExpressionExtra =
  CallExpressionExtra
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
tryBinaryExpression = build <$> tryBinaryExpressionOperand <&> many tryBinaryExpressionExtra
  where
    build x [] = x
    build x (ext : exts) =
      let p = binaryExpressionExtraPrecedence ext in
        build (insert x p ext) exts

    insert x p ext =
      case x of
        BinaryExpression l1 (Ok (BinaryExpressionExtra op t (Ok l2))) | p < binaryOperatorPrecedence op ->
          BinaryExpression l1 (Ok (BinaryExpressionExtra op t (Ok (insert l2 p ext))))
        BinaryExpression l1 (Recover ts e (BinaryExpressionExtra op t (Ok l2))) | p < binaryOperatorPrecedence op ->
          BinaryExpression l1 (Recover ts e (BinaryExpressionExtra op t (Ok (insert l2 p ext))))
        BinaryExpression l1 (Ok (BinaryExpressionExtra op t (Recover ts e l2))) | p < binaryOperatorPrecedence op ->
          BinaryExpression l1 (Ok (BinaryExpressionExtra op t (Recover ts e (insert l2 p ext))))
        BinaryExpression l1 (Recover ts1 e1 (BinaryExpressionExtra op t (Recover ts2 e2 l2))) | p < binaryOperatorPrecedence op ->
          BinaryExpression l1 (Recover ts1 e1 (BinaryExpressionExtra op t (Recover ts2 e2 (insert l2 p ext))))
        BinaryExpression l1 (Ok (BinaryExpressionExtra op t l2@(Fatal _ _))) | p < binaryOperatorPrecedence op ->
          BinaryExpression l1 (Ok (BinaryExpressionExtra op t (Ok (BinaryExpression l2 ext))))
        BinaryExpression l1 (Recover ts e (BinaryExpressionExtra op t l2@(Fatal _ _))) | p < binaryOperatorPrecedence op ->
          BinaryExpression l1 (Recover ts e (BinaryExpressionExtra op t (Ok (BinaryExpression l2 ext))))
        _ ->
          BinaryExpression (Ok x) ext

tryBinaryExpressionOperand :: TryParser Expression
tryBinaryExpressionOperand = tryUnaryExpression

tryBinaryExpressionExtra :: TryParser BinaryExpressionExtra
tryBinaryExpressionExtra =
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
    <|> and_
    <|> or_
    <|> unexpected ExpectedExpression
  where
    make = BinaryExpressionExtra
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
    and_ = make And <$> tryGlyph AmpersandDouble <&> operand
    or_ = make Or <$> tryGlyph BarDouble <&> operand

-- The precedence level of an operator.
data Precedence
  = FatalPrecedence
  | Exponentiation
  | Multiplicative
  | Additive
  | Relational
  | Equality
  | LogicalAnd
  | LogicalOr
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
binaryOperatorPrecedence And = LogicalAnd
binaryOperatorPrecedence Or = LogicalOr

binaryExpressionExtraPrecedence :: Recover BinaryExpressionExtra -> Precedence
binaryExpressionExtraPrecedence (Ok (BinaryExpressionExtra op _ _)) = binaryOperatorPrecedence op
binaryExpressionExtraPrecedence (Recover _ _ (BinaryExpressionExtra op _ _)) = binaryOperatorPrecedence op
binaryExpressionExtraPrecedence (Fatal _ _) = FatalPrecedence

tryExpression :: TryParser Expression
tryExpression = tryBinaryExpression

expression :: Parser (Recover Expression)
expression = retry tryExpression

pattern :: Parser (Recover Pattern)
pattern = retry tryPattern

tryPattern :: TryParser Pattern
tryPattern =
  tryVariablePattern
    <|> tryObjectPattern
    <|> tryVariantPattern
    <|> tryHolePattern
    <|> tryConstantPattern
    <|> unexpected ExpectedPattern

tryConstantPattern :: TryParser Pattern
tryConstantPattern = ConstantPattern <$> tryConstant

tryVariablePattern :: TryParser Pattern
tryVariablePattern = VariablePattern <$> tryName

tryHolePattern :: TryParser Pattern
tryHolePattern = HolePattern <$> tryKeyword Hole

tryObjectPattern :: TryParser Pattern
tryObjectPattern =
  ObjectPattern
    <$> tryGlyph BraceLeft
    <&> commaList tryObjectPatternProperty
    <&> optional tryObjectPatternExtension
    <&> glyph BraceRight

tryObjectPatternProperty :: TryParser ObjectPatternProperty
tryObjectPatternProperty =
  ObjectPatternProperty
    <$> tryName
    <&> optional tryObjectPatternPropertyValue

tryObjectPatternPropertyValue :: TryParser ObjectPatternPropertyValue
tryObjectPatternPropertyValue = ObjectPatternPropertyValue <$> tryGlyph Colon <&> pattern

tryObjectPatternExtension :: TryParser ObjectPatternExtension
tryObjectPatternExtension =
  ObjectPatternExtension
    <$> tryGlyph Bar
    <&> pattern

tryVariantPattern :: TryParser Pattern
tryVariantPattern =
  VariantPattern
    <$> tryGlyph Dot
    <&> name
    <&> optional tryVariantPatternElements

tryVariantPatternElements :: TryParser VariantPatternElements
tryVariantPatternElements =
  VariantPatternElements
    <$> tryGlyph ParenLeft
    <&> commaList tryPattern
    <&> glyph ParenRight
