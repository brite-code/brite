{-# LANGUAGE PatternSynonyms #-}

module Brite.Syntax.Parser
  ( parseModule
  , parseType
  , expressionParser
  , typeParser
  , tryQuantifierListParser
  ) where

import Brite.Diagnostic
import Brite.Syntax.CST
import Brite.Syntax.Glyph
import Brite.Syntax.Identifier
import Brite.Syntax.ParserFramework
import Brite.Syntax.Token
import Brite.Syntax.TokenStream
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq

-- Parses a Brite module from a stream of tokens.
parseModule :: TokenStream -> DiagnosticWriter Module
parseModule tokens = uncurry Module <$> runParser (many tryStatement) tokens

-- Parses a Brite type from a stream of tokens.
parseType :: TokenStream -> DiagnosticWriter (Recover Type)
parseType tokens = fst <$> runParser type_ tokens

-- Parses a Brite expression.
expressionParser :: Parser (Recover Expression)
expressionParser = expression

-- Parses a Brite type.
typeParser :: Parser (Recover Type)
typeParser = type_

-- Try parser for a quantifier list.
tryQuantifierListParser :: TryParser QuantifierList
tryQuantifierListParser = tryQuantifierList

name :: Parser (Recover Name)
name = retry tryName

tryName :: TryParser Name
tryName = uncurry Name <$> tryIdentifier

tryStatement :: TryParser Statement
tryStatement =
  tryBindingStatement
    <|> tryFunctionDeclaration
    <|> tryExpressionStatement
    <|> tryReturnStatement
    <|> tryBreakStatement
    <|> tryEmptyStatement
    <|> unexpected ExpectedStatement

tryExpressionStatement :: TryParser Statement
tryExpressionStatement =
  ExpressionStatement <$> tryExpression <&> semicolon

tryBindingStatement :: TryParser Statement
tryBindingStatement =
  BindingStatement
    <$> tryKeyword Let
    <&> pattern_
    <&> optional tryTypeAnnotation
    <&> glyph Equals'
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

tryEmptyStatement :: TryParser Statement
tryEmptyStatement = EmptyStatement <$> tryGlyph Semicolon

semicolon :: Parser (Maybe (Recover Token))
semicolon = optional (tryGlyph Semicolon)

tryFunctionDeclaration :: TryParser Statement
tryFunctionDeclaration =
  FunctionDeclaration
    <$> tryKeyword Fun
    <&> name
    <&> function

function :: Parser Function
function =
  Function
    <$> optional (tryQuantifierList <|> unexpected (ExpectedGlyph ParenLeft))
    <*> skipIdentifier (glyph ParenLeft)
    <*> commaList tryFunctionParameter
    <*> glyph ParenRight
    <*> optional tryFunctionReturn
    <*> block

tryFunctionParameter :: TryParser FunctionParameter
tryFunctionParameter = FunctionParameter <$> tryPattern <&> optional tryTypeAnnotation

tryFunctionReturn :: TryParser FunctionReturn
tryFunctionReturn = FunctionReturn <$> tryGlyph Arrow <&> type_

block :: Parser Block
block = Block <$> glyph BraceLeft <*> many tryStatement <*> glyph BraceRight

tryConstant :: TryParser Constant
tryConstant = tryBooleanTrue <|> tryBooleanFalse <|> tryNumberConstant <|> tryVoidConstant

tryVoidConstant :: TryParser Constant
tryVoidConstant = VoidConstant <$> tryKeyword Void

tryBooleanTrue :: TryParser Constant
tryBooleanTrue = BooleanConstant True <$> tryKeyword True'

tryBooleanFalse :: TryParser Constant
tryBooleanFalse = BooleanConstant False <$> tryKeyword False'

tryNumberConstant :: TryParser Constant
tryNumberConstant = uncurry NumberConstant <$> tryNumber

-- Ordered roughly by frequency. Parsers that are more likely to match go first.
tryPrimaryExpression :: TryParser Expression
tryPrimaryExpression =
  tryVariableExpression
    <|> tryObjectExpression
    <|> tryFunctionExpression
    <|> tryConditionalExpression
    <|> tryConstantExpression
    <|> tryWrappedExpression
    <|> tryBlockExpression
    <|> tryLoopExpression
    <|> unexpected ExpectedExpression

tryConstantExpression :: TryParser Expression
tryConstantExpression = ConstantExpression <$> tryConstant

tryVariableExpression :: TryParser Expression
tryVariableExpression = VariableExpression <$> tryName

tryFunctionExpression :: TryParser Expression
tryFunctionExpression =
  FunctionExpression
    <$> tryKeyword Fun
    <&> function

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
    -- a user writing valid syntax with an object literal test expression `if {} {}`.
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

tryBlockExpression :: TryParser Expression
tryBlockExpression = BlockExpression <$> tryKeyword Do <&> block

tryLoopExpression :: TryParser Expression
tryLoopExpression = LoopExpression <$> tryKeyword Loop <&> block

tryWrappedExpression :: TryParser Expression
tryWrappedExpression =
  WrappedExpression
    <$> tryGlyph ParenLeft
    <&> expression
    <&> optional tryTypeAnnotation
    <&> glyph ParenRight

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

tryPrefixExpression :: TryParser Expression
tryPrefixExpression =
  not_
    <|> negative
    <|> positive
    <|> trySecondaryExpression
  where
    operand = retry tryPrefixExpression
    not_ = (PrefixExpression Not) <$> tryGlyph Bang <&> operand
    negative = (PrefixExpression Negative) <$> tryGlyph Minus <&> operand
    positive = (PrefixExpression Positive) <$> tryGlyph Plus <&> operand

tryInfixExpression :: TryParser Expression
tryInfixExpression = build' <$> tryInfixExpressionOperand <&> many tryInfixExpressionOperation
  where
    build' x [] = x
    build' x ops = build x Seq.empty ops

    build x ops [] = into x ops
    build x ops [op] = into x (ops |> op)
    build x ops1 (op1 : ops2@(op2 : _)) =
      let
        p1 = infixExpressionOperationPrecedence op1
        p2 = infixExpressionOperationPrecedence op2
      in
        -- `* E`, `+ E`
        if p1 < p2 then
          build (into x (ops1 |> op1)) Seq.empty ops2

        -- `+ E`, `* E`
        else if p1 > p2 then
          case op1 of
            Ok (InfixExpressionOperation op t (Ok y)) ->
              let op1' = Ok (InfixExpressionOperation op t (Ok (build y Seq.empty ops2))) in
                into x (ops1 |> op1')
            Recover ts e (InfixExpressionOperation op t (Ok y)) ->
              let op1' = Recover ts e (InfixExpressionOperation op t (Ok (build y Seq.empty ops2))) in
                into x (ops1 |> op1')
            Ok (InfixExpressionOperation op t (Recover ts e y)) ->
              let op1' = Ok (InfixExpressionOperation op t (Recover ts e (build y Seq.empty ops2))) in
                into x (ops1 |> op1')
            Recover ts1 e1 (InfixExpressionOperation op t (Recover ts2 e2 y)) ->
              let op1' = Recover ts1 e1 (InfixExpressionOperation op t (Recover ts2 e2 (build y Seq.empty ops2))) in
                into x (ops1 |> op1')

            -- NOTE: These cases should be unreachable since in `infixExpressionOperationPrecedence`
            -- we give fatal `FatalPrecedence` which is the smallest precedence. Therefore it is
            -- impossible for a precedence to ever be greater than `FatalPrecedence`. Use a dummy
            -- implementation instead of throwing with `error`, though, because you never know.
            Ok (InfixExpressionOperation _ _ (Fatal _ _)) -> build x (ops1 |> op1) ops2
            Recover _ _ (InfixExpressionOperation _ _ (Fatal _ _)) -> build x (ops1 |> op1) ops2
            Fatal _ _ -> build x (ops1 |> op1) ops2

        -- `+ E`, `+ E`
        else
          build x (ops1 |> op1) ops2

    into x Empty = x
    into x (Ok op :<| ops) = ExpressionExtra x (Ok (InfixExpressionExtra op ops))
    into x (Recover ts e op :<| ops) = ExpressionExtra x (Recover ts e (InfixExpressionExtra op ops))
    into x (Fatal ts e :<| ops) = into (ExpressionExtra x (Fatal ts e)) ops

tryInfixExpressionOperand :: TryParser Expression
tryInfixExpressionOperand = tryPrefixExpression

tryInfixExpressionOperation :: TryParser InfixExpressionOperation
tryInfixExpressionOperation =
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
    make = InfixExpressionOperation
    operand = retry tryInfixExpressionOperand
    add = make Add <$> tryGlyph Plus <&> operand
    subtract_ = make Subtract <$> tryGlyph Minus <&> operand
    multiply = make Multiply <$> tryGlyph Asterisk <&> operand
    divide = make Divide <$> tryGlyph Slash <&> operand
    remainder = make Remainder <$> tryGlyph Percent <&> operand
    exponent_ = make Exponent <$> tryGlyph Caret <&> operand
    equals = make Equals <$> tryGlyph EqualsDouble <&> operand
    notEquals = make NotEquals <$> tryGlyph EqualsNot <&> operand
    lessThan = make LessThan <$> tryGlyph LessThan' <&> operand
    lessThanOrEqual = make LessThanOrEqual <$> tryGlyph LessThanOrEqual' <&> operand
    greaterThan = make GreaterThan <$> tryGlyph GreaterThan' <&> operand
    greaterThanOrEqual = make GreaterThanOrEqual <$> tryGlyph GreaterThanOrEqual' <&> operand
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

-- Gets the precedence level of a infix operator.
infixOperatorPrecedence :: InfixOperator -> Precedence
infixOperatorPrecedence Exponent = Exponentiation
infixOperatorPrecedence Multiply = Multiplicative
infixOperatorPrecedence Divide = Multiplicative
infixOperatorPrecedence Remainder = Multiplicative
infixOperatorPrecedence Add = Additive
infixOperatorPrecedence Subtract = Additive
infixOperatorPrecedence LessThan = Relational
infixOperatorPrecedence LessThanOrEqual = Relational
infixOperatorPrecedence GreaterThan = Relational
infixOperatorPrecedence GreaterThanOrEqual = Relational
infixOperatorPrecedence Equals = Equality
infixOperatorPrecedence NotEquals = Equality
infixOperatorPrecedence And = LogicalAnd
infixOperatorPrecedence Or = LogicalOr

-- Gets the precedence level of a infix expression operation.
infixExpressionOperationPrecedence :: Recover InfixExpressionOperation -> Precedence
infixExpressionOperationPrecedence (Ok (InfixExpressionOperation _ _ (Fatal _ _))) = FatalPrecedence
infixExpressionOperationPrecedence (Recover _ _ (InfixExpressionOperation _ _ (Fatal _ _))) = FatalPrecedence
infixExpressionOperationPrecedence (Ok (InfixExpressionOperation op _ _)) = infixOperatorPrecedence op
infixExpressionOperationPrecedence (Recover _ _ (InfixExpressionOperation op _ _)) = infixOperatorPrecedence op
infixExpressionOperationPrecedence (Fatal _ _) = FatalPrecedence

tryExpression :: TryParser Expression
tryExpression = tryInfixExpression

expression :: Parser (Recover Expression)
expression = retry tryExpression

pattern_ :: Parser (Recover Pattern)
pattern_ = retry tryPattern

tryPattern :: TryParser Pattern
tryPattern =
  tryVariablePattern
    <|> tryObjectPattern
    <|> tryHolePattern
    <|> tryConstantPattern
    <|> tryWrappedPattern
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
tryObjectPatternPropertyValue = ObjectPatternPropertyValue <$> tryGlyph Colon <&> pattern_

tryObjectPatternExtension :: TryParser ObjectPatternExtension
tryObjectPatternExtension =
  ObjectPatternExtension
    <$> tryGlyph Bar
    <&> pattern_

tryWrappedPattern :: TryParser Pattern
tryWrappedPattern =
  WrappedPattern
    <$> tryGlyph ParenLeft
    <&> pattern_
    <&> glyph ParenRight

tryType :: TryParser Type
tryType =
  tryVariableType
    <|> tryObjectType
    <|> tryFunctionType
    <|> tryVoidType
    <|> tryQuantifiedType
    <|> tryBottomType
    <|> tryWrappedType
    <|> unexpected ExpectedType

type_ :: Parser (Recover Type)
type_ = retry tryType

tryVariableType :: TryParser Type
tryVariableType = VariableType <$> tryName

tryBottomType :: TryParser Type
tryBottomType = BottomType <$> tryGlyph Bang

tryVoidType :: TryParser Type
tryVoidType = VoidType <$> tryKeyword Void

tryFunctionType :: TryParser Type
tryFunctionType =
  FunctionType
    <$> tryKeyword Fun
    <&> optional (tryQuantifierList <|> unexpected (ExpectedGlyph ParenLeft))
    <&> glyph ParenLeft
    <&> commaList tryType
    <&> glyph ParenRight
    <&> glyph Arrow
    <&> type_

tryObjectType :: TryParser Type
tryObjectType =
  ObjectType
    <$> tryGlyph BraceLeft
    <&> commaList tryObjectTypeProperty
    <&> optional tryObjectTypeExtension
    <&> glyph BraceRight

tryObjectTypeProperty :: TryParser ObjectTypeProperty
tryObjectTypeProperty =
  ObjectTypeProperty
    <$> tryName
    <&> glyph Colon
    <&> type_

tryObjectTypeExtension :: TryParser ObjectTypeExtension
tryObjectTypeExtension =
  ObjectTypeExtension
    <$> tryGlyph Bar
    <&> type_

tryQuantifiedType :: TryParser Type
tryQuantifiedType = QuantifiedType <$> tryQuantifierList <&> type_

tryQuantifierList :: TryParser QuantifierList
tryQuantifierList =
  QuantifierList
    <$> tryGlyph LessThan'
    <&> commaList tryQuantifier
    <&> glyph GreaterThan'

tryQuantifier :: TryParser Quantifier
tryQuantifier = Quantifier <$> tryName <&> optional tryBound
  where
    tryBound =
      (QuantifierBound Flexible <$> tryGlyph Colon <&> type_)
        <|> (QuantifierBound Rigid <$> tryGlyph Equals' <&> type_)

tryWrappedType :: TryParser Type
tryWrappedType =
  WrappedType
    <$> tryGlyph ParenLeft
    <&> type_
    <&> glyph ParenRight

tryTypeAnnotation :: TryParser TypeAnnotation
tryTypeAnnotation =
  TypeAnnotation
    <$> tryGlyph Colon
    <&> type_
