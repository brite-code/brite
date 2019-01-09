-- The Concrete Syntax Tree (CST) represents a parsed Brite program including every detail from the
-- source file. A CST may trivially be converted back into the source code it was parsed from. In
-- that way the CST is unlike an Abstract Syntax Tree (AST) which usually is not printable into the
-- exact source code it was parsed from.
--
-- What makes the Brite CST even more interesting is that our parser supports error recovery. This
-- means our CST contains not only all the expected tokens for a Brite program but also all the
-- unexpected tokens. Even with a bunch of unexpected tokens we still preserve the ability to print
-- the CST back into the exact source code it was parsed from.
--
-- We don’t expect the CST to be used when implementing Brite semantics. For that we have an
-- Abstract Syntax Tree (AST) with which we smooth over some of the pedantic nature of the CST.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.CST
  ( Module(..)
  , Name(..)
  , Recover(..)
  , CommaList(..)
  , commaListItems
  , Statement(..)
  , Declaration(..)
  , Function(..)
  , FunctionParameter(..)
  , FunctionReturn(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , ObjectExpressionProperty(..)
  , ObjectExpressionPropertyValue(..)
  , ObjectExpressionExtension(..)
  , UnaryOperator(..)
  , BinaryOperator(..)
  , ConditionalExpressionIf(..)
  , ConditionalExpressionElse(..)
  , ExpressionExtra(..)
  , BinaryExpressionOperation(..)
  , Pattern(..)
  , ObjectPatternProperty(..)
  , ObjectPatternPropertyValue(..)
  , ObjectPatternExtension(..)
  , Type(..)
  , ObjectTypeProperty(..)
  , ObjectTypeExtension(..)
  , QuantifierList(..)
  , Quantifier(..)
  , QuantifierBound(..)
  , QuantifierBoundKind(..)
  , TypeAnnotation(..)
  , moduleTokens
  , expressionTokens
  , moduleSource
  , statementTrimmedSource
  , recoverStatementLeadingTrivia
  ) where

import Brite.Syntax.ParserFramework (Recover(..), CommaList(..), commaListItems)
import Brite.Syntax.Tokens
import Data.Monoid (Endo(..))
import qualified Data.Text.Lazy.Builder as Text (Builder)

-- A single Brite file is a module. A module is made up of a list of statements.
data Module = Module
  { moduleStatements :: [Recover Statement]
  , moduleEnd :: EndToken
  }

-- An identifier with an associated range.
data Name = Name
  { nameIdentifier :: Identifier
  , nameToken :: Token
  }

-- Represents some imperative action to be carried out.
data Statement
  -- ```
  -- E;
  -- ```
  = ExpressionStatement Expression Semicolon

  -- ```
  -- let x = E;
  -- let x: T = E;
  -- ```
  --
  -- Binds a value to a name in the program.
  | BindingStatement
      Token
      (Recover Pattern)
      (Maybe (Recover TypeAnnotation))
      (Recover Token)
      (Recover Expression)
      Semicolon

  -- ```
  -- return;
  -- return E;
  -- ```
  --
  -- Immediately returns a value from a function. No other code in the function runs.
  --
  -- We include `return` and `break` statements since Brite’s algebraic effects lend themselves to
  -- imperative code styles.
  | ReturnStatement Token (Maybe (Recover Expression)) Semicolon

  -- ```
  -- break;
  -- break E;
  -- ```
  --
  -- Immediately breaks out of a loop with a value. No other code in the loop runs.
  --
  -- We don’t yet have labeled break or continue statements which may mostly be emulated by other
  -- means. Strictly speaking, `return` isn’t even necessary. `break` is necessary because we have
  -- loop expressions, but loop expressions aren’t necessary since we have recursion.
  --
  -- We should continue to ask ourselves: do we need the `return` statement or `loop` expressions?
  | BreakStatement Token (Maybe (Recover Expression)) Semicolon

  -- ```
  -- ;
  -- ```
  --
  -- An empty statement is only a semicolon. Aids in error recovery but otherwise has no
  -- practical use.
  | EmptyStatement Token

  -- Some declaration which is not order dependent unlike all statements.
  | Declaration Declaration

-- Convenience type alias for an optional semicolon token.
type Semicolon = Maybe (Recover Token)

-- NOTE: Eventually we will also add a `TypeDeclaration`.
data Declaration
  -- ```
  -- fun f(...) { ... }
  -- fun f(...) -> T { ... }
  -- fun f<T>(...) { ... }
  -- ```
  --
  -- `FunctionDeclaration` syntax overlaps significantly with `FunctionExpression`. We separate the
  -- two because function declarations are mutually recursive among all function declarations.
  -- Function declarations require the name to be present and don’t allow expression “extra”s like
  -- a function call.
  = FunctionDeclaration Token (Recover Name) Function

-- ```
-- (...) { ... }
-- (...) -> T { ... }
-- <T>(...) { ... }
-- ```
--
-- The data necessary for creating a function. Excluding the function keyword and optional
-- function name.
data Function = Function
  { functionQuantifiers :: Maybe (Recover QuantifierList)
  , functionParamsOpen :: Recover Token
  , functionParams :: CommaList FunctionParameter
  , functionParamsClose :: Recover Token
  , functionReturn :: Maybe (Recover FunctionReturn)
  , functionBody :: Block
  }

-- `x: T`
--
-- A single function parameter with an optional type annotation.
data FunctionParameter = FunctionParameter Pattern (Maybe (Recover TypeAnnotation))

-- `-> T`
--
-- Type annotation for the value returned by the function.
data FunctionReturn = FunctionReturn Token (Recover Type)

-- A set of statements scoped in a block. Names declared in this block may only be accessed by code
-- within the block.
data Block = Block
  { blockOpen :: Recover Token
  , blockStatements :: [Recover Statement]
  , blockClose :: Recover Token
  }

-- Some constant value in our program.
data Constant
  -- `true`, `false`
  = BooleanConstant Bool Token

-- Some instructions our programming language interprets to return a value and possibly perform
-- some side effects.
data Expression
  -- ```
  -- C
  -- ```
  --
  -- Some constant value in the program which never changes.
  = ConstantExpression Constant

  -- ```
  -- x
  -- ```
  --
  -- A reference to a variable binding in the program.
  | VariableExpression Name

  -- ```
  -- fun(...) { ... }
  -- fun(...) -> T { ... }
  -- fun<T>(...) { ... }
  -- ```
  --
  -- A block of code which is executed whenever the function is called. Shares a lot of syntax with
  -- function declarations.
  --
  -- Function expressions are never named. Unlike function declarations which are always named. In
  -- JavaScript function expressions are optionally named. We choose to never name function
  -- expressions because that would cause confusion with function declarations. It’s easy to see the
  -- difference between a function expression and declaration when expressions are never named.
  | FunctionExpression Token Function

  -- ```
  -- {p: E, ...}
  -- ```
  --
  -- A collection of labeled data.
  | ObjectExpression
      Token
      (CommaList ObjectExpressionProperty)
      (Maybe (Recover ObjectExpressionExtension))
      (Recover Token)

  -- ```
  -- !E
  -- -E
  -- +E
  -- ``
  --
  -- An operation on a single expression.
  | UnaryExpression UnaryOperator Token (Recover Expression)

  -- ```
  -- if E { ... }
  -- if E { ... } else { ... }
  -- if E { ... } else if E { ... } else { ... }
  -- ```
  --
  -- Conditionally executes some code.
  | ConditionalExpression ConditionalExpressionIf

  -- ```
  -- do { ... }
  -- ```
  --
  -- Introduces a new block scope into the program.
  | BlockExpression Token Block

  -- ```
  -- loop { ... }
  -- ``
  --
  -- Keeps repeatedly executing the block until a break statement is encountered. The argument to
  -- the break statement is the value returned by the loop.
  | LoopExpression Token Block

  -- ```
  -- (E)
  -- (E: T)
  -- ``
  --
  -- An expression wrapped in parentheses. Useful for changing the precedence of operators.
  | WrappedExpression
      Token
      (Recover Expression)
      (Maybe (Recover TypeAnnotation))
      (Recover Token)

  -- ```
  -- E + E
  -- E.p
  -- E()
  -- ``
  --
  -- Any extra syntax on a primary expression. Including property expressions, function calls,
  -- and more.
  | ExpressionExtra Expression (Recover ExpressionExtra)

-- `p: E`
--
-- A single object property.
data ObjectExpressionProperty =
  ObjectExpressionProperty Name (Maybe (Recover ObjectExpressionPropertyValue))

-- `: E`
--
-- The value of a single object property.
data ObjectExpressionPropertyValue = ObjectExpressionPropertyValue Token (Recover Expression)

-- `| E`
--
-- An extension operation on an object.
data ObjectExpressionExtension = ObjectExpressionExtension Token (Recover Expression)

data UnaryOperator
  -- `!`
  = Not
  -- `-`
  | Negative
  -- `+`
  | Positive

data BinaryOperator
  -- `+`
  = Add
  -- `-`
  | Subtract
  -- `*`
  | Multiply
  -- `/`
  | Divide
  -- `%`
  | Remainder
  -- `^`
  | Exponent
  -- `==`
  | Equals
  -- `!=`
  | NotEquals
  -- `<`
  | LessThan
  -- `<=`
  | LessThanOrEqual
  -- `>`
  | GreaterThan
  -- `>=`
  | GreaterThanOrEqual
  -- `&&`
  --
  -- NOTE: `And` and `Or` will conditionally evaluate the second argument. Don’t assume the second
  -- argument evaluates like other binary operators.
  | And
  -- `||`
  --
  -- NOTE: `And` and `Or` will conditionally evaluate the second argument. Don’t assume the second
  -- argument evaluates like other binary operators.
  | Or

-- `if E { ... }`
data ConditionalExpressionIf =
  ConditionalExpressionIf
    Token
    (Recover Expression)
    Block
    (Maybe (Recover ConditionalExpressionElse))

data ConditionalExpressionElse
  -- `else { ... }`
  = ConditionalExpressionElse Token Block
  -- `else if E { ... }`
  | ConditionalExpressionElseIf Token ConditionalExpressionIf

-- Some extra syntax of an expression. We keep this as a separate data type to match our
-- parser implementation.
data ExpressionExtra
  -- `E + E`
  = BinaryExpressionExtra BinaryExpressionOperation [Recover BinaryExpressionOperation]
  -- `E.p`
  | PropertyExpressionExtra Token (Recover Name)
  -- `f(...)`
  | CallExpressionExtra Token (CommaList Expression) (Recover Token)

-- `+ E`
--
-- We implement binary expressions in such a way that the left-hand side will always exist even when
-- the right-hand side may not. By parsing binary expressions in this way we guarantee that when
-- turning an expression into a tokens list the list will _never_ be empty.
data BinaryExpressionOperation = BinaryExpressionOperation BinaryOperator Token (Recover Expression)

-- The left hand side of a binding statement. Takes a value and deconstructs it into the parts that
-- make it up. Binding those parts to variable names in scope.
data Pattern
  -- ```
  -- C
  -- ```
  = ConstantPattern Constant

  -- ```
  -- x
  -- ```
  | VariablePattern Name

  -- ```
  -- _
  -- ```
  | HolePattern Token

  -- ```
  -- {p: P, ...}
  -- ```
  | ObjectPattern
      Token
      (CommaList ObjectPatternProperty)
      (Maybe (Recover ObjectPatternExtension))
      (Recover Token)

  -- ```
  -- (P)
  -- ```
  --
  -- A pattern wrapped in parentheses.
  | WrappedPattern
      Token
      (Recover Pattern)
      (Recover Token)

-- `p: P`
--
-- A single object property.
data ObjectPatternProperty =
  ObjectPatternProperty Name (Maybe (Recover ObjectPatternPropertyValue))

-- `: P`
--
-- The value of a single object property.
data ObjectPatternPropertyValue = ObjectPatternPropertyValue Token (Recover Pattern)

-- `| P`
--
-- An extension operation on an object.
data ObjectPatternExtension = ObjectPatternExtension Token (Recover Pattern)

-- Statically describes properties of a value at runtime. Through extensive domain modeling with
-- types a user can reduce the possibilities for bugs in their systems.
data Type
  -- `x`
  = VariableType Name

  -- `!`
  --
  -- NOTE: Are we sure we want this as the syntax for bottom types?
  | BottomType Token

  -- ```
  -- fun(...) -> T
  -- fun<T>(...) -> U
  -- ```
  --
  -- A quantifier list included with the function type is the same as a quantified function type.
  -- So `fun<T>() -> void` is the same as `<T> fun() -> void`.
  | FunctionType
      Token                            -- `fun`
      (Maybe (Recover QuantifierList)) -- Type parameters
      (Recover Token)                  -- `(`
      (CommaList Type)                 -- Parameters
      (Recover Token)                  -- `)`
      (Recover Token)                  -- `->`
      (Recover Type)                   -- Return

  -- ```
  -- {p: T, ...}
  -- ```
  | ObjectType
      Token
      (CommaList ObjectTypeProperty)
      (Maybe (Recover ObjectTypeExtension))
      (Recover Token)

  -- ```
  -- <x> T
  -- <x: T> U
  -- <x = T> U
  -- ```
  | QuantifiedType QuantifierList (Recover Type)

  -- ```
  -- (T)
  -- ```
  --
  -- A type wrapped in parentheses.
  | WrappedType
      Token
      (Recover Type)
      (Recover Token)

-- `p: T`
--
-- A single object property.
data ObjectTypeProperty = ObjectTypeProperty Name (Recover Token) (Recover Type)

-- `| T`
--
-- An extension operation on an object.
data ObjectTypeExtension = ObjectTypeExtension Token (Recover Type)

-- ```
-- <x>
-- <x: T>
-- <x = T>
-- ```
data QuantifierList = QuantifierList Token (CommaList Quantifier) (Recover Token)

-- ```
-- x
-- x: T
-- x = T
-- ```
data Quantifier = Quantifier Name (Maybe (Recover QuantifierBound))

-- ```
-- : T
-- = T
-- ```
data QuantifierBound = QuantifierBound QuantifierBoundKind Token (Recover Type)

-- `:` or `=`
data QuantifierBoundKind = Rigid | Flexible

-- `: T`
data TypeAnnotation = TypeAnnotation Token (Recover Type)

-- Get all the tokens that make up a module. Printing these tokens to source should result in the
-- exact source code of the document we parsed to produce this module.
moduleTokens :: Module -> ([Token], EndToken)
moduleTokens (Module statements end) =
  ( appEndo (mconcat (map (recoverTokens statementTokens) statements)) []
  , end
  )

-- Get the tokens for an expression. Printing these tokens to source should result in the exact
-- source code of the document we parsed to produce this expression.
expressionTokens :: Expression -> [Token]
expressionTokens e = appEndo (expressionTokensM e) []

-- Rebuild the source code that the module was parsed from. Does not return the exact same text
-- reference but rather a rebuilt text document.
moduleSource :: Module -> Text.Builder
moduleSource m =
  let (tokens, endToken) = moduleTokens m in
    mconcat (map tokenSource tokens) <> endTokenSource endToken

-- Get the source code for this statement while trimming all whitespace from the beginning
-- and ending.
statementTrimmedSource :: Recover Statement -> Text.Builder
statementTrimmedSource statement =
  sourceStart (appEndo ((recoverTokens statementTokens) statement) [])
  where
    trimStart t =
      let trivia = dropWhile isTriviaWhitespace (tokenLeadingTrivia t) in
        t { tokenLeadingTrivia = trivia }

    trimEnd t =
      let trivia = reverse (dropWhile isTriviaWhitespace (reverse (tokenTrailingTrivia t))) in
        t { tokenTrailingTrivia = trivia }

    sourceStart [] = source []
    sourceStart (t : ts) = source (trimStart t : ts)

    source [] = mempty
    source [t] = tokenSource (trimEnd t)
    source (t : ts) = tokenSource t <> source ts

-- Gets the leading trivia for a `Recover Statement`.
--
-- If the `Recover` is `Fatal` with no skipped tokens then we return an empty list.
recoverStatementLeadingTrivia :: Recover Statement -> [Trivia]
recoverStatementLeadingTrivia = recoverLeadingTrivia (tokenLeadingTrivia . statementFirstToken)

-- Gets the leading trivia for a CST node wrapped in `Recover`. If the node is `Fatal` with no
-- skipped tokens then we return an empty list.
recoverLeadingTrivia :: (a -> [Trivia]) -> Recover a -> [Trivia]
recoverLeadingTrivia f (Ok a) = f a
recoverLeadingTrivia f (Recover [] _ a) = f a
recoverLeadingTrivia _ (Recover (t : _) _ _) = tokenLeadingTrivia t
recoverLeadingTrivia _ (Fatal [] _) = []
recoverLeadingTrivia _ (Fatal (t : _) _) = tokenLeadingTrivia t

-- Gets the first token of a statement.
statementFirstToken :: Statement -> Token
statementFirstToken (ExpressionStatement e _) = expressionFirstToken e
statementFirstToken (BindingStatement t _ _ _ _ _) = t
statementFirstToken (ReturnStatement t _ _) = t
statementFirstToken (BreakStatement t _ _) = t
statementFirstToken (EmptyStatement t) = t
statementFirstToken (Declaration (FunctionDeclaration t _ _)) = t

-- Gets the first token of an expression.
expressionFirstToken :: Expression -> Token
expressionFirstToken (ConstantExpression (BooleanConstant _ t)) = t
expressionFirstToken (VariableExpression (Name _ t)) = t
expressionFirstToken (FunctionExpression t _) = t
expressionFirstToken (ObjectExpression t _ _ _) = t
expressionFirstToken (UnaryExpression _ t _) = t
expressionFirstToken (ConditionalExpression (ConditionalExpressionIf t _ _ _)) = t
expressionFirstToken (BlockExpression t _) = t
expressionFirstToken (LoopExpression t _) = t
expressionFirstToken (WrappedExpression t _ _ _) = t
expressionFirstToken (ExpressionExtra e _) = expressionFirstToken e

-- Use a “difference list” trick to more efficiently build token lists.
type Tokens = Endo [Token]

-- Singleton token.
singletonToken :: Token -> Tokens
singletonToken t = Endo (t :)

-- Get tokens from a type wrapped in `Maybe`.
maybeTokens :: (a -> Tokens) -> Maybe a -> Tokens
maybeTokens tokens (Just a) = tokens a
maybeTokens _ Nothing = mempty

-- Get tokens from a name.
nameTokens :: Name -> Tokens
nameTokens (Name _ t) = singletonToken t

-- Get tokens from a type wrapped in `Recover`.
recoverTokens :: (a -> Tokens) -> Recover a -> Tokens
recoverTokens tokens (Ok a) = tokens a
recoverTokens tokens (Recover ts _ a) = Endo (ts ++) <> tokens a
recoverTokens _ (Fatal ts _) = Endo (ts ++)

-- Get tokens from a type wrapped in `CommaList`.
commaListTokens :: (a -> Tokens) -> CommaList a -> Tokens
commaListTokens tokens (CommaList as an) =
  mconcat (map (\(a, c) -> recoverTokens tokens a <> recoverTokens singletonToken c) as)
    <> maybeTokens (recoverTokens tokens) an

-- Get tokens from a statement.
statementTokens :: Statement -> Tokens
statementTokens (ExpressionStatement e t) =
  expressionTokensM e <> maybeTokens (recoverTokens singletonToken) t
statementTokens (BindingStatement t1 p a t2 e t3) =
  singletonToken t1
    <> recoverTokens patternTokens p
    <> maybeTokens (recoverTokens typeAnnotationTokens) a
    <> recoverTokens singletonToken t2
    <> recoverTokens expressionTokensM e
    <> maybeTokens (recoverTokens singletonToken) t3
statementTokens (ReturnStatement t1 e t2) =
  singletonToken t1
    <> maybeTokens (recoverTokens expressionTokensM) e
    <> maybeTokens (recoverTokens singletonToken) t2
statementTokens (BreakStatement t1 e t2) =
  singletonToken t1
    <> maybeTokens (recoverTokens expressionTokensM) e
    <> maybeTokens (recoverTokens singletonToken) t2
statementTokens (EmptyStatement t) = singletonToken t
statementTokens (Declaration d) =
  declarationTokens d

-- Get tokens from a declaration.
declarationTokens :: Declaration -> Tokens
declarationTokens (FunctionDeclaration t n f) =
  singletonToken t <> recoverTokens nameTokens n <> functionTokens f

functionTokens :: Function -> Tokens
functionTokens (Function qs t1 ps t2 r b) =
  maybeTokens (recoverTokens quantifierListTokens) qs
    <> recoverTokens singletonToken t1
    <> commaListTokens functionParameterTokens ps
    <> recoverTokens singletonToken t2
    <> maybeTokens (recoverTokens functionReturnTokens) r
    <> blockTokens b

functionParameterTokens :: FunctionParameter -> Tokens
functionParameterTokens (FunctionParameter p a) =
  patternTokens p <> maybeTokens (recoverTokens typeAnnotationTokens) a

functionReturnTokens :: FunctionReturn -> Tokens
functionReturnTokens (FunctionReturn t a) =
  singletonToken t <> recoverTokens typeTokens a

-- Get tokens from a block.
blockTokens :: Block -> Tokens
blockTokens (Block t1 ss t2) =
  recoverTokens singletonToken t1
    <> mconcat (map (recoverTokens statementTokens) ss)
    <> recoverTokens singletonToken t2

-- Get tokens from a constant.
constantTokens :: Constant -> Tokens
constantTokens (BooleanConstant _ t) = singletonToken t

-- Get tokens from an expression.
--
-- The `M` at the end stands for “monoid”.
expressionTokensM :: Expression -> Tokens
expressionTokensM (ConstantExpression constant) = constantTokens constant

expressionTokensM (VariableExpression name) = nameTokens name

expressionTokensM (FunctionExpression t f) =
  singletonToken t <> functionTokens f

expressionTokensM (ObjectExpression t1 ps ext t2) =
  singletonToken t1
    <> commaListTokens propertyTokens ps
    <> maybeTokens (recoverTokens extensionTokens) ext
    <> recoverTokens singletonToken t2
  where
    propertyTokens (ObjectExpressionProperty n v) =
      nameTokens n <> maybeTokens (recoverTokens propertyValueTokens) v

    propertyValueTokens (ObjectExpressionPropertyValue t3 e) =
      singletonToken t3 <> recoverTokens expressionTokensM e

    extensionTokens (ObjectExpressionExtension t3 e) =
      singletonToken t3 <> recoverTokens expressionTokensM e

expressionTokensM (UnaryExpression _ t e) = singletonToken t <> recoverTokens expressionTokensM e

expressionTokensM (ConditionalExpression i') =
  ifTokens i'
  where
    ifTokens (ConditionalExpressionIf t x b e) =
      singletonToken t
        <> recoverTokens expressionTokensM x
        <> blockTokens b
        <> maybeTokens (recoverTokens elseTokens) e

    elseTokens (ConditionalExpressionElse t b) = singletonToken t <> blockTokens b
    elseTokens (ConditionalExpressionElseIf t i) = singletonToken t <> ifTokens i

expressionTokensM (BlockExpression t b) = singletonToken t <> blockTokens b

expressionTokensM (LoopExpression t b) = singletonToken t <> blockTokens b

expressionTokensM (WrappedExpression t1 e a t2) =
  singletonToken t1
    <> recoverTokens expressionTokensM e
    <> maybeTokens (recoverTokens typeAnnotationTokens) a
    <> recoverTokens singletonToken t2

expressionTokensM (ExpressionExtra e ext) =
  expressionTokensM e <> recoverTokens extraTokens ext
  where
    extraTokens (BinaryExpressionExtra op ops) =
      binaryOperation op <> mconcat (map (recoverTokens binaryOperation) ops)
    extraTokens (PropertyExpressionExtra t l) =
      singletonToken t <> recoverTokens nameTokens l
    extraTokens (CallExpressionExtra t1 args t2) =
      singletonToken t1 <> commaListTokens expressionTokensM args <> recoverTokens singletonToken t2

    binaryOperation (BinaryExpressionOperation _ t e2) =
      singletonToken t <> recoverTokens expressionTokensM e2

-- Get tokens from a pattern.
patternTokens :: Pattern -> Tokens
patternTokens (ConstantPattern constant) = constantTokens constant
patternTokens (VariablePattern name) = nameTokens name
patternTokens (HolePattern token) = singletonToken token

patternTokens (ObjectPattern t1 ps ext t2) =
  singletonToken t1
    <> commaListTokens propertyTokens ps
    <> maybeTokens (recoverTokens extensionTokens) ext
    <> recoverTokens singletonToken t2
  where
    propertyTokens (ObjectPatternProperty n v) =
      nameTokens n <> maybeTokens (recoverTokens propertyValueTokens) v

    propertyValueTokens (ObjectPatternPropertyValue t3 e) =
      singletonToken t3 <> recoverTokens patternTokens e

    extensionTokens (ObjectPatternExtension t3 e) =
      singletonToken t3 <> recoverTokens patternTokens e

patternTokens (WrappedPattern t1 p t2) =
  singletonToken t1
    <> recoverTokens patternTokens p
    <> recoverTokens singletonToken t2

typeTokens :: Type -> Tokens
typeTokens (VariableType name) = nameTokens name
typeTokens (BottomType t) = singletonToken t

typeTokens (FunctionType t1 qs t2 ps t3 t4 r) =
  singletonToken t1
    <> maybeTokens (recoverTokens quantifierListTokens) qs
    <> recoverTokens singletonToken t2
    <> commaListTokens typeTokens ps
    <> recoverTokens singletonToken t3
    <> recoverTokens singletonToken t4
    <> recoverTokens typeTokens r

typeTokens (ObjectType t1 ps ext t2) =
  singletonToken t1
    <> commaListTokens propertyTokens ps
    <> maybeTokens (recoverTokens extensionTokens) ext
    <> recoverTokens singletonToken t2
  where
    propertyTokens (ObjectTypeProperty n t a) =
      nameTokens n <> recoverTokens singletonToken t <> recoverTokens typeTokens a

    extensionTokens (ObjectTypeExtension t3 e) =
      singletonToken t3 <> recoverTokens typeTokens e

typeTokens (QuantifiedType qs t) = quantifierListTokens qs <> recoverTokens typeTokens t

typeTokens (WrappedType t1 a t2) =
  singletonToken t1
    <> recoverTokens typeTokens a
    <> recoverTokens singletonToken t2

quantifierListTokens :: QuantifierList -> Tokens
quantifierListTokens (QuantifierList t1 qs t2) =
  singletonToken t1 <> commaListTokens quantifierTokens qs <> recoverTokens singletonToken t2
  where
    quantifierTokens (Quantifier n b) = nameTokens n <> maybeTokens (recoverTokens boundTokens) b
    boundTokens (QuantifierBound _ t a) = singletonToken t <> recoverTokens typeTokens a

typeAnnotationTokens :: TypeAnnotation -> Tokens
typeAnnotationTokens (TypeAnnotation t1 t2) =
  singletonToken t1 <> recoverTokens typeTokens t2
