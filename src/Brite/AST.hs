{-# LANGUAGE OverloadedStrings #-}

module Brite.AST
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
  , VariantExpressionElements(..)
  , UnaryOperator(..)
  , BinaryExpressionExtra(..)
  , BinaryOperator(..)
  , ConditionalExpressionIf(..)
  , ConditionalExpressionElse(..)
  , MatchExpressionCase(..)
  , ExpressionExtra(..)
  , Pattern(..)
  , ObjectPatternProperty(..)
  , ObjectPatternPropertyValue(..)
  , ObjectPatternExtension(..)
  , VariantPattern(..)
  , VariantPatternElements(..)
  , Type(..)
  , ObjectTypeProperty(..)
  , ObjectTypeExtension(..)
  , VariantType(..)
  , VariantTypeElements(..)
  , QuantifierList(..)
  , Quantifier(..)
  , QuantifierBound(..)
  , QuantifierBoundKind(..)
  , TypeAnnotation(..)
  , moduleTokens
  , debugModule
  , showDebugExpression
  ) where

import Brite.Parser.Framework (Recover(..), CommaList(..), commaListItems)
import Brite.Source
import Data.Monoid (Endo(..))
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

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

  -- Some declaration which is not order dependent unlike all statements.
  | Declaration Declaration

-- Convenience type alias for an optional semicolon token.
type Semicolon = Maybe (Recover Token)

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
  -- case V
  -- case V(E)
  -- ```
  --
  -- Data with an associated label.
  | VariantExpression Token (Recover Name) (Maybe (Recover VariantExpressionElements))

  -- ```
  -- !E
  -- -E
  -- +E
  -- ``
  --
  -- An operation on a single expression.
  | UnaryExpression UnaryOperator Token (Recover Expression)

  -- ```
  -- E + E
  -- E - E
  -- E * E
  -- E / E
  -- ```
  --
  -- An operation on two expressions.
  --
  -- Unlike `ExpressionExtra` in that the first expression must be `Recover` since binary
  -- expressions are parsed differently.
  | BinaryExpression (Recover Expression) (Recover BinaryExpressionExtra)

  -- ```
  -- if E { ... }
  -- if E { ... } else { ... }
  -- if E { ... } else if E { ... } else { ... }
  -- ```
  --
  -- Conditionally executes some code.
  | ConditionalExpression ConditionalExpressionIf

  -- ```
  -- match E { P -> { ... } }
  -- ```
  --
  -- Matches an expression against the first valid pattern.
  | MatchExpression
      Token
      (Recover Expression)
      (Recover Token)
      [Recover MatchExpressionCase]
      (Recover Token)

  -- ```
  -- do { ... }
  -- ``
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

-- `(...)`
--
-- The elements of a variant.
data VariantExpressionElements =
  VariantExpressionElements Token (CommaList Expression) (Recover Token)

data UnaryOperator
  -- `!`
  = Not
  -- `-`
  | Negative
  -- `+`
  | Positive

data BinaryExpressionExtra =
  BinaryExpressionExtra BinaryOperator Token (Recover Expression)

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

-- `P -> { ... }`
data MatchExpressionCase = MatchExpressionCase Pattern (Recover Token) Block

-- Some extra syntax of an expression. We keep this as a separate data type to match our
-- parser implementation.
data ExpressionExtra
  -- `E.p`
  = PropertyExpressionExtra Token (Recover Name)
  -- `f(...)`
  | CallExpressionExtra Token (CommaList Expression) (Recover Token)

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
  -- case V
  -- case V(P)
  -- case V | case W
  -- ```
  | VariantUnionPattern
      (Maybe Token)
      VariantPattern
      [Recover (Token, VariantPattern)]

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

-- ```
-- case V
-- case V(P)
-- ```
data VariantPattern =
  VariantPattern (Recover Token) (Recover Name) (Maybe (Recover VariantPatternElements))

-- `(...)`
--
-- The elements of a variant.
data VariantPatternElements =
  VariantPatternElements Token (CommaList Pattern) (Recover Token)

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
  -- case V
  -- case V(T)
  -- case V | case W
  -- case V | case W | else T
  -- ```
  --
  -- A variant may form a union with any other type. However, only certain type kinds are acceptable
  -- as the extension of a variant union type.
  --
  -- TODO: Extension
  | VariantUnionType
      (Maybe Token)
      VariantType
      [Recover (Token, VariantType)]

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
-- case V
-- case V(T)
-- ```
data VariantType =
  VariantType (Recover Token) (Recover Name) (Maybe (Recover VariantTypeElements))

-- `(...)`
--
-- The elements of a variant.
data VariantTypeElements =
  VariantTypeElements Token (CommaList Type) (Recover Token)

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
  expressionTokens e <> maybeTokens (recoverTokens singletonToken) t
statementTokens (BindingStatement t1 p a t2 e t3) =
  singletonToken t1
    <> recoverTokens patternTokens p
    <> maybeTokens (recoverTokens typeAnnotationTokens) a
    <> recoverTokens singletonToken t2
    <> recoverTokens expressionTokens e
    <> maybeTokens (recoverTokens singletonToken) t3
statementTokens (ReturnStatement t1 e t2) =
  singletonToken t1
    <> maybeTokens (recoverTokens expressionTokens) e
    <> maybeTokens (recoverTokens singletonToken) t2
statementTokens (BreakStatement t1 e t2) =
  singletonToken t1
    <> maybeTokens (recoverTokens expressionTokens) e
    <> maybeTokens (recoverTokens singletonToken) t2
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
expressionTokens :: Expression -> Tokens
expressionTokens (ConstantExpression constant) = constantTokens constant

expressionTokens (VariableExpression name) = nameTokens name

expressionTokens (FunctionExpression t f) =
  singletonToken t <> functionTokens f

expressionTokens (ObjectExpression t1 ps ext t2) =
  singletonToken t1
    <> commaListTokens propertyTokens ps
    <> maybeTokens (recoverTokens extensionTokens) ext
    <> recoverTokens singletonToken t2
  where
    propertyTokens (ObjectExpressionProperty n v) =
      nameTokens n <> maybeTokens (recoverTokens propertyValueTokens) v

    propertyValueTokens (ObjectExpressionPropertyValue t3 e) =
      singletonToken t3 <> recoverTokens expressionTokens e

    extensionTokens (ObjectExpressionExtension t3 e) =
      singletonToken t3 <> recoverTokens expressionTokens e

expressionTokens (VariantExpression t1 n els) =
  singletonToken t1
    <> recoverTokens nameTokens n
    <> maybeTokens (recoverTokens elementTokens) els
  where
    elementTokens (VariantExpressionElements t2 es t3) =
      singletonToken t2 <> commaListTokens expressionTokens es <> recoverTokens singletonToken t3

expressionTokens (UnaryExpression _ t e) = singletonToken t <> recoverTokens expressionTokens e

expressionTokens (BinaryExpression e1 ext) =
  recoverTokens expressionTokens e1 <> recoverTokens extraTokens ext
  where
    extraTokens (BinaryExpressionExtra _ t e2) =
      singletonToken t <> recoverTokens expressionTokens e2

expressionTokens (ConditionalExpression i') =
  ifTokens i'
  where
    ifTokens (ConditionalExpressionIf t x b e) =
      singletonToken t
        <> recoverTokens expressionTokens x
        <> blockTokens b
        <> maybeTokens (recoverTokens elseTokens) e

    elseTokens (ConditionalExpressionElse t b) = singletonToken t <> blockTokens b
    elseTokens (ConditionalExpressionElseIf t i) = singletonToken t <> ifTokens i

expressionTokens (MatchExpression t1 e t2 cs t3) =
  singletonToken t1
    <> recoverTokens expressionTokens e
    <> recoverTokens singletonToken t2
    <> mconcat (map (recoverTokens caseTokens) cs)
    <> recoverTokens singletonToken t3
  where
    caseTokens (MatchExpressionCase p t4 b) =
      patternTokens p
        <> recoverTokens singletonToken t4
        <> blockTokens b

expressionTokens (BlockExpression t b) = singletonToken t <> blockTokens b

expressionTokens (LoopExpression t b) = singletonToken t <> blockTokens b

expressionTokens (WrappedExpression t1 e a t2) =
  singletonToken t1
    <> recoverTokens expressionTokens e
    <> maybeTokens (recoverTokens typeAnnotationTokens) a
    <> recoverTokens singletonToken t2

expressionTokens (ExpressionExtra e ext) =
  expressionTokens e <> recoverTokens extraTokens ext
  where
    extraTokens (PropertyExpressionExtra t l) =
      singletonToken t <> recoverTokens nameTokens l
    extraTokens (CallExpressionExtra t1 args t2) =
      singletonToken t1 <> commaListTokens expressionTokens args <> recoverTokens singletonToken t2

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

patternTokens (VariantUnionPattern t1 v1 vs) =
  maybeTokens singletonToken t1
    <> variantTokens v1
    <> mconcat (map (recoverTokens (\(t2, v) -> singletonToken t2 <> variantTokens v)) vs)
  where
    variantTokens (VariantPattern t2 n els) =
      recoverTokens singletonToken t2
        <> recoverTokens nameTokens n
        <> maybeTokens (recoverTokens elementTokens) els

    elementTokens (VariantPatternElements t2 es t3) =
      singletonToken t2 <> commaListTokens patternTokens es <> recoverTokens singletonToken t3

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

typeTokens (VariantUnionType t1 v1 vs) =
  maybeTokens singletonToken t1
    <> variantTokens v1
    <> mconcat (map (recoverTokens (\(t2, v) -> singletonToken t2 <> variantTokens v)) vs)
  where
    variantTokens (VariantType t2 n els) =
      recoverTokens singletonToken t2
        <> recoverTokens nameTokens n
        <> maybeTokens (recoverTokens elementTokens) els

    elementTokens (VariantTypeElements t2 es t3) =
      singletonToken t2 <> commaListTokens typeTokens es <> recoverTokens singletonToken t3

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

-- Prints an expression in an S-expression form for debugging. This abbreviated format should make
-- it easier to see the structure of the AST.
showDebugExpression :: Expression -> String
showDebugExpression = L.unpack . B.toLazyText . debugExpression ""

-- Debug a module in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST. Each statement in the module is on its own line.
debugModule :: Module -> B.Builder
debugModule (Module [] _) = B.fromText "empty\n"
debugModule (Module statements _) =
  mconcat $ map (\s -> debugRecover (debugStatement "") s <> B.singleton '\n') statements

debugRecover :: (a -> B.Builder) -> Recover a -> B.Builder
debugRecover debug (Ok a) = debug a
debugRecover debug (Recover _ _ a) = debug a
debugRecover _ (Fatal _ _) = B.fromText "err"

-- Debug a name in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST.
debugName :: Name -> B.Builder
debugName (Name identifier _) =
  B.fromText "(name `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"

-- Debug a statement in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugStatement :: B.Builder -> Statement -> B.Builder
debugStatement indentation (ExpressionStatement expression _) =
  debugExpression indentation expression
debugStatement indentation (BindingStatement _ pattern Nothing _ expression _) =
  B.fromText "(bind "
    <> debugRecover (debugPattern indentation) pattern
    <> B.singleton ' '
    <> debugRecover (debugExpression indentation) expression
    <> B.fromText ")"
debugStatement indentation (BindingStatement _ pattern (Just type_) _ expression _) =
  B.fromText "(bind "
    <> debugRecover (debugPattern indentation) pattern
    <> B.fromText " (type "
    <> debugRecover (debugTypeAnnotation indentation) type_
    <> B.fromText ") "
    <> debugRecover (debugExpression indentation) expression
    <> B.fromText ")"
debugStatement _ (ReturnStatement _ Nothing _) = B.fromText "return"
debugStatement indentation (ReturnStatement _ (Just expression) _) =
  B.fromText "(return " <> debugRecover (debugExpression indentation) expression <> B.singleton ')'
debugStatement _ (BreakStatement _ Nothing _) = B.fromText "break"
debugStatement indentation (BreakStatement _ (Just expression) _) =
  B.fromText "(break " <> debugRecover (debugExpression indentation) expression <> B.singleton ')'
debugStatement indentation (Declaration declaration) =
  debugDeclaration indentation declaration

-- Debug a declaration in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugDeclaration :: B.Builder -> Declaration -> B.Builder
debugDeclaration indentation (FunctionDeclaration _ name function) =
  debugFunction indentation (Just name) function

debugFunction :: B.Builder -> Maybe (Recover Name) -> Function -> B.Builder
debugFunction indentation name (Function qs _ params _ return_ block) =
  B.fromText "(fun"
    <> maybe mempty (debugNewline (debugRecover debugName)) name
    <> debugRecoverMaybe (debugQuantifierList newIndentation) qs
    <> mconcat (map (debugNewline (debugParamWrapper (debugRecover debugParam))) (commaListItems params))
    <> maybe mempty (debugNewline (debugReturnWrapper (debugRecover debugReturn))) return_
    <> debugNewline (debugBlock newIndentation) block
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugNewline debug a = B.singleton '\n' <> newIndentation <> debug a

    debugRecoverMaybe _ Nothing = mempty
    debugRecoverMaybe _ (Just (Fatal _ _)) = mempty
    debugRecoverMaybe debug (Just (Recover _ _ a)) = debug a
    debugRecoverMaybe debug (Just (Ok a)) = debug a

    debugParamWrapper debug a =
      B.fromText "(param " <> debug a <> B.singleton ')'

    debugParam (FunctionParameter pattern Nothing) = debugPattern newIndentation pattern
    debugParam (FunctionParameter pattern (Just typeAnnotation)) =
      debugPattern newIndentation pattern
        <> B.fromText " (type "
        <> debugRecover (debugTypeAnnotation newIndentation) typeAnnotation
        <> B.singleton ')'

    debugReturnWrapper debug a =
      B.fromText "(ret " <> debug a <> B.singleton ')'

    debugReturn (FunctionReturn _ type_) =
      B.fromText "(type "
        <> debugRecover (debugType newIndentation) type_
        <> B.singleton ')'

-- Debug a block in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugBlock :: B.Builder -> Block -> B.Builder
debugBlock _ (Block _ [] _) = B.fromText "block"
debugBlock indentation block =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(block"
    <> mconcat (map (\s ->
        B.singleton '\n'
          <> newIndentation
          <> debugRecover (debugStatement newIndentation) s) (blockStatements block))
    <> B.fromText ")"

-- Debug a constant in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugConstant :: Constant -> B.Builder
debugConstant (BooleanConstant True _) =
  B.fromText "(bool true)"
debugConstant (BooleanConstant False _) =
  B.fromText "(bool false)"

-- Debug an expression in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugExpression :: B.Builder -> Expression -> B.Builder
debugExpression _ (ConstantExpression constant) = debugConstant constant

debugExpression _ (VariableExpression (Name identifier _)) =
  B.fromText "(var `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"

debugExpression indentation (FunctionExpression _ function) =
  debugFunction indentation Nothing function

debugExpression _ (ObjectExpression _ (CommaList [] Nothing) Nothing _) =
  B.fromText "object"

debugExpression indentation (ObjectExpression _ properties extension _) =
  B.fromText "(object"
    <> mconcat (map (debugNewline (debugPropertyWrapper (debugRecover debugProperty))) (commaListItems properties))
    <> maybe mempty (debugNewline (debugRecover debugExtension)) extension
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugNewline debug a = B.singleton '\n' <> newIndentation <> debug a

    debugPropertyWrapper debug a =
      B.fromText "(prop " <> debug a <> B.singleton ')'

    debugProperty (ObjectExpressionProperty label Nothing) = debugName label
    debugProperty (ObjectExpressionProperty label (Just value)) =
      debugName label <> B.singleton ' ' <> debugRecover debugPropertyValue value

    debugPropertyValue (ObjectExpressionPropertyValue _ expression) =
      debugRecover (debugExpression newIndentation) expression

    debugExtension (ObjectExpressionExtension _ expression) =
      debugRecover (debugExpression newIndentation) expression

debugExpression indentation (VariantExpression _ label elements) =
  B.fromText "(variant "
    <> debugRecover debugName label
    <> maybe mempty (debugRecover debugElements) elements
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugNewline debug a = B.singleton '\n' <> newIndentation <> debug a

    debugElements (VariantExpressionElements _ patterns _) =
      mconcat $
        map (debugNewline (debugRecover (debugExpression indentation))) $
          commaListItems patterns

debugExpression indentation (UnaryExpression operator _ expression) =
  B.singleton '('
    <> B.fromText operatorDescription
    <> B.singleton ' '
    <> debugRecover (debugExpression indentation) expression
    <> B.singleton ')'
  where
    operatorDescription =
      case operator of
        Not -> "not"
        Positive -> "pos"
        Negative -> "neg"

debugExpression indentation (BinaryExpression left extra) =
  debugRecover debugExtra extra
  where
    debugExtra (BinaryExpressionExtra operator _ right) =
      B.singleton '('
        <> B.fromText operatorDescription
        <> B.singleton ' '
        <> debugRecover (debugExpression indentation) left
        <> B.singleton ' '
        <> debugRecover (debugExpression indentation) right
        <> B.singleton ')'
      where
        operatorDescription =
          case operator of
            Add -> "add"
            Subtract -> "sub"
            Multiply -> "mul"
            Divide -> "div"
            Remainder -> "rem"
            Exponent -> "pow"
            Equals -> "eq"
            NotEquals -> "neq"
            LessThan -> "lt"
            LessThanOrEqual -> "lte"
            GreaterThan -> "gt"
            GreaterThanOrEqual -> "gte"
            And -> "and"
            Or -> "or"

debugExpression indentation' (ConditionalExpression if') =
  debugIf indentation' if'
  where
    debugIf indentation (ConditionalExpressionIf  _ test consequent Nothing) =
      let newIndentation = indentation <> B.fromText "  " in
        B.fromText "(if"
          <> B.singleton '\n' <> newIndentation
          <> debugRecover (debugExpression newIndentation) test
          <> B.singleton '\n' <> newIndentation
          <> debugBlock newIndentation consequent
          <> B.singleton ')'
    debugIf indentation (ConditionalExpressionIf  _ test consequent (Just alternate)) =
      let newIndentation = indentation <> B.fromText "  " in
        B.fromText "(if"
          <> B.singleton '\n' <> newIndentation
          <> debugRecover (debugExpression newIndentation) test
          <> B.singleton '\n' <> newIndentation
          <> debugBlock newIndentation consequent
          <> B.singleton '\n' <> newIndentation
          <> debugRecover (debugElse newIndentation) alternate
          <> B.singleton ')'

    debugElse indentation (ConditionalExpressionElse _ block) =
      debugBlock indentation block
    debugElse indentation (ConditionalExpressionElseIf _ if_) =
      debugIf indentation if_

debugExpression indentation (MatchExpression _ expression _ cases _) =
  B.fromText "(match"
    <> debugNewline (debugRecover (debugExpression newIndentation)) expression
    <> mconcat (map (debugNewline (debugCaseWrapper (debugRecover debugCase))) cases)
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugNewline debug a = B.singleton '\n' <> newIndentation <> debug a

    debugCaseWrapper debug a =
      B.fromText "(case " <> debug a <> B.singleton ')'

    debugCase (MatchExpressionCase pattern _ block) =
      debugPattern newIndentation pattern
        <> B.singleton ' '
        <> debugBlock newIndentation block

debugExpression indentation (BlockExpression _ block) =
  B.fromText "(do " <> debugBlock indentation block <> B.singleton ')'

debugExpression indentation (LoopExpression _ block) =
  B.fromText "(loop " <> debugBlock indentation block <> B.singleton ')'

debugExpression indentation (WrappedExpression _ expression Nothing _) =
  B.fromText "(wrap "
    <> debugRecover (debugExpression indentation) expression
    <> B.fromText ")"

debugExpression indentation (WrappedExpression _ expression (Just typeAnnotation) _) =
  B.fromText "(wrap "
    <> debugRecover (debugExpression indentation) expression
    <> B.fromText " (type "
    <> debugRecover (debugTypeAnnotation indentation) typeAnnotation
    <> B.fromText "))"

debugExpression indentation (ExpressionExtra expression extra') =
  case extra' of
    Ok extra -> debugExtra extra
    Recover _ _ extra -> debugExtra extra
    Fatal _ _ -> debugExpression indentation expression
  where
    debugExtra (PropertyExpressionExtra _ label) =
      B.fromText "(prop "
        <> debugExpression indentation expression
        <> B.singleton ' '
        <> debugRecover debugName label
        <> B.singleton ')'

    debugExtra (CallExpressionExtra _ (CommaList [] Nothing) _) =
      B.fromText "(call " <> debugExpression indentation expression <> B.singleton ')'

    debugExtra (CallExpressionExtra _ args _) =
      B.fromText "(call"
        <> debugArg (Ok expression)
        <> mconcat (map debugArg (commaListItems args))
        <> B.singleton ')'
      where
        newIndentation = indentation <> B.fromText "  "
        debugArg arg =
          B.singleton '\n' <> newIndentation
            <> debugRecover (debugExpression newIndentation) arg

-- Debug a pattern in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugPattern :: B.Builder -> Pattern -> B.Builder
debugPattern _ (ConstantPattern constant) = debugConstant constant

debugPattern _ (VariablePattern (Name identifier _)) =
  B.fromText "(var `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"

debugPattern _ (HolePattern _) =
  B.fromText "hole"

debugPattern _ (ObjectPattern _ (CommaList [] Nothing) Nothing _) =
  B.fromText "object"

debugPattern indentation (ObjectPattern _ properties extension _) =
  B.fromText "(object"
    <> mconcat (map (debugNewline (debugPropertyWrapper (debugRecover debugProperty))) (commaListItems properties))
    <> maybe mempty (debugNewline (debugRecover debugExtension)) extension
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugNewline debug a = B.singleton '\n' <> newIndentation <> debug a

    debugPropertyWrapper debug a =
      B.fromText "(prop " <> debug a <> B.singleton ')'

    debugProperty (ObjectPatternProperty label Nothing) = debugName label
    debugProperty (ObjectPatternProperty label (Just value)) =
      debugName label <> B.singleton ' ' <> debugRecover debugPropertyValue value

    debugPropertyValue (ObjectPatternPropertyValue _ pattern) =
      debugRecover (debugPattern newIndentation) pattern

    debugExtension (ObjectPatternExtension _ pattern) =
      debugRecover (debugPattern newIndentation) pattern

debugPattern indentation0 (VariantUnionPattern _ v1 vs) =
  if null vs then
    debugVariant (indentation0 <> B.fromText "  ") v1
  else
    B.fromText "(union"
      <> mconcat
        (map
          (debugNewline
            (indentation0 <> B.fromText "  ")
            (debugRecover (debugVariant (indentation0 <> B.fromText "    "))))
          (Ok v1 : map (fmap snd) vs))
      <> B.singleton ')'
  where
    debugNewline indentation debug a = B.singleton '\n' <> indentation <> debug a

    debugVariant indentation (VariantPattern _ label elements) =
      B.fromText "(variant "
        <> debugRecover debugName label
        <> maybe mempty (debugRecover (debugElements indentation)) elements
        <> B.singleton ')'

    debugElements indentation (VariantPatternElements _ patterns _) =
      mconcat $
        map (debugNewline indentation (debugRecover (debugPattern indentation))) $
          commaListItems patterns

debugPattern indentation (WrappedPattern _ pattern _) =
  B.fromText "(wrap "
    <> debugRecover (debugPattern indentation) pattern
    <> B.fromText ")"

debugType :: B.Builder -> Type -> B.Builder
debugType _ (VariableType (Name identifier _)) =
  B.fromText "(var `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"

debugType _ (BottomType _) = B.fromText "bottom"

debugType indentation (FunctionType _ quantifiers _ params _ _ return_) =
  B.fromText "(fun"
    <> debugRecoverMaybe (debugQuantifierList newIndentation) quantifiers
    <> mconcat (map (debugNewline (debugParamWrapper (debugRecover (debugType newIndentation)))) (commaListItems params))
    <> debugNewline (debugRecover (debugType newIndentation)) return_
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugNewline debug a = B.singleton '\n' <> newIndentation <> debug a

    debugRecoverMaybe _ Nothing = mempty
    debugRecoverMaybe _ (Just (Fatal _ _)) = mempty
    debugRecoverMaybe debug (Just (Recover _ _ a)) = debug a
    debugRecoverMaybe debug (Just (Ok a)) = debug a

    debugParamWrapper debug a =
      B.fromText "(param " <> debug a <> B.singleton ')'

debugType indentation (ObjectType _ properties extension _) =
  B.fromText "(object"
    <> mconcat (map (debugNewline (debugPropertyWrapper (debugRecover debugProperty))) (commaListItems properties))
    <> maybe mempty (debugNewline (debugRecover debugExtension)) extension
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugNewline debug a = B.singleton '\n' <> newIndentation <> debug a

    debugPropertyWrapper debug a =
      B.fromText "(prop " <> debug a <> B.singleton ')'

    debugProperty (ObjectTypeProperty label _ type_) =
      debugName label <> B.singleton ' ' <> debugRecover (debugType newIndentation) type_

    debugExtension (ObjectTypeExtension _ type_) =
      debugRecover (debugType newIndentation) type_

debugType indentation0 (VariantUnionType _ v1 vs) =
  if null vs then
    debugVariant (indentation0 <> B.fromText "  ") v1
  else
    B.fromText "(union"
      <> mconcat
        (map
          (debugNewline
            (indentation0 <> B.fromText "  ")
            (debugRecover (debugVariant (indentation0 <> B.fromText "    "))))
          (Ok v1 : map (fmap snd) vs))
      <> B.singleton ')'
  where
    debugNewline indentation debug a = B.singleton '\n' <> indentation <> debug a

    debugVariant indentation (VariantType _ label elements) =
      B.fromText "(variant "
        <> debugRecover debugName label
        <> maybe mempty (debugRecover (debugElements indentation)) elements
        <> B.singleton ')'

    debugElements indentation (VariantTypeElements _ patterns _) =
      mconcat $
        map (debugNewline indentation (debugRecover (debugType indentation))) $
          commaListItems patterns

debugType indentation (QuantifiedType qs t) =
  B.fromText "(quantify"
    <> debugQuantifierList newIndentation qs
    <> B.singleton '\n' <> newIndentation
    <> debugRecover (debugType newIndentation) t
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "

debugType indentation (WrappedType _ type_ _) =
  B.fromText "(wrap "
    <> debugRecover (debugType indentation) type_
    <> B.fromText ")"

debugQuantifierList :: B.Builder -> QuantifierList -> B.Builder
debugQuantifierList indentation (QuantifierList _ qs _) =
  mconcat $
    map (debugNewline (debugQuantifierWrapper (debugRecover debugQuantifier))) $
      commaListItems qs
  where
    debugNewline debug a = B.singleton '\n' <> indentation <> debug a

    debugQuantifierWrapper debug a =
      B.fromText "(forall " <> debug a <> B.singleton ')'

    debugQuantifier (Quantifier n Nothing) = debugName n
    debugQuantifier (Quantifier n (Just b)) =
      debugName n <> B.singleton ' ' <> debugRecover debugBound b

    debugBound (QuantifierBound Flexible _ t) =
      B.fromText "flex " <> debugRecover (debugType indentation) t
    debugBound (QuantifierBound Rigid _ t) =
      B.fromText "rigid " <> debugRecover (debugType indentation) t

debugTypeAnnotation :: B.Builder -> TypeAnnotation -> B.Builder
debugTypeAnnotation indentation (TypeAnnotation _ t) = debugRecover (debugType indentation) t
