{-# LANGUAGE OverloadedStrings #-}

module Brite.AST
  ( Module(..)
  , Name(..)
  , Recover(..)
  , CommaList(..)
  , commaListItems
  , Statement(..)
  , Block(..)
  , Function(..)
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
  , ExpressionExtra(..)
  , Pattern(..)
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
  -- `E;`
  = ExpressionStatement Expression Semicolon

  -- `let x = E;`
  --
  -- Binds a value to a name in the program.
  | BindingStatement Token (Recover Pattern) (Recover Token) (Recover Expression) Semicolon

  -- `return;`, `return E;`
  --
  -- Immediately returns a value from a function. No other code in the function runs.
  --
  -- We include `return` and `break` statements since Brite’s algebraic effects lend themselves to
  -- imperative code styles.
  | ReturnStatement Token (Maybe (Recover Expression)) Semicolon

  -- `break;`, `break E;`
  --
  -- Immediately breaks out of a loop with a value. No other code in the loop runs.
  --
  -- We don’t yet have labeled break or continue statements which may mostly be emulated by other
  -- means. Strictly speaking, `return` isn’t even necessary. `break` is necessary because we have
  -- loop expressions, but loop expressions aren’t necessary since we have recursion.
  --
  -- We should continue to ask ourselves: do we need the `return` statement or `loop` expressions?
  | BreakStatement Token (Maybe (Recover Expression)) Semicolon

-- Convenience type alias for an optional semicolon token.
type Semicolon = Maybe (Recover Token)

-- A set of statements scoped in a block. Names declared in this block may only be accessed by code
-- within the block.
data Block = Block
  { blockOpen :: Recover Token
  , blockStatements :: [Recover Statement]
  , blockClose :: Recover Token
  }

-- `fun f(...) { ... }`
data Function = Function
  { functionKeyword :: Token
  , functionName :: Maybe (Recover Name)
  , functionParamsOpen :: Recover Token
  , functionParams :: CommaList Pattern
  , functionParamsClose :: Recover Token
  , functionBody :: Block
  }

-- Some constant value in our program.
data Constant
  -- `true`, `false`
  = BooleanConstant Bool Token

-- Some instructions our programming language interprets to return a value and possibly perform
-- some side effects.
data Expression
  -- `C`
  --
  -- Some constant value in the program which never changes.
  = ConstantExpression Constant

  -- `x`
  --
  -- A reference to a variable binding in the program.
  | VariableExpression Name

  -- `fun(...) { ... }`
  --
  -- A block of code which is executed whenever the function is called.
  | FunctionExpression Function

  -- `{p: E, ...}`
  --
  -- A collection of labeled data.
  | ObjectExpression
      Token
      (CommaList ObjectExpressionProperty)
      (Maybe (Recover ObjectExpressionExtension))
      (Recover Token)

  -- `.V`, `.V(E)`
  --
  -- Data with an associated label.
  | VariantExpression Token (Recover Name) (Maybe (Recover VariantExpressionElements))

  -- `!E`, `-E`
  --
  -- An operation on a single expression.
  | UnaryExpression UnaryOperator Token (Recover Expression)

  -- `E + E`, `E - E`, `E * E`, `E / E`
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

  -- `do { ... }`
  --
  -- Introduces a new block scope into the program.
  | BlockExpression Token Block

  -- `loop { ... }`
  --
  -- Keeps repeatedly executing the block until a break statement is encountered. The argument to
  -- the break statement is the value returned by the loop.
  | LoopExpression Token Block

  -- `(E)`
  --
  -- An expression wrapped in parentheses. Useful for changing the precedence of operators.
  | WrappedExpression Token (Recover Expression) (Recover Token)

  -- `E.p`, `E()`
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
  -- `C`
  = ConstantPattern Constant
  -- `x`
  | VariablePattern Name
  -- `_`
  | HolePattern Token

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
statementTokens (BindingStatement t1 p t2 e t3) =
  singletonToken t1
    <> recoverTokens patternTokens p
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

-- Get tokens from a block.
blockTokens :: Block -> Tokens
blockTokens (Block t1 ss t2) =
  recoverTokens singletonToken t1
    <> mconcat (map (recoverTokens statementTokens) ss)
    <> recoverTokens singletonToken t2

-- Get tokens from a function.
functionTokens :: Function -> Tokens
functionTokens (Function t1 n t2 ps t3 b) =
  singletonToken t1
    <> maybeTokens (recoverTokens nameTokens) n
    <> recoverTokens singletonToken t2
    <> commaListTokens patternTokens ps
    <> recoverTokens singletonToken t3
    <> blockTokens b

-- Get tokens from a constant.
constantTokens :: Constant -> Tokens
constantTokens (BooleanConstant _ t) = singletonToken t

-- Get tokens from an expression.
expressionTokens :: Expression -> Tokens
expressionTokens (ConstantExpression constant) = constantTokens constant
expressionTokens (VariableExpression name) = nameTokens name
expressionTokens (FunctionExpression function) = functionTokens function
expressionTokens (ObjectExpression t1 fs ext t2) =
  singletonToken t1
    <> commaListTokens objectExpressionPropertyTokens fs
    <> maybeTokens (recoverTokens objectExpressionExtensionTokens) ext
    <> recoverTokens singletonToken t2
expressionTokens (VariantExpression t n es) =
  singletonToken t
    <> recoverTokens nameTokens n
    <> maybeTokens (recoverTokens variantExpressionElementsTokens) es
expressionTokens (UnaryExpression _ t e) = singletonToken t <> recoverTokens expressionTokens e
expressionTokens (BinaryExpression e ext) =
  recoverTokens expressionTokens e <> recoverTokens binaryExpressionExtraTokens ext
expressionTokens (ConditionalExpression i) = conditionalExpressionIfTokens i
expressionTokens (BlockExpression t b) = singletonToken t <> blockTokens b
expressionTokens (LoopExpression t b) = singletonToken t <> blockTokens b
expressionTokens (WrappedExpression t1 e t2) =
  singletonToken t1 <> recoverTokens expressionTokens e <> recoverTokens singletonToken t2
expressionTokens (ExpressionExtra e ext) =
  expressionTokens e <> recoverTokens expressionExtraTokens ext

objectExpressionPropertyTokens :: ObjectExpressionProperty -> Tokens
objectExpressionPropertyTokens (ObjectExpressionProperty l v) =
  nameTokens l <> maybeTokens (recoverTokens objectExpressionPropertyValueTokens) v

objectExpressionPropertyValueTokens :: ObjectExpressionPropertyValue -> Tokens
objectExpressionPropertyValueTokens (ObjectExpressionPropertyValue t e) =
  singletonToken t <> recoverTokens expressionTokens e

objectExpressionExtensionTokens :: ObjectExpressionExtension -> Tokens
objectExpressionExtensionTokens (ObjectExpressionExtension t e) =
  singletonToken t <> recoverTokens expressionTokens e

variantExpressionElementsTokens :: VariantExpressionElements -> Tokens
variantExpressionElementsTokens (VariantExpressionElements t1 xs t2) =
  singletonToken t1 <> commaListTokens expressionTokens xs <> recoverTokens singletonToken t2

binaryExpressionExtraTokens :: BinaryExpressionExtra -> Tokens
binaryExpressionExtraTokens (BinaryExpressionExtra _ t e) =
  singletonToken t <> recoverTokens expressionTokens e

conditionalExpressionIfTokens :: ConditionalExpressionIf -> Tokens
conditionalExpressionIfTokens (ConditionalExpressionIf t x b e) =
  singletonToken t
    <> recoverTokens expressionTokens x
    <> blockTokens b
    <> maybeTokens (recoverTokens conditionalExpressionElseTokens) e

conditionalExpressionElseTokens :: ConditionalExpressionElse -> Tokens
conditionalExpressionElseTokens (ConditionalExpressionElse t b) = singletonToken t <> blockTokens b
conditionalExpressionElseTokens (ConditionalExpressionElseIf t i) =
  singletonToken t <> conditionalExpressionIfTokens i

expressionExtraTokens :: ExpressionExtra -> Tokens
expressionExtraTokens (PropertyExpressionExtra t l) =
  singletonToken t <> recoverTokens nameTokens l
expressionExtraTokens (CallExpressionExtra t1 args t2) =
  singletonToken t1 <> commaListTokens expressionTokens args <> recoverTokens singletonToken t2

-- Get tokens from a pattern.
patternTokens :: Pattern -> Tokens
patternTokens (ConstantPattern constant) = constantTokens constant
patternTokens (VariablePattern name) = nameTokens name
patternTokens (HolePattern token) = singletonToken token

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

debugMaybe :: (a -> B.Builder) -> Maybe a -> B.Builder
debugMaybe _ Nothing = mempty
debugMaybe debug (Just a) = debug a

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
debugStatement indentation (BindingStatement _ pattern _ expression _) =
  B.fromText "(bind "
    <> debugRecover debugPattern pattern
    <> B.singleton ' '
    <> debugRecover (debugExpression indentation) expression
    <> B.fromText ")"
debugStatement _ (ReturnStatement _ Nothing _) = B.fromText "return"
debugStatement indentation (ReturnStatement _ (Just expression) _) =
  B.fromText "(return " <> debugRecover (debugExpression indentation) expression <> B.singleton ')'
debugStatement _ (BreakStatement _ Nothing _) = B.fromText "break"
debugStatement indentation (BreakStatement _ (Just expression) _) =
  B.fromText "(break " <> debugRecover (debugExpression indentation) expression <> B.singleton ')'

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

-- Debug a function in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugFunction :: B.Builder -> Function -> B.Builder
debugFunction indentation (Function _ name _ params _ block) =
  B.fromText "(fun"
    <> maybe mempty ((B.singleton '\n' <>) . (newIndentation <>) . debugRecover debugName) name
    <> mconcat (map debugParam (commaListItems params))
    <> B.singleton '\n' <> newIndentation
    <> debugBlock newIndentation block
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugParam param = B.singleton '\n' <> newIndentation <> debugRecover debugPattern param

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

debugExpression indentation (FunctionExpression function) =
  debugFunction indentation function

debugExpression _ (ObjectExpression _ (CommaList [] Nothing) Nothing _) =
  B.fromText "object"

debugExpression indentation (ObjectExpression _ properties extension _) =
  B.fromText "(object"
    <> mconcat (map debugRecoverProperty (commaListItems properties))
    <> maybe mempty debugRecoverExtension extension
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "

    debugRecoverProperty (Ok property) = debugProperty property
    debugRecoverProperty (Recover _ _ property) = debugProperty property
    debugRecoverProperty (Fatal _ _) = B.singleton '\n' <> newIndentation <> B.fromText "(prop err)"

    debugProperty (ObjectExpressionProperty label Nothing) =
      B.singleton '\n' <> newIndentation
        <> B.fromText "(prop "
        <> debugName label
        <> B.singleton ')'

    debugProperty (ObjectExpressionProperty label (Just value)) =
      B.singleton '\n' <> newIndentation
        <> B.fromText "(prop "
        <> debugName label
        <> B.singleton ' '
        <> debugRecover debugPropertyValue value
        <> B.singleton ')'

    debugPropertyValue (ObjectExpressionPropertyValue _ value) =
      debugRecover (debugExpression newIndentation) value

    debugRecoverExtension =
      (B.singleton '\n' <>) . (newIndentation <>) . debugRecover debugExtension

    debugExtension (ObjectExpressionExtension _ value) =
      debugRecover (debugExpression newIndentation) value

debugExpression indentation (VariantExpression _ n es') =
  B.fromText "(variant "
    <> debugRecover debugName n
    <> debugMaybe (debugRecover debugElements) es'
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "

    debugElements (VariantExpressionElements _ es _) =
      mconcat $ map
        ((B.singleton '\n' <>) . (newIndentation <>) . debugRecover (debugExpression newIndentation))
        (commaListItems es)

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

debugExpression indentation (BinaryExpression expression extra) =
  debugRecover (debugBinaryExpressionExtra indentation expression) extra

debugExpression indentation (ConditionalExpression if_) =
  debugConditionalExpressionIf indentation if_

debugExpression indentation (BlockExpression _ block) =
  B.fromText "(do " <> debugBlock indentation block <> B.singleton ')'

debugExpression indentation (LoopExpression _ block) =
  B.fromText "(loop " <> debugBlock indentation block <> B.singleton ')'

debugExpression indentation (WrappedExpression _ expression _) =
  B.fromText "(wrap "
    <> debugRecover (debugExpression indentation) expression
    <> B.fromText ")"

debugExpression indentation (ExpressionExtra expression (Ok extra)) =
  debugExpressionExtra indentation expression extra
debugExpression indentation (ExpressionExtra expression (Recover _ _ extra)) =
  debugExpressionExtra indentation expression extra
debugExpression indentation (ExpressionExtra expression (Fatal _ _)) =
  debugExpression indentation expression

debugBinaryExpressionExtra :: B.Builder -> Recover Expression -> BinaryExpressionExtra -> B.Builder
debugBinaryExpressionExtra indentation left (BinaryExpressionExtra operator _ right) =
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

debugConditionalExpressionIf :: B.Builder -> ConditionalExpressionIf -> B.Builder
debugConditionalExpressionIf indentation (ConditionalExpressionIf  _ test consequent Nothing) =
  let newIndentation = indentation <> B.fromText "  " in
    B.fromText "(if"
      <> B.singleton '\n' <> newIndentation
      <> debugRecover (debugExpression newIndentation) test
      <> B.singleton '\n' <> newIndentation
      <> debugBlock newIndentation consequent
      <> B.singleton ')'
debugConditionalExpressionIf indentation (ConditionalExpressionIf  _ test consequent (Just alternate)) =
  let newIndentation = indentation <> B.fromText "  " in
    B.fromText "(if"
      <> B.singleton '\n' <> newIndentation
      <> debugRecover (debugExpression newIndentation) test
      <> B.singleton '\n' <> newIndentation
      <> debugBlock newIndentation consequent
      <> B.singleton '\n' <> newIndentation
      <> debugRecover (debugConditionalExpressionElse newIndentation) alternate
      <> B.singleton ')'

debugConditionalExpressionElse :: B.Builder -> ConditionalExpressionElse -> B.Builder
debugConditionalExpressionElse indentation (ConditionalExpressionElse _ block) =
  debugBlock indentation block
debugConditionalExpressionElse indentation (ConditionalExpressionElseIf _ if_) =
  debugConditionalExpressionIf indentation if_

debugExpressionExtra :: B.Builder -> Expression -> ExpressionExtra -> B.Builder
debugExpressionExtra indentation expression (PropertyExpressionExtra _ label) =
  B.fromText "(prop "
    <> debugExpression indentation expression
    <> B.singleton ' '
    <> debugRecover debugName label
    <> B.singleton ')'

debugExpressionExtra indentation expression (CallExpressionExtra _ (CommaList [] Nothing) _) =
  B.fromText "(call " <> debugExpression indentation expression <> B.singleton ')'

debugExpressionExtra indentation expression (CallExpressionExtra _ args _) =
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
debugPattern :: Pattern -> B.Builder
debugPattern (ConstantPattern constant) = debugConstant constant
debugPattern (VariablePattern (Name identifier _)) =
  B.fromText "(var `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"
debugPattern (HolePattern _) =
  B.fromText "hole"
