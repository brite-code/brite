{-# LANGUAGE OverloadedStrings #-}

module Brite.AST
  ( Module(..)
  , Name(..)
  , Recover(..)
  , CommaList(..)
  , Statement(..)
  , Block(..)
  , Function(..)
  , Constant(..)
  , Expression(..)
  , ConditionalExpressionAlternate(..)
  , ExpressionExtension(..)
  , Pattern(..)
  , moduleTokens
  , debugModule
  ) where

import Brite.Parser.Framework (Recover(..), CommaList(..))
import Brite.Source
import Data.Monoid (Endo(..))
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
  | BindingStatement Token (Recover Pattern) (Recover Token) (Recover Expression) Semicolon

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
  = ConstantExpression Constant
  -- `x`
  | VariableExpression Name
  -- `fun(...) { ... }`
  | FunctionExpression Function
  -- `if E { ... }`, `if E { ... } else { ... }`
  | ConditionalExpression Token (Recover Expression) Block (Maybe (Recover ConditionalExpressionAlternate))
  -- `do { ... }`
  | BlockExpression Token Block
  -- `(E)`
  | WrappedExpression Token (Recover Expression) (Recover Token)
  -- `E ...`
  | ExpressionExtension Expression (Recover ExpressionExtension)

-- `else { ... }`
data ConditionalExpressionAlternate = ConditionalExpressionAlternate Token Block

-- Some extension of an expression. We keep this as a separate data type to match our
-- parser implementation.
data ExpressionExtension
  -- `E.p`
  = PropertyExpressionExtension Token (Recover Name)
  -- `f(...)`
  | CallExpressionExtension Token (CommaList Expression) (Recover Token)

-- The left hand side of a binding statement. Takes a value and deconstructs it into the parts that
-- make it up. Binding those parts to variable names in scope.
data Pattern
  -- `x`
  = VariablePattern Name
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
expressionTokens (ConditionalExpression t e b Nothing) =
  singletonToken t <> recoverTokens expressionTokens e <> blockTokens b
expressionTokens (ConditionalExpression t e b (Just alt)) =
  singletonToken t
    <> recoverTokens expressionTokens e
    <> blockTokens b
    <> recoverTokens conditionalExpressionAlternateTokens alt
expressionTokens (BlockExpression t b) = singletonToken t <> blockTokens b
expressionTokens (WrappedExpression t1 e t2) =
  singletonToken t1 <> recoverTokens expressionTokens e <> recoverTokens singletonToken t2
expressionTokens (ExpressionExtension e ext) =
  expressionTokens e <> recoverTokens expressionExtensionTokens ext

conditionalExpressionAlternateTokens :: ConditionalExpressionAlternate -> Tokens
conditionalExpressionAlternateTokens (ConditionalExpressionAlternate t b) =
  singletonToken t <> blockTokens b

expressionExtensionTokens :: ExpressionExtension -> Tokens
expressionExtensionTokens (PropertyExpressionExtension t l) =
  singletonToken t <> recoverTokens nameTokens l
expressionExtensionTokens (CallExpressionExtension t1 args t2) =
  singletonToken t1 <> commaListTokens expressionTokens args <> recoverTokens singletonToken t2

-- Get tokens from a pattern.
patternTokens :: Pattern -> Tokens
patternTokens (VariablePattern name) = nameTokens name
patternTokens (HolePattern token) = singletonToken token

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
debugName (Name identifier token) =
  B.fromText "(name "
    <> debugRange (tokenRange token)
    <> B.fromText " `"
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
debugFunction indentation (Function _ name _ (CommaList params paramn) _ block) =
  B.fromText "(fun"
    <> maybe mempty ((B.singleton '\n' <>) . (newIndentation <>) . debugRecover debugName) name
    <> mconcat (map (debugParam . fst) params)
    <> maybe mempty debugParam paramn
    <> B.singleton '\n' <> newIndentation
    <> debugBlock newIndentation block
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugParam param = B.singleton '\n' <> newIndentation <> debugRecover debugPattern param

-- Debug a constant in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugConstant :: Constant -> B.Builder
debugConstant (BooleanConstant True token) =
  B.fromText "(bool " <> debugRange (tokenRange token) <> B.fromText " true)"
debugConstant (BooleanConstant False token) =
  B.fromText "(bool " <> debugRange (tokenRange token) <> B.fromText " false)"

-- Debug an expression in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugExpression :: B.Builder -> Expression -> B.Builder
debugExpression _ (ConstantExpression constant) = debugConstant constant
debugExpression _ (VariableExpression (Name identifier token)) =
  B.fromText "(var "
    <> debugRange (tokenRange token)
    <> B.fromText " `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"
debugExpression indentation (FunctionExpression function) = debugFunction indentation function
debugExpression indentation (ExpressionExtension expression (Ok extension)) =
  debugExpressionExtension indentation expression extension
debugExpression indentation (ExpressionExtension expression (Recover _ _ extension)) =
  debugExpressionExtension indentation expression extension
debugExpression indentation (ExpressionExtension expression (Fatal _ _)) =
  debugExpression indentation expression
debugExpression indentation (ConditionalExpression _ test consequent Nothing) =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(if"
    <> B.singleton '\n' <> newIndentation
    <> debugRecover (debugExpression newIndentation) test
    <> B.singleton '\n' <> newIndentation
    <> debugBlock newIndentation consequent
    <> B.singleton ')'
debugExpression indentation (ConditionalExpression _ test consequent (Just alternate)) =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(if"
    <> B.singleton '\n' <> newIndentation
    <> debugRecover (debugExpression newIndentation) test
    <> B.singleton '\n' <> newIndentation
    <> debugBlock newIndentation consequent
    <> B.singleton '\n' <> newIndentation
    <> debugRecover (debugConditionalExpressionAlternate newIndentation) alternate
    <> B.singleton ')'
debugExpression indentation (BlockExpression _ block) =
  B.fromText "(do " <> debugBlock indentation block <> B.singleton ')'
debugExpression indentation (WrappedExpression _ expression _) =
  B.fromText "(wrap "
    <> debugRecover (debugExpression indentation) expression
    <> B.fromText ")"

debugConditionalExpressionAlternate :: B.Builder -> ConditionalExpressionAlternate -> B.Builder
debugConditionalExpressionAlternate indentation (ConditionalExpressionAlternate _ block) =
  debugBlock indentation block

debugExpressionExtension :: B.Builder -> Expression -> ExpressionExtension -> B.Builder
debugExpressionExtension indentation expression (PropertyExpressionExtension _ label) =
  B.fromText "(prop "
    <> debugExpression indentation expression
    <> B.singleton ' '
    <> debugRecover debugName label
    <> B.singleton ')'
debugExpressionExtension indentation expression (CallExpressionExtension _ (CommaList args argn) _) =
  B.fromText "(call"
    <> debugArg (Ok expression)
    <> mconcat (map (debugArg . fst) args)
    <> maybe mempty debugArg argn
    <> B.singleton ')'
  where
    newIndentation = indentation <> B.fromText "  "
    debugArg arg =
      B.singleton '\n' <> newIndentation
        <> debugRecover (debugExpression newIndentation) arg

-- Debug a pattern in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugPattern :: Pattern -> B.Builder
debugPattern (VariablePattern (Name identifier token)) =
  B.fromText "(var "
    <> debugRange (tokenRange token)
    <> B.fromText " `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"
debugPattern (HolePattern token) =
  B.fromText "(hole "
    <> debugRange (tokenRange token)
    <> B.fromText "`)"
