{-# LANGUAGE OverloadedStrings #-}

module Brite.AST
  ( Module(..)
  , Name(..)
  , Recover(..)
  , Statement(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , ConditionalExpressionAlternate(..)
  , ExpressionExtension(..)
  , Pattern(..)
  , debugModule
  ) where

import Brite.Parser.Framework (Recover(..))
import Brite.Source
import qualified Data.Text.Lazy.Builder as B

-- A single Brite file is a module. A module is made up of a list of statements.
newtype Module = Module [Recover Statement]

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

-- The left hand side of a binding statement. Takes a value and deconstructs it into the parts that
-- make it up. Binding those parts to variable names in scope.
data Pattern
  -- `x`
  = VariablePattern Name

-- Debug a module in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST. Each statement in the module is on its own line.
debugModule :: Module -> B.Builder
debugModule (Module []) = B.fromText "empty\n"
debugModule (Module statements) =
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
debugBlock indentation block =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(block "
    <> mconcat (map (\s ->
        B.singleton '\n'
          <> newIndentation
          <> debugRecover (debugStatement newIndentation) s) (blockStatements block))
    <> B.fromText ")"

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
debugExpression indentation (ExpressionExtension expression (Ok extension)) =
  debugExpressionExtension indentation expression extension
debugExpression indentation (ExpressionExtension expression (Recover _ _ extension)) =
  debugExpressionExtension indentation expression extension
debugExpression indentation (ExpressionExtension expression (Fatal _ _)) =
  debugExpression indentation expression
debugExpression indentation (ConditionalExpression _ test consequent Nothing) =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(if "
    <> B.singleton '\n' <> newIndentation
    <> debugRecover (debugExpression newIndentation) test
    <> B.singleton '\n' <> newIndentation
    <> debugBlock newIndentation consequent
    <> B.singleton ')'
debugExpression indentation (ConditionalExpression _ test consequent (Just alternate)) =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(if "
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

-- Debug a pattern in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugPattern :: Pattern -> B.Builder
debugPattern (VariablePattern (Name identifier token)) =
  B.fromText "(var "
    <> debugRange (tokenRange token)
    <> B.fromText " `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"
