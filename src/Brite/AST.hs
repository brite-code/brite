{-# LANGUAGE OverloadedStrings #-}

module Brite.AST
  ( Module(..)
  , Statement(..)
  , StatementNode(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  , Pattern(..)
  , PatternNode(..)
  , debugModule
  ) where

import Brite.Diagnostics
import Brite.Source
import qualified Data.Text.Lazy.Builder as B

-- A single Brite file is a module. A module is made up of a list of statements.
newtype Module = Module [Statement]

-- Represents some imperative action to be carried out.
data Statement = Statement
  { statementRange :: Range
  , statementNode :: StatementNode
  }

data StatementNode
  -- `E;`
  = ExpressionStatement Expression
  -- `let x = E;`
  | BindingStatement Pattern Expression
  -- A parsing error occurred when trying to parse our statement. We might or might not have been
  -- able to recover.
  | ErrorStatement Diagnostic (Maybe StatementNode)

-- A set of statements scoped in a block. Names declared in this block may only be accessed by code
-- within the block.
data Block = Block
  { blockRange :: Range
  , blockStatements :: [Statement]
  }

-- Some constant value in our program.
data Constant
  -- `true`, `false`
  = BooleanConstant Bool

-- Some instructions our programming language interprets to return a value and possibly perform
-- some side effects.
data Expression = Expression
  { expressionRange :: Range
  , expressionNode :: ExpressionNode
  }

data ExpressionNode
  -- `C`
  = ConstantExpression Constant
  -- `x`
  | VariableExpression Identifier
  -- `if E { ... }`, if E { ... } else { ... }`
  | ConditionalExpression Expression Block (Maybe Block)
  -- `do { ... }`
  | BlockExpression Block
  -- `(E)`
  | WrappedExpression Expression
  -- A parsing error occurred when trying to parse our expression. We might or might not have been
  -- able to recover.
  | ErrorExpression Diagnostic (Maybe ExpressionNode)

-- The left hand side of a binding statement. Takes a value and deconstructs it into the parts that
-- make it up. Binding those parts to variable names in scope.
data Pattern = Pattern
  { patternRange :: Range
  , patternNode :: PatternNode
  }

data PatternNode
  -- `x`
  = VariablePattern Identifier
  -- A parsing error occurred when trying to parse our pattern. We might or might not have been
  -- able to recover.
  | ErrorPattern Diagnostic (Maybe PatternNode)

-- Debug a module in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST. Each statement in the module is on its own line.
debugModule :: Module -> B.Builder
debugModule (Module []) = B.fromText "empty\n"
debugModule (Module statements) =
  mconcat $ map (\s -> debugStatement "" s <> B.singleton '\n') statements

-- Debug a statement in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugStatement :: B.Builder -> Statement -> B.Builder
debugStatement indentation (Statement _ (ExpressionStatement expression)) =
  debugExpression indentation expression
debugStatement indentation (Statement range (BindingStatement pattern expression)) =
  B.fromText "(bind "
    <> debugRange range
    <> B.singleton ' '
    <> debugPattern pattern
    <> B.singleton ' '
    <> debugExpression indentation expression
    <> B.fromText ")"
debugStatement _ (Statement range (ErrorStatement _ Nothing)) =
  B.fromText "(err " <> debugRange range <> B.fromText ")"
debugStatement indentation (Statement range (ErrorStatement _ (Just statement))) =
  B.fromText "(err "
    <> debugStatement indentation (Statement range statement)
    <> B.fromText ")"

-- Debug a block in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugBlock :: B.Builder -> Block -> B.Builder
debugBlock indentation block =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(block "
    <> debugRange (blockRange block)
    <> mconcat (map (\s ->
        B.singleton '\n'
          <> newIndentation
          <> debugStatement newIndentation s) (blockStatements block))
    <> B.fromText ")"

-- Debug a constant in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugConstant :: Range -> Constant -> B.Builder
debugConstant range (BooleanConstant True) = B.fromText "(bool " <> debugRange range <> B.fromText " true)"
debugConstant range (BooleanConstant False) = B.fromText "(bool " <> debugRange range <> B.fromText " false)"

-- Debug an expression in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugExpression :: B.Builder -> Expression -> B.Builder
debugExpression _ (Expression range (ConstantExpression constant)) =
  debugConstant range constant
debugExpression _ (Expression range (VariableExpression identifier)) =
  B.fromText "(var "
    <> debugRange range
    <> B.fromText " `"
    <> B.fromText (identifierText identifier)
    <> B.fromText "`)"
debugExpression indentation (Expression range (ConditionalExpression test consequent Nothing)) =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(if "
    <> debugRange range <> B.singleton '\n'
    <> newIndentation <> debugExpression newIndentation test <> B.singleton '\n'
    <> newIndentation <> debugBlock newIndentation consequent
    <> B.singleton ')'
debugExpression indentation (Expression range (ConditionalExpression test consequent (Just alternate))) =
  let newIndentation = indentation <> B.fromText "  " in
  B.fromText "(if "
    <> debugRange range <> B.singleton '\n'
    <> newIndentation <> debugExpression newIndentation test <> B.singleton '\n'
    <> newIndentation <> debugBlock newIndentation consequent <> B.singleton '\n'
    <> newIndentation <> debugBlock newIndentation alternate
    <> B.singleton ')'
debugExpression indentation (Expression range (BlockExpression block)) =
  B.fromText "(do "
    <> debugRange range
    <> B.singleton ' '
    <> debugBlock indentation block
    <> B.singleton ')'
debugExpression indentation (Expression range (WrappedExpression expression)) =
  B.fromText "(wrap "
    <> debugRange range
    <> B.fromText " "
    <> debugExpression indentation expression
    <> B.fromText ")"
debugExpression _ (Expression range (ErrorExpression _ Nothing)) =
  B.fromText "(err " <> debugRange range <> B.fromText ")"
debugExpression indentation (Expression range (ErrorExpression _ (Just expression))) =
  B.fromText "(err "
    <> debugExpression indentation (Expression range expression)
    <> B.fromText ")"

-- Debug a pattern in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugPattern :: Pattern -> B.Builder
debugPattern (Pattern range (VariablePattern ident)) =
  B.fromText "(var "
    <> debugRange range
    <> B.fromText " `"
    <> B.fromText (identifierText ident)
    <> B.fromText "`)"
debugPattern (Pattern range (ErrorPattern _ Nothing)) =
  B.fromText "(err " <> debugRange range <> B.fromText ")"
debugPattern (Pattern range (ErrorPattern _ (Just pattern))) =
  B.fromText "(err "
    <> debugPattern (Pattern range pattern)
    <> B.fromText ")"
