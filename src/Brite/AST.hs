{-# LANGUAGE OverloadedStrings #-}

module Brite.AST
  ( Statement(..)
  , Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  , Pattern(..)
  , PatternNode(..)
  , debugStatement
  , debugExpression
  ) where

import Brite.Diagnostics
import Brite.Source
import qualified Data.Text.Lazy.Builder as B

-- Represents some imperative action to be carried out.
data Statement
  -- `E;`
  = ExpressionStatement Expression
  -- `let x = E;`
  | BindingStatement Pattern Expression
  -- A parsing error occurred when trying to parse our statement. We might or might not have been
  -- able to recover.
  | ErrorStatement Diagnostic (Maybe Statement)

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
  -- `(E)`
  | WrappedExpression Expression
  -- A parsing error occurred when trying to parse our expression. We might or might not have been
  -- able to recover.
  | ErrorExpression Diagnostic (Maybe Expression)

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
  | ErrorPattern Diagnostic (Maybe Pattern)

-- Debug a statement in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugStatement :: Statement -> B.Builder
debugStatement (ExpressionStatement x) = debugExpression x
debugStatement (BindingStatement p x) =
  B.fromText "(bind "
    <> debugPattern p
    <> B.singleton ' '
    <> debugExpression x
    <> B.fromText ")"
debugStatement (ErrorStatement _ Nothing) = B.fromText "err"
debugStatement (ErrorStatement _ (Just s)) =
  B.fromText "(err "
    <> debugStatement s
    <> B.fromText ")"

-- Debug a constant in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugConstant :: Range -> Constant -> B.Builder
debugConstant range (BooleanConstant True) = B.fromText "(bool " <> debugRange range <> B.fromText " true)"
debugConstant range (BooleanConstant False) = B.fromText "(bool " <> debugRange range <> B.fromText " false)"

-- Debug an expression in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugExpression :: Expression -> B.Builder
debugExpression (Expression range (ConstantExpression constant)) = debugConstant range constant
debugExpression (Expression range (VariableExpression ident)) =
  B.fromText "(var "
    <> debugRange range
    <> B.fromText " `"
    <> B.fromText (identifierText ident)
    <> B.fromText "`)"
debugExpression (Expression range (WrappedExpression expression)) =
  B.fromText "(wrap "
    <> debugRange range
    <> B.fromText " "
    <> debugExpression expression
    <> B.fromText ")"
debugExpression (Expression range (ErrorExpression _ Nothing)) =
  B.fromText "(err " <> debugRange range <> B.fromText ")"
debugExpression (Expression range (ErrorExpression _ (Just expression))) =
  B.fromText "(err "
    <> debugRange range
    <> B.fromText " "
    <> debugExpression expression
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
debugPattern (Pattern range (ErrorPattern _ (Just p))) =
  B.fromText "(err "
    <> debugRange range
    <> B.fromText " "
    <> debugPattern p
    <> B.fromText ")"
