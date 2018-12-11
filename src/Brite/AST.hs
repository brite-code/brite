{-# LANGUAGE OverloadedStrings #-}

module Brite.AST
  ( Statement(..)
  , Expression(..)
  , Pattern(..)
  , debugStatement
  , debugExpression
  , debugPattern
  ) where

import Brite.Diagnostics
import Brite.Source
import qualified Data.Text.Lazy.Builder as B

-- Represents some imperative action to be carried out.
data Statement
  -- `let x = E;`
  = BindingStatement Pattern Expression
  -- A parsing error occurred when trying to parse our statement. We might or might not have been
  -- able to recover.
  | ErrorStatement Diagnostic (Maybe Statement)

-- Some instructions our programming language interprets to return a value and possibly perform
-- some side effects.
data Expression
  -- `x`
  = VariableExpression Range Identifier
  -- A parsing error occurred when trying to parse our expression. We might or might not have been
  -- able to recover.
  | ErrorExpression Diagnostic (Maybe Expression)

-- The left hand side of a binding statement. Takes a value and deconstructs it into the parts that
-- make it up. Binding those parts to variable names in scope.
data Pattern
  -- `x`
  = VariablePattern Range Identifier
  -- A parsing error occurred when trying to parse our pattern. We might or might not have been
  -- able to recover.
  | ErrorPattern Diagnostic (Maybe Pattern)

-- Debug a statement in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugStatement :: Statement -> B.Builder
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

-- Debug an expression in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugExpression :: Expression -> B.Builder
debugExpression (VariableExpression range ident) =
  B.fromText "(var "
    <> debugRange range
    <> B.fromText " `"
    <> B.fromText (identifierText ident)
    <> B.fromText "`)"
debugExpression (ErrorExpression _ Nothing) = B.fromText "err"
debugExpression (ErrorExpression _ (Just x)) =
  B.fromText "(err "
    <> debugExpression x
    <> B.fromText ")"

-- Debug a pattern in an S-expression form. This abbreviated format should make it easier to see
-- the structure of the AST node.
debugPattern :: Pattern -> B.Builder
debugPattern (VariablePattern range ident) =
  B.fromText "(var "
    <> debugRange range
    <> B.fromText " `"
    <> B.fromText (identifierText ident)
    <> B.fromText "`)"
debugPattern (ErrorPattern _ Nothing) = B.fromText "err"
debugPattern (ErrorPattern _ (Just x)) =
  B.fromText "(err "
    <> debugPattern x
    <> B.fromText ")"
