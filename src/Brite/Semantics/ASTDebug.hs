-- Module for debugging an AST as S-expressions. Uses the printing framework from
-- `Brite.Syntax.PrinterFramework`.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.ASTDebug
  ( debugModule
  ) where

import Brite.Semantics.AST
import Brite.Syntax.PrinterFramework
import Brite.Syntax.Tokens (debugRange)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

-- An [S-expression][1].
--
-- [1]: https://en.wikipedia.org/wiki/S-expression
data S
  = A Text
  | E S S

-- Prints a Brite AST module to an S-expression text form for debugging purposes.
debugModule :: Module -> Text.Builder
debugModule (Module ss) = mconcat $ map
  ((<> Text.Builder.singleton '\n') . printDocument 80 . printS . debugStatement) ss

debugStatement :: Statement -> S
debugStatement s0 = case statementNode s0 of
  ExpressionStatement x -> debugExpression x

debugExpression :: Expression -> S
debugExpression x0 = case expressionNode x0 of
  ConstantExpression (BooleanConstant True) -> (A "bool") `E` (A range) `E` (A "true")
  ConstantExpression (BooleanConstant False) -> (A "bool") `E` (A range) `E` (A "false")

  VariableExpression ident -> (A "var") `E` (A range) `E` (A (identifierText ident))

  UnaryExpression op' x ->
    (A op) `E` (A range) `E` (debugExpression x)
    where
      op = case op' of
        Not -> "not"
        Negative -> "neg"
        Positive -> "pos"

  WrappedExpression x Nothing -> (A "wrap") `E` (A range) `E` (debugExpression x)

  ErrorExpression _ Nothing -> (A "err") `E` (A range)
  ErrorExpression _ (Just x) -> (A "err") `E` (debugExpression (Expression (expressionRange x0) x))

  where
    range = Text.Lazy.toStrict (Text.Builder.toLazyText (debugRange (expressionRange x0)))

-- Prints an S-expression to a printer framework document.
printS :: S -> Document
printS (A t) = text t
printS (E s1 s2) = group (indent (text "(" <> printLeftS s1 <> line <> printS s2 <> text ")"))

-- Prints an S-expression on the left-hand-side of an application.
printLeftS :: S -> Document
printLeftS (A t) = text t
printLeftS (E s1 s2) = printLeftS s1 <> line <> printS s2
