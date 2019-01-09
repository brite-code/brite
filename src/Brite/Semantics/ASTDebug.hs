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

-- Prints a Brite AST module to an S-expression text form for debugging purposes.
debugModule :: Module -> Text.Builder
debugModule (Module ss) = mconcat $ map
  ((<> Text.Builder.singleton '\n') . printDocument 80 . printS . debugStatement) ss

debugName :: Name -> S
debugName (Name r n) =
  (B (Text.Lazy.toStrict (Text.Builder.toLazyText
    (Text.Builder.fromText "name" <> Text.Builder.singleton ' ' <> debugRange r))))
    `E` (A (identifierText n))

debugStatement :: Statement -> S
debugStatement s0 = case statementNode s0 of
  ExpressionStatement x -> debugExpression x

debugExpression :: Expression -> S
debugExpression x0 = case expressionNode x0 of
  ConstantExpression (BooleanConstant True) -> (symbol "bool") `E` (A "true")
  ConstantExpression (BooleanConstant False) -> (symbol "bool") `E` (A "false")

  VariableExpression ident -> (symbol "var") `E` (A (identifierText ident))

  CallExpression f xs ->
    foldl
      (\s x -> s `E` (debugExpression x))
      ((symbol "call") `E` (debugExpression f))
      xs

  PropertyExpression x n -> (symbol "prop") `E` (debugExpression x) `E` (debugName n)

  UnaryExpression op' x ->
    (symbol op) `E` (debugExpression x)
    where
      op = case op' of
        Not -> "not"
        Negative -> "neg"
        Positive -> "pos"

  BinaryExpression l op' r ->
    (symbol op) `E` (debugExpression l) `E` (debugExpression r)
    where
      op = case op' of
        Add -> "add"
        Subtract -> "sub"
        Multiply -> "mul"
        Divide -> "div"
        Remainder -> "rem"
        Exponent -> "exp"
        Equals -> "eq"
        NotEquals -> "ne"
        LessThan -> "lt"
        LessThanOrEqual -> "lte"
        GreaterThan -> "gt"
        GreaterThanOrEqual -> "gte"

  LogicalExpression l op' r ->
    (symbol op) `E` (debugExpression l) `E` (debugExpression r)
    where
      op = case op' of
        And -> "and"
        Or -> "or"

  WrappedExpression x Nothing -> (symbol "wrap") `E` (debugExpression x)

  ErrorExpression _ Nothing -> symbol "err"
  ErrorExpression _ (Just x) -> (A "err") `E` (debugExpression (Expression (expressionRange x0) x))

  where
    symbol t = B $ Text.Lazy.toStrict $ Text.Builder.toLazyText $
      Text.Builder.fromText t <> Text.Builder.singleton ' ' <> debugRange (expressionRange x0)

-- An [S-expression][1].
--
-- [1]: https://en.wikipedia.org/wiki/S-expression
data S
  = A Text
  | B Text -- Just like `A` but alway wrapped when on the right-hand-side of `S`.
  | E S S

-- Prints an S-expression to a printer framework document.
printS :: S -> Document
printS (A t) = text t
printS (B t) = text "(" <> text t <> text ")"
printS (E s1 s2) = group (indent (text "(" <> printLeftS s1 <> line <> printS s2 <> text ")"))

-- Prints an S-expression on the left-hand-side of an application.
printLeftS :: S -> Document
printLeftS (A t) = text t
printLeftS (B t) = text t
printLeftS (E s1 s2) = printLeftS s1 <> line <> printS s2
