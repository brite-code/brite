-- Module for debugging an AST as S-expressions. Uses the printing framework from
-- `Brite.Syntax.PrinterFramework`.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.ASTDebug
  ( debugModule
  ) where

import Brite.Semantics.AST
import Brite.Syntax.Identifier
import Brite.Syntax.PrinterFramework
import Brite.Syntax.Range
import Data.Char (intToDigit, toUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Numeric (showInt, showIntAtBase, showFloat)

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

  BindingStatement p Nothing x ->
    (A "bind") `E` (debugPattern p) `E` (debugExpression x)
  BindingStatement p (Just t) x ->
    (A "bind") `E` (debugPattern p) `E` ((A "type") `E` (debugType t)) `E` (debugExpression x)

  ReturnStatement Nothing -> (symbol "return")
  ReturnStatement (Just x) -> (symbol "return") `E` (debugExpression x)

  BreakStatement Nothing -> (symbol "break")
  BreakStatement (Just x) -> (symbol "break") `E` (debugExpression x)

  EmptyStatement -> (symbol "empty")

  FunctionDeclaration n f -> debugFunction (statementRange s0) (Just n) f

  ErrorStatement _ Nothing -> (symbol "err")
  ErrorStatement _ (Just x) -> (A "err") `E` (debugStatement (Statement (statementRange s0) x))

  where
    symbol t = B $ Text.Lazy.toStrict $ Text.Builder.toLazyText $
      Text.Builder.fromText t <> Text.Builder.singleton ' ' <> debugRange (statementRange s0)

debugFunction :: Range -> Maybe (Either a Name) -> Function -> S
debugFunction r n (Function qs ps ret b) =
  let
    s1 = (symbol "fun")
    s2 = maybe s1 (E s1 . either (const (A "err")) debugName) n
    s3 = foldl (\s q -> s `E` (debugQuantifier q)) s2 qs
    s4 = foldl (\s p -> s `E` (debugFunctionParameter p)) s3 ps
    s5 = maybe s4 (E s4 . E (A "type") . debugType) ret
  in
    s5 `E` (debugBlock b)
  where
    debugFunctionParameter (FunctionParameter p Nothing) =
      (A "param") `E` (debugPattern p)
    debugFunctionParameter (FunctionParameter p (Just t)) =
      (A "param") `E` (debugPattern p) `E` ((A "type") `E` (debugType t))

    symbol t = B $ Text.Lazy.toStrict $ Text.Builder.toLazyText $
      Text.Builder.fromText t <> Text.Builder.singleton ' ' <> debugRange r

debugBlock :: Block -> S
debugBlock (Block r ss) =
  foldl
    (\e s -> e `E` (debugStatement s))
    symbol
    ss
  where
    symbol = B $ Text.Lazy.toStrict $ Text.Builder.toLazyText $
      Text.Builder.fromText "block " <> debugRange r

debugConstant :: Range -> Constant -> S
debugConstant range constant = case constant of
  VoidConstant -> (symbol "void")
  BooleanConstant True -> (symbol "bool") `E` (A "true")
  BooleanConstant False -> (symbol "bool") `E` (A "false")
  IntegerConstant Decimal value -> (symbol "int") `E` (A (Text.pack (showInt value "")))
  IntegerConstant Binary value -> (symbol "bin") `E` (A (Text.pack (showIntAtBase 2 intToDigit value "")))
  IntegerConstant Hexadecimal value -> (symbol "hex") `E` (A (Text.pack (showIntAtBase 16 (toUpper . intToDigit) value "")))
  FloatConstant value -> (symbol "float") `E` (A (Text.pack (showFloat value "")))
  where
    symbol t = B $ Text.Lazy.toStrict $ Text.Builder.toLazyText $
      Text.Builder.fromText t <> Text.Builder.singleton ' ' <> debugRange range

debugExpression :: Expression -> S
debugExpression x0 = case expressionNode x0 of
  ConstantExpression c -> debugConstant (expressionRange x0) c

  VariableExpression ident -> (symbol "var") `E` (A (identifierText ident))

  FunctionExpression f -> debugFunction (expressionRange x0) Nothing f

  CallExpression f xs ->
    foldl
      (\s x -> s `E` (debugExpression x))
      ((symbol "call") `E` (debugExpression f))
      xs

  ObjectExpression ps ext ->
    let
      s2 =
        foldl
          (\s1 p -> s1 `E` (debugProperty p))
          (symbol "object")
          ps
    in
      case ext of
        Nothing -> s2
        Just x -> s2 `E` (debugExpression x)
    where
      debugProperty (ObjectExpressionProperty n Nothing) =
        (A "prop") `E` (debugName n)
      debugProperty (ObjectExpressionProperty n (Just x)) =
        (A "prop") `E` (debugName n) `E` (debugExpression x)

  PropertyExpression x n -> (symbol "prop") `E` (debugExpression x) `E` (debugName n)

  PrefixExpression op' x ->
    (symbol op) `E` (debugExpression x)
    where
      op = case op' of
        Not -> "not"
        Negative -> "neg"
        Positive -> "pos"

  InfixExpression l op' r ->
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

  ConditionalExpression (ConditionalExpressionIf x1 b1 Nothing) ->
    (symbol "if") `E` (debugExpression x1) `E` (debugBlock b1)

  ConditionalExpression (ConditionalExpressionIf x1 b1 (Just a1)) ->
    (symbol "if") `E` (debugExpression x1) `E` (debugBlock b1) `E` (alternate a1)
    where
      alternate (ConditionalExpressionElse b2) = debugBlock b2
      alternate (ConditionalExpressionElseIf c2) = consequent c2

      consequent (ConditionalExpressionIf x2 b2 Nothing) =
        (A "if") `E` (debugExpression x2) `E` (debugBlock b2)
      consequent (ConditionalExpressionIf x2 b2 (Just a2)) =
        (A "if") `E` (debugExpression x2) `E` (debugBlock b2) `E` (alternate a2)

  BlockExpression (Block _ ss) ->
    foldl
      (\e s -> e `E` (debugStatement s))
      (symbol "block")
      ss

  LoopExpression (Block _ ss) ->
    foldl
      (\e s -> e `E` (debugStatement s))
      (symbol "loop")
      ss

  WrappedExpression x Nothing -> (symbol "wrap") `E` (debugExpression x)
  WrappedExpression x (Just t) ->
    (symbol "wrap") `E` (debugExpression x) `E` ((A "type") `E` (debugType t))

  ErrorExpression _ Nothing -> symbol "err"
  ErrorExpression _ (Just x) -> (A "err") `E` (debugExpression (Expression (expressionRange x0) x))

  where
    symbol t = B $ Text.Lazy.toStrict $ Text.Builder.toLazyText $
      Text.Builder.fromText t <> Text.Builder.singleton ' ' <> debugRange (expressionRange x0)

debugPattern :: Pattern -> S
debugPattern x0 = case patternNode x0 of
  ConstantPattern c -> debugConstant (patternRange x0) c

  VariablePattern ident -> (symbol "var") `E` (A (identifierText ident))

  HolePattern -> (symbol "hole")

  ObjectPattern ps ext ->
    let
      s2 =
        foldl
          (\s1 p -> s1 `E` (debugProperty p))
          (symbol "object")
          ps
    in
      case ext of
        Nothing -> s2
        Just x -> s2 `E` (debugPattern x)
    where
      debugProperty (ObjectPatternProperty n Nothing) =
        (A "prop") `E` (debugName n)
      debugProperty (ObjectPatternProperty n (Just x)) =
        (A "prop") `E` (debugName n) `E` (debugPattern x)

  WrappedPattern x -> (symbol "wrap") `E` (debugPattern x)

  ErrorPattern _ Nothing -> symbol "err"
  ErrorPattern _ (Just x) -> (A "err") `E` (debugPattern (Pattern (patternRange x0) x))

  where
    symbol t = B $ Text.Lazy.toStrict $ Text.Builder.toLazyText $
      Text.Builder.fromText t <> Text.Builder.singleton ' ' <> debugRange (patternRange x0)

debugType :: Type -> S
debugType x0 = case typeNode x0 of
  VariableType ident -> (symbol "var") `E` (A (identifierText ident))

  BottomType -> (symbol "bottom")

  VoidType -> (symbol "void")

  FunctionType qs ps x ->
    let
      s2 =
        foldl
          (\s q -> s `E` (debugQuantifier q))
          (symbol "fun")
          qs
      s3 =
        foldl
          (\s p -> s `E` ((A "param") `E` (debugType p)))
          s2
          ps
    in
      s3 `E` (debugType x)

  ObjectType ps ext ->
    let
      s2 =
        foldl
          (\s1 p -> s1 `E` (debugProperty p))
          (symbol "object")
          ps
    in
      case ext of
        Nothing -> s2
        Just x -> s2 `E` (debugType x)
    where
      debugProperty (ObjectTypeProperty n x) =
        (A "prop") `E` (debugName n) `E` (debugType x)

  QuantifiedType qs x ->
    let
      s2 =
        foldl
          (\s1 q -> s1 `E` (debugQuantifier q))
          (symbol "quantify")
          qs
    in
      s2 `E` (debugType x)

  WrappedType x -> (symbol "wrap") `E` (debugType x)

  ErrorType _ Nothing -> symbol "err"
  ErrorType _ (Just x) -> (A "err") `E` (debugType (Type (typeRange x0) x))

  where
    symbol t = B $ Text.Lazy.toStrict $ Text.Builder.toLazyText $
      Text.Builder.fromText t <> Text.Builder.singleton ' ' <> debugRange (typeRange x0)

debugQuantifier :: Quantifier -> S
debugQuantifier (Quantifier n Nothing) = (A "forall") `E` (debugName n)
debugQuantifier (Quantifier n (Just (Flexible, t))) =
  (A "forall") `E` (debugName n) `E` (A "flex") `E` (debugType t)
debugQuantifier (Quantifier n (Just (Rigid, t))) =
  (A "forall") `E` (debugName n) `E` (A "rigid") `E` (debugType t)

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
