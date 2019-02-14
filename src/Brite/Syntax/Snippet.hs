-- Builds snippets of various Brite syntactical features for display in diagnostic messages. For
-- example, code like:
--
-- ```ite
-- MyFancyModule.myFancyFunction(
--   multiple,
--   large,
--   arguments,
-- );
-- ```
--
-- Would generate the snippet `MyFancyModule.myFancyFunction()`. The goal is to capture the shape
-- of the syntax in a way that is human readable.
--
-- ## Style Guide
--
-- * Make the snippet as short as possible without sacrificing any important syntactic features to
--   the clarity of the message.
--
-- * Try to avoid elisions (`...`). An elision is visually a bit noisy and much wider in a monospace
--   font (three characters!) then in a regular font with the “…” character.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Snippet
  ( ListSnippet(..)
  , listSnippet
  , ConstantSnippet(..)
  , ExpressionSnippet(..)
  , PatternSnippet(..)
  , TypeConstructorSnippet(..)
  , printExpressionSnippet
  ) where

import Brite.Syntax.Identifier
import Brite.Syntax.Number
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

-- A list of at most two items. With an extra case, `ListN`, to signal if there were more than
-- two items in the original list.
data ListSnippet a
  = ListSnippet0
  | ListSnippet1 a
  | ListSnippet2 a a
  | ListSnippet3 a a a

-- Converts a list of any arity to a list of at most two items.
listSnippet :: (a -> b) -> [a] -> ListSnippet b
listSnippet _ [] = ListSnippet0
listSnippet f [a1] = ListSnippet1 (f a1)
listSnippet f [a1, a2] = ListSnippet2 (f a1) (f a2)
listSnippet f (a1 : a2 : a3 : _) = ListSnippet3 (f a1) (f a2) (f a3)

data ConstantSnippet
  -- `void`
  = VoidConstantSnippet

  -- `true`, `false`
  | BooleanConstantSnippet Bool

  -- `42`
  | IntegerConstantSnippet IntegerBase Integer

data ExpressionSnippet
  -- `C`
  --
  -- Constants are small so we can print the constant snippet source directly.
  = ConstantExpressionSnippet ConstantSnippet

  -- `x`
  --
  -- Variables are small so we can print the variable snippet source directly.
  | VariableExpressionSnippet Identifier

  -- `fun() {}`
  --
  -- We only print the parameters in a function expression snippet. Not even the types!
  | FunctionExpressionSnippet PatternSnippet

  -- `f()`
  --
  -- We only print the callee and a few arguments.
  | CallExpressionSnippet ExpressionSnippet (ListSnippet ExpressionSnippet)

  -- `{}`
  --
  -- We only print the object and a few property names.
  | ObjectExpressionSnippet (ListSnippet Identifier) (Maybe ExpressionSnippet)

  -- `E.p`
  | PropertyExpressionSnippet ExpressionSnippet Identifier

  -- `if E {}`
  --
  -- We only print `if` with the test expression snippet.
  | ConditionalExpressionSnippet ExpressionSnippet

  -- `do {}`
  --
  -- We print nothing for block expression snippets.
  | BlockExpressionSnippet

  -- `!`
  --
  -- If we have an error expression which did not recover we print a `!`.
  | ErrorExpressionSnippet

data PatternSnippet
  -- `x`
  --
  -- Variables are small so we can print the variable snippet source directly.
  = VariablePatternSnippet Identifier

-- A constructor is the “type level” function which creates a constructed type (`Construct`).
-- Different constructors have a different arity. For instance, `fun(T) -> U` has an arity of two.
--
-- NOTE: Technically, we allow some constructors like functions to have an arbitrary arity. Since
-- you can create a function with multiple arguments like `fun(T, U) -> V`. We don’t represent that
-- in this type since it doesn’t affect snippet printing.
data TypeConstructorSnippet
  = VoidConstructorSnippet
  | BooleanConstructorSnippet
  | IntegerConstructorSnippet
  | FunctionConstructorSnippet
  | ObjectConstructorSnippet

  -- An opaque type variable could be _any_ type constructor. Since we don’t know which one it is,
  -- we’ll print the name and range of the opaque type variable.
  | UnknownConstructorSnippet Identifier

-- Prints a list snippet to a comma list.
printListSnippet :: (a -> Text.Builder) -> ListSnippet a -> Text.Builder
printListSnippet _ ListSnippet0 = mempty
printListSnippet f (ListSnippet1 a) = f a
printListSnippet f (ListSnippet2 a b) = (f a) <> Text.Builder.fromText ", " <> (f b)
printListSnippet f (ListSnippet3 a b c) = (f a) <> Text.Builder.fromText ", " <> (f b) <> Text.Builder.fromText ", " <> (f c)

-- Prints a constant snippet to some text.
printConstantSnippet :: ConstantSnippet -> Text.Builder
printConstantSnippet VoidConstantSnippet = Text.Builder.fromText "void"
printConstantSnippet (BooleanConstantSnippet value) = Text.Builder.fromText (if value then "true" else "false")
printConstantSnippet (IntegerConstantSnippet base value) = printInteger base value

-- Prints an expression snippet to some text.
printExpressionSnippet :: ExpressionSnippet -> Text.Builder
printExpressionSnippet expression = case expression of
  ConstantExpressionSnippet constant -> printConstantSnippet constant
  VariableExpressionSnippet name -> Text.Builder.fromText (identifierText name)

  FunctionExpressionSnippet parameter ->
    Text.Builder.fromText "fun(" <> printPatternSnippet parameter <> Text.Builder.fromText ") {}"

  CallExpressionSnippet callee arguments ->
    printExpressionSnippet callee <>
    Text.Builder.fromText "(" <>
    printListSnippet printExpressionSnippet arguments <>
    Text.Builder.fromText ")"

  ObjectExpressionSnippet properties Nothing ->
    Text.Builder.fromText "{" <>
    printListSnippet (Text.Builder.fromText . identifierText) properties <>
    Text.Builder.fromText "}"

  ObjectExpressionSnippet properties (Just extension) ->
    Text.Builder.fromText "{" <>
    printListSnippet (Text.Builder.fromText . identifierText) properties <>
    Text.Builder.fromText " | " <>
    printExpressionSnippet extension <>
    Text.Builder.fromText "}"

  PropertyExpressionSnippet object property ->
    printExpressionSnippet object <>
    Text.Builder.fromText "." <>
    Text.Builder.fromText (identifierText property)

  ConditionalExpressionSnippet test ->
    Text.Builder.fromText "if " <>
    printExpressionSnippet test <>
    Text.Builder.fromText " {}"

  BlockExpressionSnippet -> Text.Builder.fromText "do {}"
  ErrorExpressionSnippet -> Text.Builder.singleton '!'

-- Prints a pattern snippet to some text.
printPatternSnippet :: PatternSnippet -> Text.Builder
printPatternSnippet pattern = case pattern of
  VariablePatternSnippet name -> Text.Builder.fromText (identifierText name)
