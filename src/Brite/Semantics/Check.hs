-- Checks an AST for type errors. Converting that AST into an internal, typed, representation.

{-# LANGUAGE OverloadedStrings #-} -- TODO: Remove when removing `Bool` and `Int` special handling.

module Brite.Semantics.Check
  ( checkPolytype
  ) where

import Brite.Diagnostics
import Brite.Semantics.AST (Identifier)
import qualified Brite.Semantics.AST as AST
import Brite.Semantics.AVT
import Brite.Semantics.CheckMonad
import Brite.Semantics.Namer
import Brite.Semantics.Type (Polytype, Monotype)
import qualified Brite.Semantics.Type as Type
import Brite.Syntax.Tokens (identifierText)
import Data.Foldable (toList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- Type checks an expression AST and returns a typed AVT expression.
--
-- This algorithm corresponds to the type inference algorithm named `infer()` in the [MLF thesis][1]
-- described in section 7.1.
--
-- In theory:
--
-- > A type inference problem is a triple `(Q, Γ, a)`, where all free type variables in `Γ` are
-- > bound in `Q`. A pair `(Q', o)` is a solution to this problem if `Q ⊑ Q'` and
-- > `(Q') Γ ⊦ a : o` holds.
--
-- In practice:
--
-- * `Q` corresponds to the `Prefix` argument.
-- * `Γ` corresponds to the `Context` argument.
-- * `a` corresponds to the `AST.Expression` argument.
-- * `Q'` corresponds to the `Prefix` argument. We mutate the prefix reference during type checking.
--   Any future uses of the prefix will be `Q'`. This works because the MLF thesis never asks us to
--   clone a prefix.
-- * `o` corresponds to the result of `expressionType` on the returned expression. Not only do we
--   type check, but we also build an AVT which is semantically equivalent to our input AST.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
checkExpression :: AST.Expression -> Check s Expression
checkExpression astExpression =
  -- TODO: Type annotations on function parameters should have a _negative_ polarity. The polarity
  -- is an aesthetic choice that matches type annotations. Consider:
  --
  -- ```ite
  -- fun f(
  --   f: fun(fun<T>(T) -> T) -> Int
  -- ) {
  --   f(add1)
  -- }
  --
  -- (f: fun(fun(fun<T>(T) -> T) -> Int) -> Int);
  -- ```
  error "TODO"

-- Checks an AST type and turns it into a polytype.
--
-- Any errors in the syntax or semantics of the AST are turned into bottom types (`!`). If we had
-- a syntax error which was recovered from then we forget the syntax error and check the recovered
-- type. Unlike expressions or patterns, there is no component of types which are executed at
-- runtime. Types are completely erased from the source code. So if there’s an error in the type it
-- is unclear where we should throw a runtime error.
--
-- * Should we throw a runtime error where the type is used? This complicates our type system since
--   we’d need to add support for this in unification. It might even endanger soundness.
-- * Should we throw a runtime error where the type is defined? This is too aggressive and will
--   prevent valid code from being run.
--
-- By converting errors into bottom types we use a form of the first strategy. An error in the types
-- will appear as a “could not unify with bottom” type error.
checkPolytype :: Type.Polarity -> HashSet Identifier -> AST.Type -> DiagnosticWriter Polytype
checkPolytype polarity context0 type0 = case AST.typeNode type0 of
  -- Special handling for booleans and integers.
  --
  -- TODO: Replace this with proper handling. When doing so also delete the `OverloadedStrings`
  -- language extension.
  AST.VariableType name | identifierText name == "Bool" -> return (Type.polytype Type.boolean)
  AST.VariableType name | identifierText name == "Int" -> return (Type.polytype Type.integer)

  -- Lookup the variable type in our context. If we don’t find it then return a “variable not found”
  -- error type.
  AST.VariableType name ->
    if HashSet.member name context0 then return (Type.polytype (Type.variable name))
    else unboundTypeVariable range name >>= errorType

  AST.BottomType -> return Type.bottom

  -- Check a function type with its quantifiers.
  AST.FunctionType quantifiers [uncheckedParameterType] uncheckedBodyType -> do
    (context1, bindings1) <- checkQuantifiers polarity context0 quantifiers Seq.empty
    let counter0 = initialFreshCounter
    (counter1, bindings2, parameterType) <- checkMonotype (Type.flipPolarity polarity) context1 counter0 bindings1 uncheckedParameterType
    (_, bindings3, bodyType) <- checkMonotype polarity context1 counter1 bindings2 uncheckedBodyType
    return (Type.quantify (toList bindings3) (Type.function parameterType bodyType))

  -- Check the quantifiers of a quantified type. If the body is also a quantified type then we will
  -- inline those bindings into our prefix as well.
  AST.QuantifiedType quantifiers uncheckedBodyType -> do
    (context1, bindings1) <- checkQuantifiers polarity context0 quantifiers Seq.empty
    (_, bindings2, bodyType) <- checkMonotype polarity context1 initialFreshCounter bindings1 uncheckedBodyType
    return (Type.quantify (toList bindings2) bodyType)

  AST.WrappedType type1 -> checkPolytype polarity context0 type1

  where
    range = AST.typeRange type0

    -- Forces the programmer to provide proof that they reported an error diagnostic. However, we
    -- don’t include the diagnostic in our internal type structure since there is no obvious place
    -- to throw that error at runtime.
    errorType :: Diagnostic -> DiagnosticWriter Polytype
    errorType _ = return Type.bottom

-- Check all the AST type quantifiers and convert them into a list of bindings.
checkQuantifiers ::
  Type.Polarity -> HashSet Identifier -> [AST.Quantifier] -> Seq Type.Binding ->
    DiagnosticWriter (HashSet Identifier, Seq Type.Binding)
checkQuantifiers _ context0 [] bindings = return (context0, bindings)
checkQuantifiers polarity context0 (AST.Quantifier name bound : quantifiers) bindings = do
  -- Create the binding. If no bound was provided in the AST then we use a flexible, bottom
  -- type, bound. Otherwise we need to check our bound type with the current context.
  binding <- case bound of
    Nothing -> return (Type.Binding (AST.nameIdentifier name) Type.Flexible Type.bottom)
    Just (flexibility, boundType) ->
      -- TODO: We use the current polarity of the position where the quantifiers lie. Check if this
      -- works when reconstructing the type.
      Type.Binding (AST.nameIdentifier name) flexibility <$> checkPolytype polarity context0 boundType
  -- Introduce our new type variable ID into our context. Notably introduce our type variable
  -- after checking our binding type.
  let context1 = HashSet.insert (AST.nameIdentifier name) context0
  -- Add our binding and process the remaining quantifiers in our new context.
  checkQuantifiers polarity context1 quantifiers (bindings |> binding)

-- Checks a type expecting that we return a monotype. However, in our type syntax we can have nested
-- polytypes but we can’t have that in our internal representation of types. So when we see a
-- polytype nested inside a monotype we add a new binding where the flexibility is determined by the
-- polarity in the position we find the polytype. So for example, the type:
--
-- ```ite
-- fun(fun<T>(T) -> T) -> fun<T>(T) -> T
-- ```
--
-- Becomes:
--
-- ```ite
-- fun<
--   Type1 = fun<T>(T) -> T,
--   Type2: fun<T>(T) -> T,
-- >(Type1) -> Type2
-- ```
--
-- When we print our internal type representation back out we will inline the types again.
checkMonotype ::
  Type.Polarity -> HashSet Identifier -> FreshCounter -> Seq Type.Binding -> AST.Type ->
    DiagnosticWriter (FreshCounter, Seq Type.Binding, Monotype)
checkMonotype polarity context counter0 bindings0 type0 = case AST.typeNode type0 of
  -- Special handling for booleans and integers.
  --
  -- TODO: Replace this with proper handling. When doing so also delete the `OverloadedStrings`
  -- language extension.
  AST.VariableType name | identifierText name == "Bool" -> return (counter0, bindings0, Type.boolean)
  AST.VariableType name | identifierText name == "Int" -> return (counter0, bindings0, Type.integer)

  -- Lookup the variable type in our context. If we don’t find it then return a “variable not found”
  -- error type.
  AST.VariableType name ->
    if HashSet.member name context then return (counter0, bindings0, Type.variable name)
    else unboundTypeVariable range name >>= errorType

  -- If we see a bottom type when we are expecting a monotype then create a fresh type variable
  -- which we will place in our polytype prefix.
  AST.BottomType -> do
    let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
    let binding = Type.Binding name flexibility Type.bottom
    return (counter1, bindings0 |> binding, Type.variable name)

  -- Check the parameter and body type of a function.
  AST.FunctionType [] [uncheckedParameterType] uncheckedBodyType -> do
    (counter1, bindings1, parameterType) <- checkMonotype (Type.flipPolarity polarity) context counter0 bindings0 uncheckedParameterType
    (counter2, bindings2, bodyType) <- checkMonotype polarity context counter1 bindings1 uncheckedBodyType
    return (counter2, bindings2, Type.function parameterType bodyType)

  -- If we see a quantified type when we are expecting a monotype then create a fresh type variable
  -- with a bound of this quantified type which we will place in our polytype prefix.
  --
  -- NOTE: We don’t inline the quantifiers as bindings of our prefix! That could break scoping and
  -- we wouldn’t be able to print back out the same type.
  AST.QuantifiedType _ _ -> do
    let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
    binding <- Type.Binding name flexibility <$> checkPolytype polarity context type0
    return (counter1, bindings0 |> binding, Type.variable name)

  -- If we see a quantified type when we are expecting a monotype then create a fresh type variable
  -- with a bound of this quantified type which we will place in our polytype prefix.
  --
  -- NOTE: We don’t inline the quantifiers as bindings of our prefix! That could break scoping and
  -- we wouldn’t be able to print back out the same type.
  --
  -- NOTE: We do treat a quantified function type the same as a `QuantifiedType`.
  AST.FunctionType (_ : _) _ _ -> do
    let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
    binding <- Type.Binding name flexibility <$> checkPolytype polarity context type0
    return (counter1, bindings0 |> binding, Type.variable name)

  AST.WrappedType type1 ->
    checkMonotype polarity context counter0 bindings0 type1

  where
    flexibility = Type.polarityFlexibility polarity
    range = AST.typeRange type0

    -- Forces the programmer to provide proof that they reported an error diagnostic. However, we
    -- don’t include the diagnostic in our internal type structure since there is no obvious place
    -- to throw that error at runtime.
    errorType :: Diagnostic -> DiagnosticWriter (FreshCounter, Seq Type.Binding, Monotype)
    errorType _ = do
      let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
      let binding = Type.Binding name flexibility Type.bottom
      return (counter1, bindings0 |> binding, Type.variable name)
