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
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
checkExpression :: AST.Expression -> Check s Expression
checkExpression astExpression =
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
checkPolytype :: HashSet Identifier -> AST.Type -> DiagnosticWriter Polytype
checkPolytype context0 type0 = case AST.typeNode type0 of
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
    (context1, bindings1) <- checkQuantifiers context0 quantifiers Seq.empty
    let counter0 = initialFreshCounter
    (counter1, bindings2, parameterType) <- checkMonotype Type.Negative context1 counter0 bindings1 uncheckedParameterType
    (_, bindings3, bodyType) <- checkMonotype Type.Positive context1 counter1 bindings2 uncheckedBodyType
    return (Type.quantify (toList bindings3) (Type.function parameterType bodyType))

  -- Check the quantifiers of a quantified type. If the body is also a quantified type then we will
  -- inline those bindings into our prefix as well.
  AST.QuantifiedType quantifiers uncheckedBodyType -> do
    (context1, bindings1) <- checkQuantifiers context0 quantifiers Seq.empty
    (_, bindings2, bodyType) <- checkMonotype Type.Positive context1 initialFreshCounter bindings1 uncheckedBodyType
    return (Type.quantify (toList bindings2) bodyType)

  AST.WrappedType type1 -> checkPolytype context0 type1

  -- Error types which don’t have a recovered type return error types.
  AST.ErrorType diagnostic Nothing -> errorType diagnostic

  -- Error types which do have a recovered type ignore their diagnostic (there’s no point in keeping
  -- it, where would we throw it?) and continue type checking with the recovered type.
  AST.ErrorType _ (Just type1) ->
    checkPolytype context0 (type0 { AST.typeNode = type1 })

  where
    range = AST.typeRange type0

    -- Forces the programmer to provide proof that they reported an error diagnostic. However, we
    -- don’t include the diagnostic in our internal type structure since there is no obvious place
    -- to throw that error at runtime.
    errorType :: Diagnostic -> DiagnosticWriter Polytype
    errorType _ = return Type.bottom

-- Check all the AST type quantifiers and convert them into a list of bindings.
checkQuantifiers ::
  HashSet Identifier -> [AST.Quantifier] -> Seq Type.Binding ->
    DiagnosticWriter (HashSet Identifier, Seq Type.Binding)
checkQuantifiers context0 [] bindings = return (context0, bindings)
checkQuantifiers context0 (AST.Quantifier name bound : quantifiers) bindings = do
  -- Create the binding. If no bound was provided in the AST then we use a flexible, bottom
  -- type, bound. Otherwise we need to check our bound type with the current context.
  binding <- case bound of
    Nothing -> return (Type.Binding (AST.nameIdentifier name) Type.Flexible Type.bottom)
    Just (flexibility, boundType) ->
      Type.Binding (AST.nameIdentifier name) flexibility <$> checkPolytype context0 boundType
  -- Introduce our new type variable ID into our context. Notably introduce our type variable
  -- after checking our binding type.
  let context1 = HashSet.insert (AST.nameIdentifier name) context0
  -- Add our binding and process the remaining quantifiers in our new context.
  checkQuantifiers context1 quantifiers (bindings |> binding)

-- Checks a type expecting that we return a monotype. However, in our type syntax we can have nested
-- polytypes but we can’t have that in our internal representation of types.
--
-- In [MLF][1] all quantifiers are forced to the top level of a type. That is the object type
-- `{identity: fun<T>(T) -> T}` is invalid because the quantifier `<T>` is _inside_ the object type.
-- In MLF this example must be written as `<F: fun<T>(T) -> T> {identity: F}`. Writing all
-- quantified types in this way is really inconvenient. The programmer can’t think about their
-- polymorphism locally.
--
-- Brite implements a heuristic first proposed in [“Qualified Types for MLF”][2] which allows us to
-- write polymorphic types inline. A polymorphic type inside a function parameter is given a rigid
-- bound (`=`) and a polymorphic type anywhere else is given a flexible bound (`:`). So
-- `fun(fun<T>(T) -> T) -> void` would be translated to the MLF type
-- `<F = fun<T>(T) -> T> fun(F) -> void` but `fun() -> fun<T>(T) -> T` would be translated to the
-- MLF type `<F: fun<T>(T) -> T> fun() -> F`.
--
-- This heuristic raises the question of how we should handle function parameters inside of function
-- parameters. For example, if `T` in `fun(fun(T) -> void) -> void` had a quantifier should we hoist
-- `T` to a rigid (`=`) or flexible (`:`) bound? If we say a function parameter inside a function
-- parameter cancels each other out then we’d give `T` a rigid (`=`) bound. However, this means our
-- rule requires context to be correct.
--
-- Consider `<F = fun(T) -> void> fun(F) -> void`. Here, we know that `T` is inside a function
-- parameter, but we don’t know that `F` is inside another function parameter. Implementing this
-- interpretation of the heuristic would not be straightforward. So instead we implement the
-- heuristic locally.
--
-- We only look at the nearest type constructor to determine the bound flexibility. So in
-- `fun(fun(T) -> void) -> void` we would give `T` a rigid bound (`=`) since, locally, it is inside
-- a function parameter.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
-- [2]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/qmlf.pdf
checkMonotype ::
  Type.Polarity -> HashSet Identifier -> FreshCounter -> Seq Type.Binding -> AST.Type ->
    DiagnosticWriter (FreshCounter, Seq Type.Binding, Monotype)
checkMonotype localPolarity context counter0 bindings0 type0 = case AST.typeNode type0 of
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
    let binding = Type.Binding name localFlexibility Type.bottom
    return (counter1, bindings0 |> binding, Type.variable name)

  -- Check the parameter and body type of a function.
  AST.FunctionType [] [uncheckedParameterType] uncheckedBodyType -> do
    (counter1, bindings1, parameterType) <- checkMonotype Type.Negative context counter0 bindings0 uncheckedParameterType
    (counter2, bindings2, bodyType) <- checkMonotype Type.Positive context counter1 bindings1 uncheckedBodyType
    return (counter2, bindings2, Type.function parameterType bodyType)

  -- If we see a quantified type when we are expecting a monotype then create a fresh type variable
  -- with a bound of this quantified type which we will place in our polytype prefix.
  --
  -- NOTE: We don’t inline the quantifiers as bindings of our prefix! That could break scoping and
  -- we wouldn’t be able to print back out the same type.
  AST.QuantifiedType _ _ -> do
    let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
    binding <- Type.Binding name localFlexibility <$> checkPolytype context type0
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
    binding <- Type.Binding name localFlexibility <$> checkPolytype context type0
    return (counter1, bindings0 |> binding, Type.variable name)

  AST.WrappedType type1 ->
    checkMonotype localPolarity context counter0 bindings0 type1

  -- Error types which don’t have a recovered type return error types.
  AST.ErrorType diagnostic Nothing -> errorType diagnostic

  -- Error types which do have a recovered type ignore their diagnostic (there’s no point in keeping
  -- it, where would we throw it?) and continue type checking with the recovered type.
  AST.ErrorType _ (Just type1) ->
    checkMonotype localPolarity context counter0 bindings0 (type0 { AST.typeNode = type1 })

  where
    range = AST.typeRange type0

    localFlexibility = case localPolarity of
      Type.Positive -> Type.Flexible
      Type.Negative -> Type.Rigid

    -- Forces the programmer to provide proof that they reported an error diagnostic. However, we
    -- don’t include the diagnostic in our internal type structure since there is no obvious place
    -- to throw that error at runtime.
    errorType :: Diagnostic -> DiagnosticWriter (FreshCounter, Seq Type.Binding, Monotype)
    errorType _ = do
      let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
      let binding = Type.Binding name localFlexibility Type.bottom
      return (counter1, bindings0 |> binding, Type.variable name)
