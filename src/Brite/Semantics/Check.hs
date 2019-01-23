-- Checks an AST for type errors. Converting that AST into an internal, typed, representation.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.Check
  ( checkType
  ) where

import Brite.Semantics.AST (Identifier)
import qualified Brite.Semantics.AST as AST
import Brite.Semantics.AVT
import Brite.Semantics.CheckMonad
import Brite.Semantics.Type (Polytype, Monotype)
import qualified Brite.Semantics.Type as Type
import Brite.Syntax.Tokens (unsafeIdentifier)
import Data.Foldable (toList)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)
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
checkExpression :: AST.Expression -> Expression
checkExpression astExpression = error "TODO"

type Context = HashMap Identifier Monotype

-- Checks a type and converts it into an internal polytype representation.
checkType :: AST.Type -> Check s Polytype
checkType type' = checkPolytype initialContext Positive type'
  where
    initialContext = HashMap.fromList
      [ (unsafeIdentifier "Bool", Type.boolean)
      , (unsafeIdentifier "Int", Type.integer)
      ]

-- Checks an AST type and turns it into a polytype.
checkPolytype :: Context -> Polarity -> AST.Type -> Check s Polytype
checkPolytype context0 polarity type0 = case AST.typeNode type0 of
  -- Lookup the variable type in our context. If we don’t find it then return a “variable not found”
  -- error type.
  AST.VariableType name -> return $ Type.polytype $
    fromMaybe (Type.variableNotFoundError name) (HashMap.lookup name context0)

  AST.BottomType -> return Type.bottom

  -- Check a function type with its quantifiers.
  AST.FunctionType quantifiers [uncheckedParameterType] uncheckedBodyType -> do
    (context1, bindings1) <- checkQuantifiers context0 polarity quantifiers Seq.empty
    (bindings2, parameterType) <- checkMonotype context1 (flipPolarity polarity) bindings1 uncheckedParameterType
    (bindings3, bodyType) <- checkMonotype context1 polarity bindings2 uncheckedBodyType
    return (Type.quantify (toList bindings3) (Type.function parameterType bodyType))

  -- Check the quantifiers of a quantified type. If the body is also a quantified type then we will
  -- inline those bindings into our prefix as well.
  AST.QuantifiedType quantifiers uncheckedBodyType -> do
    (context1, bindings1) <- checkQuantifiers context0 polarity quantifiers Seq.empty
    (bindings2, bodyType) <- checkMonotype context1 polarity bindings1 uncheckedBodyType
    return (Type.quantify (toList bindings2) bodyType)

  AST.WrappedType type1 -> checkPolytype context0 polarity type1

-- Check all the AST type quantifiers and convert them into a list of bindings.
checkQuantifiers :: Context -> Polarity -> [AST.Quantifier] -> Seq Type.Binding -> Check s (Context, Seq Type.Binding)
checkQuantifiers context0 _ [] bindings = return (context0, bindings)
checkQuantifiers context0 polarity (AST.Quantifier name bound : quantifiers) bindings = do
  -- Get a fresh type variable ID for our binding.
  id' <- freshTypeVariable
  -- Create the binding. If no bound was provided in the AST then we use a flexible, bottom
  -- type, bound. Otherwise we need to check our bound type with the current context.
  binding <- case bound of
    Nothing -> return (Type.Binding id' (Just name) Type.Flexible Type.bottom)
    Just (flexibility, boundType) ->
      Type.Binding id' (Just name) flexibility <$> checkPolytype context0 polarity boundType
  -- Introduce our new type variable ID into our context. Notably introduce our type variable
  -- after checking our binding type.
  let context1 = HashMap.insert (AST.nameIdentifier name) (Type.variable id') context0
  -- Add our binding and process the remaining quantifiers in our new context.
  checkQuantifiers context1 polarity quantifiers (bindings |> binding)

checkMonotype :: Context -> Polarity -> Seq Type.Binding -> AST.Type -> Check s (Seq Type.Binding, Monotype)
checkMonotype context polarity bindings0 type0 = case AST.typeNode type0 of
  -- Lookup the variable type in our context. If we don’t find it then return a “variable not found”
  -- error type.
  AST.VariableType name -> return $ (,) bindings0 $
    fromMaybe (Type.variableNotFoundError name) (HashMap.lookup name context)

  -- If we see a bottom type when we are expecting a monotype then create a fresh type variable
  -- which we will place in our polytype prefix.
  AST.BottomType -> do
    id' <- freshTypeVariable
    let binding = Type.Binding id' Nothing polarityFlexibility Type.bottom
    return (bindings0 |> binding, Type.variable id')

  -- Check the parameter and body type of a function.
  AST.FunctionType [] [uncheckedParameterType] uncheckedBodyType -> do
    (bindings1, parameterType) <- checkMonotype context (flipPolarity polarity) bindings0 uncheckedParameterType
    (bindings2, bodyType) <- checkMonotype context polarity bindings1 uncheckedBodyType
    return (bindings2, Type.function parameterType bodyType)

  -- If we see a quantified type when we are expecting a monotype then create a fresh type variable
  -- with a bound of this quantified type which we will place in our polytype prefix.
  --
  -- NOTE: We don’t inline the quantifiers as bindings of our prefix! That could break scoping and
  -- we wouldn’t be able to print back out the same type.
  AST.QuantifiedType _ _ -> do
    id' <- freshTypeVariable
    binding <- Type.Binding id' Nothing polarityFlexibility <$> checkPolytype context polarity type0
    return (bindings0 |> binding, Type.variable id')

  -- If we see a quantified type when we are expecting a monotype then create a fresh type variable
  -- with a bound of this quantified type which we will place in our polytype prefix.
  --
  -- NOTE: We don’t inline the quantifiers as bindings of our prefix! That could break scoping and
  -- we wouldn’t be able to print back out the same type.
  --
  -- NOTE: We do treat a quantified function type the same as a `QuantifiedType`.
  AST.FunctionType (_ : _) _ _ -> do
    id' <- freshTypeVariable
    binding <- Type.Binding id' Nothing polarityFlexibility <$> checkPolytype context polarity type0
    return (bindings0 |> binding, Type.variable id')

  AST.WrappedType type1 ->
    checkMonotype context polarity bindings0 type1

  where
    polarityFlexibility = case polarity of
      Negative -> Type.Rigid
      Positive -> Type.Flexible

-- Polarity represents whether a type is in an “input” or an “output” position. You may also know
-- polarity as “variance”.
--
-- Consider the type `fun(T) -> U`. Here `T` is in an _input_ position so we give it a negative
-- polarity. However, `U` is in an _output_ position so we give it a positive polarity.
--
-- A negative position in a negative position is a positive position. Consider the type
-- `fun(fun(T) -> U) -> V`. Here, like before, `V` has a positive polarity. `U` has a negative
-- polarity because it it is in a negative position (function parameter) and a positive position
-- (function return) so a negative times a positive is a negative. `T`, however, has a positive
-- polarity because it is in a negative position (function parameter) and a negative position again
-- (function parameter of a function parameter) so a negative times a negative is a positive.
data Polarity = Positive | Negative

-- Flips the polarity from positive to negative and vice-versa.
flipPolarity :: Polarity -> Polarity
flipPolarity Positive = Negative
flipPolarity Negative = Positive
