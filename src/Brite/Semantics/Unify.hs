module Brite.Semantics.Unify
  ( unify
  ) where

import Brite.Diagnostic
import Brite.Semantics.CheckMonad
import Brite.Semantics.Prefix (Prefix)
import qualified Brite.Semantics.Prefix as Prefix
import Brite.Semantics.Type (Monotype, MonotypeDescription(..), Polytype, PolytypeDescription(..))
import qualified Brite.Semantics.Type as Type

-- IMPORTANT: It is expected that all free type variables are bound in the prefix! If this is not
-- true we will report internal error diagnostics.
--
-- The unification algorithm takes a prefix and two monotypes asserting that the types are
-- equivalent under the prefix. However, what makes unification extra special is that at the same
-- time if we encounter flexible bounds in the prefix then we update our prefix so that the types
-- become equivalent under the prefix.
--
-- In the terms of the [MLF thesis][1] unification is defined as:
--
-- > Definition 4.1.1 (Unification): A prefix `Q'` unifies `t1` and `t2` under `Q` if and only if
-- > `Q ⊑ Q'` and `(Q') t1 ≡ t2` hold.
--
-- Our implementation is a bit different then the algorithm proposed in the thesis. Our algorithm
-- _never fails_. Instead our algorithm returns a result. If the result is ok then the two types are
-- equivalent. That is `(Q') t1 ≡ t2` holds. If our algorithm result is an error then the two types
-- are _not_ equivalent. That is `(Q') t1 ≡ t2` does _not_ hold. The prefix after unification will
-- always be an instance of the prefix before unification. That is `Q ⊑ Q'` always holds.
--
-- If the caller of the unification algorithm depends on equivalent types for soundness then they
-- must handle the error locally. Typically this is done by panicking at runtime. That is if the
-- types are not equivalent then we will use a fresh type variable which will always be equivalent
-- when unified.
--
-- NOTE: While the order of the types does not matter for unification correctness, we should take
-- care to keep the order the same. To never switch the types. This is because error reporting
-- _does_ depend on the order of types to provide a better error message.
--
-- For the purpose of error reporting, the first type in a unification is the “actual” type. The
-- type of a value in source code that we are comparing against the second type, the “expected”
-- type, which could be some type annotation the programmer wrote.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
unify :: UnifyStack -> Prefix s -> Monotype -> Monotype -> Check s (Either Diagnostic ())
unify stack prefix type1 type2 = case (Type.monotypeDescription type1, Type.monotypeDescription type2) of
  -- Variables with the same name unify without any further analysis. We rename variables in
  -- `type1` and `type2` so that the same variable name does not represent different bounds.
  (Variable name1, Variable name2) | name1 == name2 -> return (Right ())

  -- Unify `type1` with some other monotype. If the monotype is a variable then we will perform
  -- some special handling.
  (Variable name1, description2) -> do
    -- Lookup the variable’s binding. If it does not exist then we have an internal error!
    -- Immediately stop trying to unify this type.
    maybeBinding1 <- Prefix.lookup prefix name1
    case maybeBinding1 of
      Nothing -> error "TODO: diagnostic"
      Just binding1 -> do
        -- Pattern match with our binding type and our second monotype.
        case (Type.polytypeDescription (Type.bindingType binding1), description2) of
          -- If the bound for our variable is a monotype then recursively call `unify` with that
          -- monotype. As per the normal-form monotype bound rewrite rule.
          (Monotype' monotype1, _) -> unify stack prefix monotype1 type2

          -- Two variables with different names were unified with one another. This case is
          -- different from the variable branch below because we need to merge the two
          -- variables together.
          (_, Variable name2) -> do
            -- Lookup the variable’s binding. If it does not exist then we have an internal error!
            -- Immediately stop trying to unify this type.
            maybeBinding2 <- Prefix.lookup prefix name2
            case maybeBinding2 of
              Nothing -> error "TODO: diagnostic"
              Just binding2 ->
                case Type.polytypeDescription (Type.bindingType binding2) of
                  -- If the bound for our variable is a monotype then recursively call `unify`
                  -- with that monotype. As per the normal-form monotype bound rewrite rule. Make
                  -- sure we don’t fall into the next case where we try to update the types to
                  -- each other!
                  Monotype' monotype2 -> unify stack prefix type1 monotype2

                  -- Actually merge the two type variables together.
                  _ -> do
                    result <- unifyPolytype stack prefix (Type.bindingType binding1) (Type.bindingType binding2)
                    case result of
                      -- If the two types are not equivalent we don’t want to merge them together
                      -- in the prefix!
                      Left e -> return (Left e)
                      Right newType -> do
                        let
                          -- Our merged binding is only flexible if both bindings are flexible.
                          flexibility = case (Type.bindingFlexibility binding1, Type.bindingFlexibility binding2) of
                            (Type.Flexible, Type.Flexible) -> Type.Flexible
                            _ -> Type.Rigid
                        -- Merge the two type variables together! Huzzah!
                        Prefix.mergeUpdate prefix name1 name2 flexibility newType

          -- Unify the polymorphic bound type with our other monotype. If the unification is
          -- successful then update the type variable in our prefix to our monotype.
          _ -> do
            let polytype2 = Type.polytype type2
            result <- unifyPolytype stack prefix (Type.bindingType binding1) polytype2
            case result of
              Left e -> return (Left e)
              Right _ -> Prefix.update prefix (Type.Binding name1 Type.Rigid polytype2)

  -- Unify `type2` with some other monotype. The other monotype is guaranteed to not be a variable
  -- since that case would be caught by the match case above.
  (_, Variable name2) -> do
    -- Lookup the variable’s binding. If it does not exist then we have an internal error!
    -- Immediately stop trying to unify this type.
    maybeBinding2 <- Prefix.lookup prefix name2
    case maybeBinding2 of
      Nothing -> error "TODO: diagnostic"
      Just binding2 -> do
        -- Pattern match with our binding type and our second monotype.
        case Type.polytypeDescription (Type.bindingType binding2) of
          -- If the bound for our variable is a monotype then recursively call `unify` with that
          -- monotype. As per the normal-form monotype bound rewrite rule.
          Monotype' monotype2 -> unify stack prefix type1 monotype2

          -- Unify the polymorphic bound type with our other monotype. If the unification is
          -- successful then update the type variable in our prefix to our monotype.
          _ -> do
            let polytype1 = Type.polytype type1
            result <- unifyPolytype stack prefix polytype1 (Type.bindingType binding2)
            case result of
              Left e -> return (Left e)
              Right _ -> Prefix.update prefix (Type.Binding name2 Type.Rigid polytype1)

  -- Primitive intrinsic types unify with each other no problem.
  (Boolean, Boolean) -> return (Right ())
  (Integer, Integer) -> return (Right ())

  -- Functions unify if the parameters and bodies unify.
  --
  -- If both the unification of the parameters and bodies fail then we will report two error
  -- diagnostics. However, we will only return the first error from unify.
  (Function parameter1 body1, Function parameter2 body2) -> do
    let parameterFrame = functionParameterFrame (Type.monotypeRange parameter1) stack
    let bodyFrame = functionBodyFrame (Type.monotypeRange body1) stack
    result1 <- unify parameterFrame prefix parameter1 parameter2
    result2 <- unify bodyFrame prefix body1 body2
    return (result1 `eitherOr` result2)

  -- Exhaustive match for failure case. Don’t use a wildcard (`_`) since if we add a new type we
  -- want a compiler warning telling us to add a case for that type to unification.
  (Boolean, _) -> incompatibleTypesError
  (Integer, _) -> incompatibleTypesError
  (Function _ _, _) -> incompatibleTypesError

  where
    -- Report an incompatible types error with both of the types and return it.
    incompatibleTypesError = Left <$>
      incompatibleTypes (Type.monotypeRange type1) (typeMessage type1) (typeMessage type2) stack

    typeMessage type3 = case Type.monotypeDescription type3 of
      -- All variables should be handled by unification. No matter what they unify to.
      Variable _ -> undefined

      Boolean -> BooleanMessage
      Integer -> IntegerMessage
      Function _ _ -> FunctionMessage

-- Unifies two polytypes. When the two types are equivalent we return an ok result with a type. This
-- type is an instance of both our input types. That is if `t1` and `t2` are our inputs and
-- `t1 ≡ t2` holds then we return `t3` where both `t1 ⊑ t3` and `t2 ⊑ t3` hold.
unifyPolytype :: UnifyStack -> Prefix s -> Polytype -> Polytype -> Check s (Either Diagnostic Polytype)
unifyPolytype stack prefix type1 type2 = case (Type.polytypeDescription type1, Type.polytypeDescription type2) of
  -- If either is bottom then return the other one.
  (Bottom _, _) -> return (Right type2)
  (_, Bottom _) -> return (Right type1)

  -- If we have two monotypes then unify them. Don’t bother with creating a new level
  -- or generalizing.
  (Monotype' monotype1, Monotype' monotype2) -> do
    result <- unify stack prefix monotype1 monotype2
    case result of
      Left e -> return (Left e)
      Right () -> return (Right (Type.polytype monotype1))

  -- When two quantified types unify we instantiate their local bounds in the prefix. We consider a
  -- monotype to be a quantified type with an empty bounds list. We then unify the body of the two
  -- quantified types in the new prefix. If unification is successful then we know that the two
  -- types we unified must be equivalent. We then generalize type1 and return it. Since we know our
  -- types to be equivalent it does not matter which one we return. If unification returned an error
  -- then we also return that error. We do all this in an isolated level so that we don’t quantify
  -- type variables that are needed at an earlier level.

  (Quantify bindings1 body1, Monotype' monotype2) -> Prefix.withLevel prefix $ do
    newBody1 <- Prefix.instantiate prefix bindings1 body1
    result <- unify stack prefix newBody1 monotype2
    case result of
      Left e -> return (Left e)
      Right () -> Right <$> Prefix.generalize prefix newBody1

  (Monotype' monotype1, Quantify bindings2 body2) -> Prefix.withLevel prefix $ do
    newBody2 <- Prefix.instantiate prefix bindings2 body2
    result <- unify stack prefix monotype1 newBody2
    case result of
      Left e -> return (Left e)
      Right () -> Right <$> Prefix.generalize prefix newBody2

  (Quantify bindings1 body1, Quantify bindings2 body2) -> Prefix.withLevel prefix $ do
    newBody1 <- Prefix.instantiate prefix bindings1 body1
    newBody2 <- Prefix.instantiate prefix bindings2 body2
    result <- unify stack prefix newBody1 newBody2
    case result of
      Left e -> return (Left e)
      Right () -> Right <$> Prefix.generalize prefix newBody1

-- If the first `Either` is `Right` we return the second. If the first `Either` is `Left` we return
-- the first. So we return the first `Either` with an error.
eitherOr :: Either a b -> Either a b -> Either a b
eitherOr (Right _) x = x
eitherOr x@(Left _) _ = x
