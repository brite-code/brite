{-# LANGUAGE ViewPatterns #-}

module Brite.Semantics.Unify
  ( unify
  ) where

import Brite.Diagnostic
import Brite.Semantics.CheckMonad
import Brite.Semantics.Prefix (Prefix)
import qualified Brite.Semantics.Prefix as Prefix
import Brite.Semantics.TypeNames
import Brite.Semantics.Type (Monotype, MonotypeDescription(..), Polytype, PolytypeDescription(..), Quantifier(..), Flexibility(..))
import qualified Brite.Semantics.Type as Type
import Brite.Semantics.TypeConstruct
import Brite.Syntax.Identifier
import Brite.Syntax.Range
import Brite.Syntax.Snippet (TypeConstructorSnippet(..))
import Data.Foldable (foldlM, traverse_)
import Data.HashTable.ST.Cuckoo (HashTable)
import qualified Data.HashTable.ST.Cuckoo as HashTable
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.STRef

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
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
unify :: UnifyStack -> Prefix s -> Monotype -> Monotype -> Check s (Either Diagnostic ())
unify stack prefix actual expected = do
  exists <- newExistentialAssociation
  unifyMonotype stack prefix exists actual expected

-- Unifies two monotypes.
--
-- IMPORTANT: If you are recursively calling unify make sure to call this function and not `unify`
-- as this function passes around the existential association environment!
unifyMonotype :: UnifyStack -> Prefix s -> ExistentialAssociation s -> Monotype -> Monotype -> Check s (Either Diagnostic ())
unifyMonotype stack prefix exists actual expected = case (Type.monotypeDescription actual, Type.monotypeDescription expected) of
  -- Variables with the same name unify without any further analysis. We rename variables in
  -- `actual` and `expected` so that the same variable name does not represent different bounds.
  (Variable actualName, Variable expectedName) | actualName == expectedName -> return (Right ())

  -- Unify `actual` with some other monotype. If the monotype is a variable then we will perform
  -- some special handling.
  (Variable actualName, expectedDescription) -> do
    -- Lookup the variable’s quantifier. If it does not exist then we have an internal error!
    -- Immediately stop trying to unify this type.
    maybeActualQuantifier <- Prefix.lookup prefix actualName
    case maybeActualQuantifier of
      Nothing -> Left <$> internalExpectedTypeVariableToExist actualName stack
      Just actualQuantifier -> do
        -- Pattern match with our quantifier type and our second monotype.
        case (actualQuantifier, expectedDescription) of
          -- If the bound for our variable is a monotype variable then recursively call `unify` with
          -- that monotype.
          --
          -- The `unify()` algorithm presented in the [MLF thesis][1] requires this case only for
          -- variable monotypes (written as σ ∈ V). However, the proof will allow this shortcut for
          -- any monotype, not just variables. We choose a middle-ground. We shortcut rigid bounds
          -- so that we won’t have to update them in our prefix again. (Which will call
          -- `updateCheck` which might be expensive.)
          --
          -- This case is repeated in three places.
          --
          -- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
          (UniversalQuantifier actualFlexibility (Type.polytypeDescription -> Monotype' actualMonotype), _)
            | Type.isVariableMonotype actualMonotype || actualFlexibility == Rigid ->
              unifyMonotype stack prefix exists actualMonotype expected

          -- Two variables with different names were unified with one another. This case is
          -- different from the variable branch below because we need to merge the two
          -- variables together.
          (_, Variable expectedName) -> do
            -- Lookup the variable’s quantifier. If it does not exist then we have an internal
            -- error! Immediately stop trying to unify this type.
            maybeExpectedQuantifier <- Prefix.lookup prefix expectedName
            case maybeExpectedQuantifier of
              Nothing -> Left <$> internalExpectedTypeVariableToExist expectedName stack
              Just expectedQuantifier ->
                case (actualQuantifier, expectedQuantifier) of
                  -- If the bound for our variable is a monotype variable then recursively call
                  -- `unify` with that monotype.
                  --
                  -- The `unify()` algorithm presented in the [MLF thesis][1] requires this case
                  -- only for variable monotypes (written as σ ∈ V). However, the proof will allow
                  -- this shortcut for any monotype, not just variables. We choose a middle-ground.
                  -- We shortcut rigid bounds so that we won’t have to update them in our prefix
                  -- again. (Which will call `updateCheck` which might be expensive.)
                  --
                  -- This case is repeated in three places.
                  --
                  -- IMPORTANT: Make sure we don’t fall into the next case where we try to update
                  -- the types to each other!
                  --
                  -- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
                  (_, UniversalQuantifier expectedFlexibility (Type.polytypeDescription -> Monotype' expectedMonotype))
                    | Type.isVariableMonotype expectedMonotype || expectedFlexibility == Rigid ->
                      unifyMonotype stack prefix exists actual expectedMonotype

                  -- Merge two universally quantified type variables together.
                  (UniversalQuantifier actualFlexibility actualBound, UniversalQuantifier expectedFlexibility expectedBound) -> do
                    -- Tell `unifyPolytype` to prefer the expected type when our actual quantifier
                    -- has a rigid bound.
                    let preferExpected = actualFlexibility == Rigid && expectedFlexibility == Flexible
                    result <- unifyPolytype stack prefix exists (not preferExpected) actualBound expectedBound
                    case result of
                      -- If the two types are not equivalent we don’t want to merge them together
                      -- in the prefix!
                      Left e -> return (Left e)
                      Right newType -> do
                        -- Our merged quantifier is only flexible if both quantifiers are flexible.
                        let
                          flexibility = case (actualFlexibility, expectedFlexibility) of
                            (Flexible, Flexible) -> Flexible
                            _ -> Rigid
                        -- Merge the two type variables together! Huzzah!
                        Prefix.mergeUpdate stack prefix actualName expectedName flexibility newType

                  -- Unify a type variable with an existential quantifier and a type variable with a
                  -- universal quantifier...
                  (ExistentialQuantifier actualRange, UniversalQuantifier expectedFlexibility expectedBound) ->
                    case (expectedFlexibility, Type.polytypeDescription expectedBound) of
                      -- An existential quantifier could be _any_ type, so we can only update
                      -- universally quantified types with a flexible bottom type bound.
                      (Flexible, Bottom _) -> Prefix.update stack prefix expectedName (Type.polytype actual)

                      -- A rigid bottom type bound expects _exactly_ the bottom type. While an
                      -- existential type may be exactly the bottom type, we can’t be sure so
                      -- return an error.
                      (Rigid, Bottom expectedRangeStack) -> Left <$>
                        doesNotAbstract
                          (actualRange, identifierText actualName)
                          (initialRange expectedRangeStack, bottomTypeName)
                          stack

                      -- At this point, we expect our existential quantifier to fail unification
                      -- with every other type, so unify recursively to get a good error message.
                      (_, Monotype' expectedMonotype) -> unifyMonotype stack prefix exists actual expectedMonotype >>= expectError stack
                      (_, Quantify _ _) -> unifyPolytype stack prefix exists True (Type.polytype actual) expectedBound >>= expectError stack

                  -- Unify a type variable with a universal quantifier and a type variable with an
                  -- existential quantifier...
                  (UniversalQuantifier actualFlexibility actualBound, ExistentialQuantifier expectedRange) ->
                    case (actualFlexibility, Type.polytypeDescription actualBound) of
                      -- An existential quantifier could be _any_ type, so we can only update
                      -- universally quantified types with a flexible bottom type bound.
                      (Flexible, Bottom _) -> Prefix.update stack prefix actualName (Type.polytype expected)

                      -- A rigid bottom type bound expects _exactly_ the bottom type. While an
                      -- existential type may be exactly the bottom type, we can’t be sure so
                      -- return an error.
                      (Rigid, Bottom actualRangeStack) -> Left <$>
                        doesNotAbstract
                          (expectedRange, identifierText expectedName)
                          (initialRange actualRangeStack, bottomTypeName)
                          stack

                      -- At this point, we expect our existential quantifier to fail unification
                      -- with every other type, so unify recursively to get a good error message.
                      (_, Monotype' actualMonotype) -> unifyMonotype stack prefix exists actualMonotype expected >>= expectError stack
                      (_, Quantify _ _) -> unifyPolytype stack prefix exists True actualBound (Type.polytype expected) >>= expectError stack

                  -- We may only unify two existential quantifiers with different names if they were
                  -- quantified at the same level. Then we create an alias for the two quantifiers
                  -- by updating one of the existential quantifiers to point at the other in our
                  -- prefix. If an existentially quantified type was already aliased then our
                  -- existential association check will fail.
                  (ExistentialQuantifier actualRange, ExistentialQuantifier expectedRange) -> do
                    result <- existentialAssociationCheck exists actualName expectedName
                    if result then
                      Prefix.update stack prefix expectedName (Type.polytype actual)
                    else
                      -- If we fail our existential environment association check then we know that
                      -- these two existential quantifiers are incompatible. So make sure to fail
                      -- with an error!
                      Left <$>
                        incompatibleTypes
                          (actualRange, UnknownConstructorSnippet actualName)
                          (expectedRange, UnknownConstructorSnippet expectedName)
                          stack

          -- An existentially quantified type is incompatible with a constructed type. Report an
          -- incompatible types error.
          (ExistentialQuantifier actualRange, Construct expectedConstruct) ->
            Left <$>
              incompatibleTypes
                (actualRange, UnknownConstructorSnippet actualName)
                (initialRange (Type.monotypeRangeStack expected), typeConstructorSnippet expectedConstruct)
                stack

          -- Unify the polymorphic bound type with our other monotype. If the unification is
          -- successful then update the type variable in our prefix to our monotype.
          (UniversalQuantifier _ actualBound, _) -> do
            let expectedPolytype = Type.polytype expected
            result <- unifyPolytype stack prefix exists True actualBound expectedPolytype
            case result of
              Left e -> return (Left e)
              Right _ -> Prefix.update stack prefix actualName expectedPolytype

  -- Unify `expected` with some other monotype. The other monotype is guaranteed to not be a variable
  -- since that case would be caught by the match case above.
  (Construct actualConstruct, Variable expectedName) -> do
    -- Lookup the variable’s quantifier. If it does not exist then we have an internal error!
    -- Immediately stop trying to unify this type.
    maybeExpectedQuantifier <- Prefix.lookup prefix expectedName
    case maybeExpectedQuantifier of
      Nothing -> Left <$> internalExpectedTypeVariableToExist expectedName stack
      Just expectedQuantifier -> do
        -- Pattern match with our quantifier type and our second monotype.
        case expectedQuantifier of
          -- If the bound for our variable is a monotype variable then recursively call `unify` with
          -- that monotype.
          --
          -- The `unify()` algorithm presented in the [MLF thesis][1] requires this case only for
          -- variable monotypes (written as σ ∈ V). However, the proof will allow this shortcut for
          -- any monotype, not just variables. We choose a middle-ground. We shortcut rigid bounds
          -- so that we won’t have to update them in our prefix again. (Which will call
          -- `updateCheck` which might be expensive.)
          --
          -- This case is repeated in three places.
          --
          -- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
          UniversalQuantifier expectedFlexibility (Type.polytypeDescription -> Monotype' expectedMonotype)
            | Type.isVariableMonotype expectedMonotype || expectedFlexibility == Rigid ->
              unifyMonotype stack prefix exists actual expectedMonotype

          -- Unify the polymorphic bound type with our other monotype. If the unification is
          -- successful then update the type variable in our prefix to our monotype.
          UniversalQuantifier _ expectedBound -> do
            let actualPolytype = Type.polytype actual
            result <- unifyPolytype stack prefix exists True actualPolytype expectedBound
            case result of
              Left e -> return (Left e)
              Right _ -> Prefix.update stack prefix expectedName actualPolytype

          -- An existentially quantified type is incompatible with a constructed type. Report an
          -- incompatible types error.
          ExistentialQuantifier expectedRange ->
            Left <$>
              incompatibleTypes
                (initialRange (Type.monotypeRangeStack actual), typeConstructorSnippet actualConstruct)
                (expectedRange, UnknownConstructorSnippet expectedName)
                stack

  -- Unify two constructed types.
  (Construct actualConstruct, Construct expectedConstruct) ->
    case (actualConstruct, expectedConstruct) of
      -- Arity-zero constructors.
      (Void, Void) -> return (Right ())
      (Boolean, Boolean) -> return (Right ())
      (Integer, Integer) -> return (Right ())

      -- Functions unify if the parameters and bodies unify.
      --
      -- If both the unification of the parameters and bodies fail then we will report two error
      -- diagnostics. However, we will only return the first error from unify.
      (Function actualParameter actualBody, Function expectedParameter expectedBody) -> do
        -- If this is a function call operation then reverse the order of the expected and actual
        -- types to reflect that we called `unify` with the actual arguments on the expected side.
        --
        -- NOTE: Manually flipping in `unify` like this isn’t the prettiest solution to this
        -- problem. It also might be a bit brittle. However, it is the most efficient solution I
        -- (Caleb) can come up with at the moment.
        result1 <-
          if isFunctionCall stack then (
            unifyWithFrame functionParameterFrame expectedParameter actualParameter
          ) else (
            unifyWithFrame functionParameterFrame actualParameter expectedParameter
          )
        result2 <- unifyWithFrame functionBodyFrame actualBody expectedBody
        return (result1 `eitherOr` result2)

      -- Unifies two objects together. This gets a bit complicated because our objects are row types
      -- implementing the [“Extensible records with scoped labels”][1] paper.
      --
      -- See Section 6 of that paper for the typing rules we implement here.
      --
      -- At a high level:
      --
      -- 1. First we merge the properties of our two objects to get shared properties and
      --    overflow properties.
      -- 2. We unify all the shared properties together.
      -- 3. If neither object extends anything we report an error for all of the overflown
      --    properties from both objects. One error for each overflown property.
      -- 4. Otherwise we unify the extensions together with the overflown properties.
      --
      -- In step 4 we create a fresh type variable so we also have to take care and ensure the
      -- algorithm always terminates. Introducing fresh type variables introduces the risk of
      -- non-termination in our unification algorithm!
      --
      -- [1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf
      (Object actualProperties actualMaybeExtension, Object expectedProperties expectedMaybeExtension) -> do
        -- Unify all of the shared properties between the two objects. We use `unifyWithFrame` to
        -- add an object property frame to the unification stack.
        --
        -- While any unification may report a diagnostic, we only return the one with the
        -- earliest range.
        result1 <-
          Map.foldlWithKey'
            (\result2M name nameProperties -> result2M >>= \result2 ->
              foldlM
                (\result3 ((_, actualValue), (_, expectedValue)) -> do
                  -- Perform the actual unification here!
                  result4 <- unifyWithFrame (objectPropertyFrame name) actualValue expectedValue
                  return (result3 `eitherOr` result4))
                result2
                nameProperties)
            (return (Right ()))
            sharedProperties

        -- We might need to unify our extensions depending on whether or not we have extensions.
        eitherOr result1 <$> case (actualMaybeExtension, expectedMaybeExtension) of
          -- If we don’t have any extensions then report missing property errors and return.
          (Nothing, Nothing) -> missingPropertyErrors

          -- If one of our objects is an empty object then there are no more properties in that
          -- object, so unifying extensions is pointless. Report missing property errors and return.
          --
          -- NOTE: This is an important check to ensure the unification algorithm terminates!
          -- Otherwise, we will create a new empty object and perform unification of the same form.
          (Nothing, _) | Map.null actualProperties -> missingPropertyErrors
          (_, Nothing) | Map.null expectedProperties -> missingPropertyErrors

          -- If both extensions are variables of the same name then don’t unify them since we know
          -- there will be no more properties.
          --
          -- NOTE: This is an important check to ensure the unification algorithm terminates!
          -- Otherwise, we will create a new type variable and perform a unification of the same
          -- form. More notes on this later.
          (Just (Type.monotypeDescription -> Variable actualName), Just (Type.monotypeDescription -> Variable expectedName))
            | actualName == expectedName -> missingPropertyErrors

          -- Optimization: If we made some progress in this object unification and we only have one
          -- extension then don’t bother with creating a new type variable. Instead, directly unify
          -- the extension and object with overflown properties.
          (Just actualExtension, Nothing) | not (Map.null sharedProperties) ->
            let
              actualOverflowObject =
                if Map.null actualOverflowProperties then actualExtension else
                  Type.object
                    (currentRange (Type.monotypeRangeStack actual))
                    actualOverflowProperties (Just actualExtension)

              expectedOverflowObject =
                Type.object
                  (currentRange (Type.monotypeRangeStack expected))
                  expectedOverflowProperties Nothing
            in
              unifyWithFrame objectExtensionFrame actualOverflowObject expectedOverflowObject

          -- Optimization: If we made some progress in this object unification and we only have one
          -- extension then don’t bother with creating a new type variable. Instead, directly unify
          -- the extension and object with overflown properties.
          (Nothing, Just expectedExtension) | not (Map.null sharedProperties) ->
            let
              actualOverflowObject =
                Type.object
                  (currentRange (Type.monotypeRangeStack actual))
                  actualOverflowProperties Nothing

              expectedOverflowObject =
                if Map.null expectedOverflowProperties then expectedExtension else
                  Type.object
                    (currentRange (Type.monotypeRangeStack expected))
                    expectedOverflowProperties (Just expectedExtension)
            in
              unifyWithFrame objectExtensionFrame actualOverflowObject expectedOverflowObject

          -- Unify extensions if none of the above cases passed. If we have an actual extension then
          -- let’s use its range.
          (Just actualExtension, _) -> unifyExtensions (currentRange (Type.monotypeRangeStack actualExtension))
          (Nothing, Just expectedExtension) -> unifyExtensions (currentRange (Type.monotypeRangeStack expectedExtension))

        where
          -- Merge the object properties into shared and overflow properties. We will unify the
          -- shared properties together and will unify the overflow properties with the
          -- object’s extension.
          (sharedProperties, (actualOverflowProperties, expectedOverflowProperties)) =
            mergeProperties actualProperties expectedProperties

          -- Goes through all the overflowed properties and reports a missing property error. We
          -- perform this action when we’ve successfully unified our properties
          missingPropertyErrors = do
            let
              -- Reports a missing property error for every overflowed property.
              overflowProperty objectType makeDiagnostic result2M name nameProperties = result2M >>= \result2 ->
                foldlM
                  (\result3 (nameRange, _) -> do
                    let objectRange = initialRange (Type.monotypeRangeStack objectType)
                    e <- makeDiagnostic objectRange (nameRange, name) stack
                    return (result3 `eitherOr` Left e))
                  result2
                  nameProperties

            -- Loop through all our overflowed properties and report an error. If there was no
            -- property overflow then these folds will do nothing and return `result1`.
            let result2 = Map.foldlWithKey' (overflowProperty actual missingProperty) (return (Right ())) expectedOverflowProperties
            Map.foldlWithKey' (overflowProperty expected extraProperty) result2 actualOverflowProperties

          -- Unify the two object extension together. Be careful, if called inappropriately it
          -- could cause non-termination problems.
          unifyExtensions sharedExtensionRange =
            -- We will create some type variables so perform the following in a new level. The type
            -- variables we create may escape through updates.
            Prefix.withLevel prefix $ do
              -- Create a new type variable. We need a fresh type variable as a part of the
              -- unification algorithm described in [“Extensible records with scoped labels”][1].
              -- Particularly the rule `(row-var)` in the isomorphic rows relation.
              --
              -- Every time we create a type variable we risk introducing non-termination into our
              -- algorithm. In this case we guard against the non-termination above where we early
              -- return if the extension are two variables with the same name. Here’s why that case
              -- would fail to terminate otherwise.
              --
              -- 1. Say we are unifying `(| p: a | r |)` and `(| q: b | r |)`.
              -- 2. We create a new type variable `t1` and unify `r` with `(| q: b | t1 |)`
              --    according to the code below.
              -- 3. Then we unify `(| p: a | t1 |)` with `r` according to the code below.
              -- 4. Because `r` was unified earlier we now unify `(| p: a | t1 |)` and
              --    `(| q: b | t1 |)`. This is of the same form as step 1. If we
              --    repeat these steps we recurse forever.
              --
              -- We mitigate this non-termination by returning early when both extensions are the
              -- same variable.
              --
              -- [1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf
              sharedExtension <- Prefix.fresh prefix sharedExtensionRange

              let
                -- Use an empty object for the actual extension if there is no extension.
                actualExtension =
                  fromMaybe
                    (Type.emptyObject (currentRange (Type.monotypeRangeStack actual)))
                    actualMaybeExtension

                -- Use an empty object for the expected extension if there is no extension.
                expectedExtension =
                  fromMaybe
                    (Type.emptyObject (currentRange (Type.monotypeRangeStack expected)))
                    expectedMaybeExtension

                -- Create an actual overflow object using our actual overflow properties.
                actualOverflowObject =
                  if Map.null actualOverflowProperties then sharedExtension
                  else Type.object (currentRange (Type.monotypeRangeStack actual)) actualOverflowProperties (Just sharedExtension)

                -- Create an expected overflow object using our expected overflow properties.
                expectedOverflowObject =
                  if Map.null expectedOverflowProperties then sharedExtension
                  else Type.object (currentRange (Type.monotypeRangeStack expected)) expectedOverflowProperties (Just sharedExtension)

              -- Unify each extension with the overflow from the other object. If the extension has
              -- some overflown properties then those properties will be removed and we may perform
              -- the true extension unification.
              result2 <- unifyWithFrame objectExtensionFrame actualExtension expectedOverflowObject
              result3 <- unifyWithFrame objectExtensionFrame actualOverflowObject expectedExtension

              -- Pick our first error and return that.
              return (result2 `eitherOr` result3)

      -- Exhaustive match for failure case. Don’t use a wildcard (`_`) since if we add a new type we
      -- want a compiler warning telling us to add a case for that type to unification.
      (Void, _) -> incompatibleTypesError
      (Boolean, _) -> incompatibleTypesError
      (Integer, _) -> incompatibleTypesError
      (Function _ _, _) -> incompatibleTypesError
      (Object _ _, _) -> incompatibleTypesError
    where
      -- Unifies two types and adds a new unification stack frame to the unification stack.
      unifyWithFrame frame nextActual nextExpected =
        let newStack = frame (currentRange (Type.monotypeRangeStack nextActual)) stack in
          unifyMonotype newStack prefix exists nextActual nextExpected

      -- Report an incompatible types error with both of the types and return it. Use the initial
      -- ranges for our monotypes. This will point directly to where the type was defined.
      incompatibleTypesError = Left <$>
        incompatibleTypes
          (initialRange (Type.monotypeRangeStack actual), typeConstructorSnippet actualConstruct)
          (initialRange (Type.monotypeRangeStack expected), typeConstructorSnippet expectedConstruct)
          stack

-- Unifies two polytypes. When the two types are equivalent we return an ok result with a type. This
-- type is an instance of both our input types. That is if `t1` and `t2` are our inputs and
-- `t1 ≡ t2` holds then we return `t3` where both `t1 ⊑ t3` and `t2 ⊑ t3` hold.
--
-- Takes a boolean flag named `preferActual` which we consult when we have a choice between
-- returning the actual type and the expected type. The type we return _does not_ matter for
-- soundness, but we can improve error messages with the ability to choose.
unifyPolytype :: UnifyStack -> Prefix s -> ExistentialAssociation s -> Bool -> Polytype -> Polytype -> Check s (Either Diagnostic Polytype)
unifyPolytype stack prefix exists preferActual actual expected = case (Type.polytypeDescription actual, Type.polytypeDescription expected) of
  -- If either is bottom then return the other one.
  (Bottom _, _) -> return (Right expected)
  (_, Bottom _) -> return (Right actual)

  -- If we have two monotypes then unify them. Don’t bother with creating a new level
  -- or generalizing.
  (Monotype' actualMonotype, Monotype' expectedMonotype) -> do
    result <- unifyMonotype stack prefix exists actualMonotype expectedMonotype
    case result of
      Left e -> return (Left e)
      Right () -> return (Right (Type.polytype actualMonotype))

  -- When two quantified types unify we instantiate their local bounds in the prefix. We consider a
  -- monotype to be a quantified type with an empty bounds list. We then unify the body of the two
  -- quantified types in the new prefix. If unification is successful then we know that the two
  -- types we unified must be equivalent. We then generalize `actual` and return it. Since we know our
  -- types to be equivalent it does not matter which one we return. If unification returned an error
  -- then we also return that error. We do all this in an isolated level so that we don’t quantify
  -- type variables that are needed at an earlier level.

  (Quantify actualQuantifiers actualBody, Monotype' expectedMonotype) -> Prefix.withLevel prefix $ do
    -- NOTE: We don’t bother updating our existential association environment when unifying with a
    -- monotype since a monotype has no quantifiers to associate with.
    (_, newActualBody) <- Prefix.instantiate prefix actualQuantifiers actualBody
    result <- unifyMonotype stack prefix exists newActualBody expectedMonotype
    case result of
      Left e -> return (Left e)
      -- At this point we know that the two types are equivalent so we may return either one! We
      -- choose to return the monotype.
      --
      -- We return the monotype because we won’t need to generalize the type. All the bounds
      -- instantiated for our quantified type will not be referenced by the monotype because we are
      -- guaranteed that instantiation will only add _new_ bounds to the prefix.
      --
      -- We also return the monotype because it is better for error messages. Some of the bounds in
      -- our quantified type might have been instantiated. So in an error message the user would see
      -- `{p: Int}` for a type they wrote as `{p: !}`, for example. It would be better to point at
      -- the monotype.
      Right () -> return (Right (Type.polytype expectedMonotype))

  (Monotype' actualMonotype, Quantify expectedQuantifiers expectedBody) -> Prefix.withLevel prefix $ do
    -- NOTE: We don’t bother updating our existential association environment when unifying with a
    -- monotype since a monotype has no quantifiers to associate with.
    (_, newExpectedBody) <- Prefix.instantiate prefix expectedQuantifiers expectedBody
    result <- unifyMonotype stack prefix exists actualMonotype newExpectedBody
    case result of
      Left e -> return (Left e)
      -- At this point we know that the two types are equivalent so we may return either one! We
      -- choose to return the monotype.
      --
      -- We return the monotype because we won’t need to generalize the type. All the bounds
      -- instantiated for our quantified type will not be referenced by the monotype because we are
      -- guaranteed that instantiation will only add _new_ bounds to the prefix.
      --
      -- We also return the monotype because it is better for error messages. Some of the bounds in
      -- our quantified type might have been instantiated. So in an error message the user would see
      -- `{p: Int}` for a type they wrote as `{p: !}`, for example. It would be better to point at
      -- the monotype.
      Right () -> return (Right (Type.polytype actualMonotype))

  (Quantify actualQuantifiers actualBody, Quantify expectedQuantifiers expectedBody) -> Prefix.withLevel prefix $ do
    -- Instantiate our quantifiers in the prefix and remember the list of the existential
    -- quantifier names.
    (actualExistentialNames, newActualBody) <- Prefix.instantiate prefix actualQuantifiers actualBody
    (expectedExistentialNames, newExpectedBody) <- Prefix.instantiate prefix expectedQuantifiers expectedBody
    -- Update our existential association environment with the names of the type variables we just
    -- instantiated in our prefix. These type variables may be aliases of each other.
    --
    -- Unify the quantified type bodies with our updated prefix and updated existential
    -- association environment.
    result <- withExistentialAssociations exists actualExistentialNames expectedExistentialNames $
      unifyMonotype stack prefix exists newActualBody newExpectedBody
    -- If unification was successful then generalize the actual type and return it.
    case result of
      Left e -> return (Left e)
      Right () -> Right <$> Prefix.generalize prefix (if preferActual then newActualBody else newExpectedBody)

-- If one of the `Either`s is `Left` then we return `Left`. If both of the `Either`s are `Right` we
-- return `Right`. If both of the `Either`s are `Left` then we return the one which has the earliest
-- diagnostic range.
eitherOr :: Either Diagnostic a -> Either Diagnostic a -> Either Diagnostic a
eitherOr (Right _) x = x
eitherOr x@(Left _) (Right _) = x
eitherOr x1@(Left e1) x2@(Left e2) =
  if rangeStart (diagnosticRange e1) > rangeStart (diagnosticRange e2) then x2
  else x1

-- Expects a unification error. If we don’t get one then we report an internal error instead.
expectError :: UnifyStack -> Either Diagnostic a -> Check s (Either Diagnostic b)
expectError stack (Right _) = Left <$> internalExpectedUnificationError stack
expectError _ (Left e) = return (Left e)

-- An existential association environment ensures that unification between existential quantifiers
-- only happens if it corresponds to a renaming. The environment represents all possible renames
-- and every time we see a rename we check if it is valid and remove the names from our environment.
-- This makes sure we won’t accidentally set up two aliases for the same existential type. After
-- a successful check we unify the aliases so that future unifications of the aliases will pass.
--
-- The existential association environment is defined by the paper [“First-class polymorphism with
-- existential types”][1] Section 7.
--
-- NOTE: In Figure 10 of the existential types paper, the rule U-Mono-L has the condition
-- `(Q, E) τ ~ σ : (Q', _)`. Notice how the existential association is ignored with `_`. I’m (Caleb)
-- pretty sure this is a mistake since no where else in the paper is this addressed. The intuition
-- described for existential associations in section 7.1 doesn’t fit the behavior of this rule where
-- all updates to the environment are dropped. In addition, the soundness proof in Annex B.2 depends
-- on a simple propagation of the existential association environment in the existing MLF rules
-- which this is not. So I will treat this as a mistake and assume instead of `(Q', _)` the author
-- meant to write `(Q', E')` and meant to use that `E'` in the conclusion of the U-Mono-L rule.
--
-- [1]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.120.5025&rep=rep1&type=pdf
newtype ExistentialAssociation s =
  ExistentialAssociation (STRef s [(HashTable s Identifier (), HashTable s Identifier ())])

-- Creates a new existential association environment.
newExistentialAssociation :: Check s (ExistentialAssociation s)
newExistentialAssociation = liftST (ExistentialAssociation <$> newSTRef [])

-- Adds a pair of existential variable names as a new association set before executing the action.
-- Removes the association after the action finishes executing.
--
-- Checks to make sure none of the quantified existentials escaped from the current level in
-- our prefix. We assume that all of the provided existential type variable names were freshly
-- instantiated in our current prefix level.
withExistentialAssociations :: ExistentialAssociation s -> Seq Identifier -> Seq Identifier -> Check s a -> Check s a
withExistentialAssociations (ExistentialAssociation assoc) names1 names2 action =
  -- If either sequence is empty then execute our action without adding a new association. There
  -- must be at least one name in both sets if we want to associate existential type variables with
  -- each other.
  if Seq.null names1 || Seq.null names2 then action else do
    -- Create two existential variable name sets allocating enough size to fit all our names.
    set1 <- liftST $ HashTable.newSized (Seq.length names1)
    set2 <- liftST $ HashTable.newSized (Seq.length names2)
    -- Insert all the existential variable names into the appropriate set.
    liftST $ traverse_ (flip (HashTable.insert set1) ()) names1
    liftST $ traverse_ (flip (HashTable.insert set2) ()) names2
    -- Push the sets to our existential association environment. Pop the sets off our existential
    -- association environment after the action has finished executing. In this way, by the end of
    -- unification we should have an empty existential association environment since we scope our
    -- association sets.
    liftST $ modifySTRef assoc ((set1, set2) :)
    result <- action
    liftST $ modifySTRef assoc tail
    -- Return the result of our action.
    return result

-- Checks if two existential type variables are allowed to unify together. After we check this we
-- remove the type variables from `ExistentialAssociation` ensuring that the existential quantifiers
-- unify one-to-one.
existentialAssociationCheck :: ExistentialAssociation s -> Identifier -> Identifier -> Check s Bool
existentialAssociationCheck (ExistentialAssociation assoc0) name1 name2 = liftST (readSTRef assoc0 >>= loop)
  where
    loop [] = return False
    loop ((set1, set2) : assoc1) = do
      ifM
        ((isJust <$> HashTable.lookup set1 name1) `andM` (isJust <$> HashTable.lookup set2 name2))
        (HashTable.delete set1 name1 *> HashTable.delete set2 name2 *> return True)
        (ifM
          ((isJust <$> HashTable.lookup set1 name2) `andM` (isJust <$> HashTable.lookup set2 name1))
          (HashTable.delete set1 name2 *> HashTable.delete set2 name1 *> return True)
          (loop assoc1))

-- A conditional “if” expression but on monads.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM a x y = a >>= \b -> if b then x else y
{-# INLINE ifM #-}

-- Boolean “and” but on monads. The second monad will not be executed if the first monad
-- returns false.
andM :: Monad m => m Bool -> m Bool -> m Bool
andM a b = a >>= \c -> if c then b else return c
{-# INLINE andM #-}
