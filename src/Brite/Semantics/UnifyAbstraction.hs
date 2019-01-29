-- Implements the abstraction-check algorithm which is an auxillary algorithm of unification defined
-- by the [MLF thesis][1] in Section 4.2.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Brite.Semantics.UnifyAbstraction () where

import Brite.Semantics.CheckMonad
import Brite.Semantics.Type (Polytype, PolytypeDescription(..), Monotype, MonotypeDescription(..))
import qualified Brite.Semantics.Type as Type
import Brite.Syntax.Tokens (Identifier)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromJust)

-- A data-type is needed here to make the type recursive.
newtype Locals = Locals { getLocals :: HashMap Identifier (Locals, Polytype) }

-- Checks if the projections of two types are equal. The projection of a type is the type with all
-- quantifications inlined no matter the flexibility of the quantification bound. The projection
-- function is written in Definition 1.3.2 of the [MLF thesis][1]. This function efficiently
-- compares the projection of two types under a prefix and returns true if the projections are equal
-- to one another.
--
-- Multiple calls to the prefix function should return the exact same binding. If the prefix
-- function does not return a binding when we call it then we treat the type variable as opaque. It
-- will only match against unbound type variables with the exact same name.
--
--
-- This function is used to implement abstraction checks. Since according to Property 2.1.3 (i) of
-- the instance relation if one type abstracts another then the types have the same skeletons.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
projectionsEqual :: forall s. (Identifier -> Check s (Maybe Type.Binding)) -> Polytype -> Polytype -> Check s Bool
projectionsEqual prefix = error "unimplemented"
  where
    monotypeProjectionsEqual :: Locals -> Locals -> Monotype -> Monotype -> Check s Bool
    monotypeProjectionsEqual locals1 locals2 type1 type2 =
      case (Type.monotypeDescription type1, Type.monotypeDescription type2) of
        -- Unwrap variable from locals. Note that each local uses its own locals map. This is so we
        -- can correctly check types with shadowed bounds like `∀(x, x = x).x ≡ ⊥`.
        (Variable name1, _) | HashMap.member name1 (getLocals locals1) ->
          let (locals3, type3) = fromJust (HashMap.lookup name1 (getLocals locals1)) in
            polytypeProjectionsEqual locals3 locals2 type3 (Type.polytype type2)

        -- Unwrap variable from locals. Note that each local uses its own locals map. This is so we
        -- can correctly check types with shadowed bounds like `∀(x, x = x).x ≡ ⊥`.
        (_, Variable name2) | HashMap.member name2 (getLocals locals2) ->
          let (locals3, type3) = fromJust (HashMap.lookup name2 (getLocals locals2)) in
            polytypeProjectionsEqual locals1 locals3 (Type.polytype type1) type3

        -- Optimization: If neither variable exists as a local type and the names equal each other
        -- then they are equal to the same type in the prefix. It doesn’t matter whether or not they
        -- both exist in the prefix.
        (Variable name1, Variable name2) | name1 == name2 ->
          return True

        -- Unwrap variable from prefix. Note that we set an empty locals map when we use a variable
        -- from the prefix. This is to avoid shadowed bounds like in the
        -- relation `(x) ∀(x = x).x ≡ ⊥`
        (Variable name1, _) -> do
          maybeBinding1 <- prefix name1
          case maybeBinding1 of
            Nothing -> return False
            Just binding1 ->
              polytypeProjectionsEqual (Locals HashMap.empty) locals2 (Type.bindingType binding1) (Type.polytype type2)

        -- Unwrap variable from prefix. Note that we set an empty locals map when we use a variable
        -- from the prefix. This is to avoid shadowed bounds like in the
        -- relation `(x) ∀(x = x).x ≡ ⊥`
        (_, Variable name2) -> do
          maybeBinding2 <- prefix name2
          case maybeBinding2 of
            Nothing -> return False
            Just binding2 ->
              polytypeProjectionsEqual locals1 (Locals HashMap.empty) (Type.polytype type1) (Type.bindingType binding2)

        -- Two functions are only equivalent if both their parameters and bodies are equivalent.
        (Function parameter1 body1, Function parameter2 body2) ->
          (&&)
            <$> monotypeProjectionsEqual locals1 locals2 parameter1 parameter2
            <*> monotypeProjectionsEqual locals1 locals2 body1 body2

        -- Scalars are only equivalent with each other.
        (Boolean, Boolean) -> return True
        (Integer, Integer) -> return True

        -- Exhaustive failure cases.
        (Boolean, _) -> return False
        (Integer, _) -> return False
        (Function _ _, _) -> return False

    polytypeProjectionsEqual :: Locals -> Locals -> Polytype -> Polytype -> Check s Bool
    polytypeProjectionsEqual locals1 locals2 type1 type2 =
      case (Type.polytypeDescription type1, Type.polytypeDescription type2) of
        -- NOTE: This section for matching `Variable` is copied from `monotypeProjectionsEqual`
        -- above. We copy it since we need to unwrap variables _before_ some of our polytype cases
        -- like the `Bottom` case. Consider the equivalence `∀a.a ≡ ∀b.b`. Here `proj(∀a.a) = ⊥` and
        -- `proj(∀b.b) = ⊥`. Since `⊥ = ⊥` our relation `∀a.a ≡ ∀b.b` holds. But in order for our
        -- function to produce this result we need to unwrap variables before our `Bottom` case.

        -- Unwrap variable from locals. Note that each local uses its own locals map. This is so we
        -- can correctly check types with shadowed bounds like `∀(x, x = x).x ≡ ⊥`.
        (Monotype' (Type.monotypeDescription -> Variable name1), _) | HashMap.member name1 (getLocals locals1) ->
          let (locals3, type3) = fromJust (HashMap.lookup name1 (getLocals locals1)) in
            polytypeProjectionsEqual locals3 locals2 type3 type2

        -- Unwrap variable from locals. Note that each local uses its own locals map. This is so we
        -- can correctly check types with shadowed bounds like `∀(x, x = x).x ≡ ⊥`.
        (_, Monotype' (Type.monotypeDescription -> Variable name2)) | HashMap.member name2 (getLocals locals2) ->
          let (locals3, type3) = fromJust (HashMap.lookup name2 (getLocals locals2)) in
            polytypeProjectionsEqual locals1 locals3 type1 type3

        -- Optimization: If neither variable exists as a local type and the names equal each other
        -- then they are equal to the same type in the prefix. It doesn’t matter whether or not they
        -- both exist in the prefix.
        (Monotype' (Type.monotypeDescription -> Variable name1), Monotype' (Type.monotypeDescription -> Variable name2)) | name1 == name2 ->
          return True

        -- Unwrap variable from prefix. Note that we set an empty locals map when we use a variable
        -- from the prefix. This is to avoid shadowed bounds like in the
        -- relation `(x) ∀(x = x).x ≡ ⊥`
        (Monotype' (Type.monotypeDescription -> Variable name1), _) -> do
          maybeBinding1 <- prefix name1
          case maybeBinding1 of
            Nothing -> return False
            Just binding1 ->
              polytypeProjectionsEqual (Locals HashMap.empty) locals2 (Type.bindingType binding1) type2

        -- Unwrap variable from prefix. Note that we set an empty locals map when we use a variable
        -- from the prefix. This is to avoid shadowed bounds like in the
        -- relation `(x) ∀(x = x).x ≡ ⊥`
        (_, Monotype' (Type.monotypeDescription -> Variable name2)) -> do
          maybeBinding2 <- prefix name2
          case maybeBinding2 of
            Nothing -> return False
            Just binding2 ->
              polytypeProjectionsEqual locals1 (Locals HashMap.empty) type1 (Type.bindingType binding2)

        -- Bottom is only equivalent with itself.
        (Bottom _, Bottom _) -> return True
        (Bottom _, _) -> return False
        (_, Bottom _) -> return False

        -- Perform a monotype equivalence check when we have two monotypes.
        (Monotype' monotype1, Monotype' monotype2) ->
          monotypeProjectionsEqual locals1 locals2 monotype1 monotype2

        -- Unwrap quantifications and check monotype equivalence.
        (Quantify bindings1 body1, Monotype' body2) ->
          let newLocals1 = addBindingsToLocals locals1 bindings1 in
            monotypeProjectionsEqual newLocals1 locals2 body1 body2

        -- Unwrap quantifications and check monotype equivalence.
        (Monotype' body1, Quantify bindings2 body2) ->
          let newLocals2 = addBindingsToLocals locals2 bindings2 in
            monotypeProjectionsEqual locals1 newLocals2 body1 body2

        -- Unwrap quantifications and check monotype equivalence.
        (Quantify bindings1 body1, Quantify bindings2 body2) ->
          let
            newLocals1 = addBindingsToLocals locals1 bindings1
            newLocals2 = addBindingsToLocals locals2 bindings2
          in
            monotypeProjectionsEqual newLocals1 newLocals2 body1 body2

    -- Adds bounds from a quantification to a map of locals. Each bound includes the locals map at
    -- the point it was added. This is because if we need to check equivalence with a bound then the
    -- bound will have a different set of locals in scope then the point the bound was
    -- referenced at.
    addBindingsToLocals = foldl $ \locals binding ->
      Locals (HashMap.insert (Type.bindingName binding) (locals, Type.bindingType binding) (getLocals locals))
