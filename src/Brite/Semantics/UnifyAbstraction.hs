-- Implements the abstraction-check algorithm which is an auxillary algorithm of unification defined
-- by the [MLF thesis][1] in Section 4.2.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Brite.Semantics.UnifyAbstraction
  ( abstractionCheck
  ) where

import Brite.Semantics.CheckMonad
import Brite.Semantics.Type (Polytype, PolytypeDescription(..), Monotype, MonotypeDescription(..))
import qualified Brite.Semantics.Type as Type
import Brite.Syntax.Token (Identifier)
import Data.Foldable (foldl')
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
projectionsEqual prefix = polytypeProjectionsEqual (Locals HashMap.empty) (Locals HashMap.empty)
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
        (Void, Void) -> return True
        (Boolean, Boolean) -> return True
        (Integer, Integer) -> return True

        -- Exhaustive failure cases.
        (Void, _) -> return False
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

-- See Section 2.7.1 of the [MLF thesis][1].
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
data PolynomialVariable = X | Y | Z
  deriving (Eq)

-- Represents a polynomial. Each entry represents a term. The key of an entry represents the _power_
-- of each polynomial variable (`X`, `Y`, and `Z`) and the value represents the integer coefficient.
--
-- For example, an entry with a key of `(1, 2, 0)` and a value of `3` represents the term `3XY²`.
-- All entries are added together to get our polynomial.
type Polynomial = Map (Int, Int, Int) Int

-- This is an interesting little function. In this function we detect if the _quantity_ of flexible
-- binders are different in the two types we are provided. It can be tricky to understand _why_ this
-- is important. Consider the program:
--
-- ```
-- // add1: number → number
-- let auto = λ(x: ∀a.a → a).x x in // auto: ∀(x = ∀a.a → a).x → x
-- auto add1
-- ```
--
-- We better reject this program since we can’t call `add1 add1`! That program would be ill-formed.
-- This is what rigid bindings represent. The refusal to instantiate a given type bound to a new
-- type. We can’t update the bound `b = ∀a.a → a` to `b = number → number` since that would be an
-- instantiation of `a`.
--
-- This program is rejected when the `projectionsEqual` function above returns false. Here’s a
-- program which needs `flexibleBindersUnchanged` to be properly rejected:
--
-- ```
-- // add1: number → number
-- let auto = λ(x: ∀a.a → a).x x in // auto: ∀(x = ∀a.a → a).x → x
-- (auto: ∀(x ≥ ∀a.a → a).x → x) add1
-- ```
--
-- In this program we try to “loosen” the type of `auto` to `∀(x ≥ ∀a.a → a).x → x` so that we can
-- apply `add1`. Similarly, we must reject this program or else we execute `add1 add1` which is
-- unsound! To reject this program `flexibleBindersUnchanged` observes that we have a new flexible
-- binder in the annotation.
--
-- So why does this implementation work? Formally the [MLF thesis][1] describes this operation in
-- Lemma 2.7.8 as:
--
-- > X ∉ w(o1) − w(o2)
--
-- What is `X`? What is `w()`? Why are we subtracting? How is `X` an element of a number? This makes
-- sense if you read section 2.7.1, section 2.7.2, and lemma 2.7.8.
--
-- What’s happening is that `w(t)` calculates a “weight” number based on the position of bindings in
-- `t`. Except the “weight” isn’t really a number. It’s a polynomial. A polynomial with three
-- abstract variables `X`, `Y`, and `Z`. So a weight looks like `X + X² + XY + Y² + YZ`. Except you
-- can think of each polynomial as a path. So `X * X` is actually the path to a bottom (`⊥`)
-- quantification of `≥≥`.
--
-- Ok, look, I tried explaining it, but you should really just read section 2.7. It has nice and
-- helpful pictures for understanding the theory of this function. Once you’re done with that come
-- back here.
--
-- ...
--
-- Oh good, you’re back! In the implementation of `flexibleBindersUnchanged` we represent a
-- polynomial as a map which takes a triple as the key. The triple consists of the power of `X`,
-- `Y`, and `Z` respectively. The value represents the integer coefficient of the polynomial entry.
-- For example the polynomial `X + X² + XY + 2Z²` is represented by the map:
--
-- ```
-- (X, Y, Z) -> n
-- --------------
-- (1, 0, 0) -> 1
-- (2, 0, 0) -> 1
-- (1, 1, 0) -> 1
-- (0, 0, 2) -> 2
-- ```
--
-- Except we don’t care about entries where the power of `X` is 0 for the final check, so the map we
-- build does not contain those terms. Instead of trying to subtract polynomials and then checking
-- `X ∉ w(o1) − w(o2)` we check to see that the two maps are equal for all terms that contain an
-- `X`. If they are equal that is the same as performing a subtraction and checking for bindings
-- that contain an `X`.
--
-- NOTE: Both types should be in normal form!
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
flexibleBindersUnchanged :: Polytype -> Polytype -> Bool
flexibleBindersUnchanged type1 type2 =
  -- Create the polynomials for both of our types and check to make sure they are equal. Remember
  -- that these polynomials exclude terms which do not have an `X` variable.
  let
    polynomial1 = loop X Map.empty 0 0 0 type1
    polynomial2 = loop X Map.empty 0 0 0 type2
  in
    polynomial1 == polynomial2
  where
    -- Create a polynomial for the provided type.
    loop :: PolynomialVariable -> Polynomial -> Int -> Int -> Int -> Polytype -> Polynomial
    loop state acc0 xs ys zs t0 =
      -- If we will never have the opportunity to add another `X` to our polynomial and we have no
      -- `xs` then we can short-circuit since we know, logically, no more entries will be added.
      if state /= X && xs == 0 then acc0 else
        case Type.polytypeDescription t0 of
          -- Monotypes do not contain binders.
          Monotype' _ -> acc0

          -- If we find a bottom type and we have some `xs` then add this path to our polynomial.
          -- The short-circuit above depends on our `xs = 0` check here for correctness.
          Bottom _ -> if xs == 0 then acc0 else Map.insertWith (+) (xs, ys, zs) 1 acc0

          -- For quantifications add one to the correct polynomial variable and recurse into the
          -- polynomial’s bound. Also make sure to proceed to the next state correctly.
          Quantify bindings _ ->
            foldl'
              (\acc1 binding ->
                let t1 = Type.bindingType binding in
                  case (state, Type.bindingFlexibility binding) of
                    (X, Type.Flexible) -> loop X acc1 (xs + 1) ys zs t1
                    (X, Type.Rigid)    -> loop Y acc1 xs (ys + 1) zs t1
                    (Y, Type.Flexible) -> loop Z acc1 xs ys (zs + 1) t1
                    (Y, Type.Rigid)    -> loop Y acc1 xs (ys + 1) zs t1
                    (Z, _)             -> loop Z acc1 xs ys (zs + 1) t1)
              acc0
              bindings

-- IMPORTANT: We assume that `(Q) t1 ⊑ t2` holds. Do not call this function with two types which do
-- not uphold this relation!
--
-- The abstraction check algorithm defined in Figure 4.1 of the [MLF Thesis][1]. Also see Lemma
-- 2.7.8 as it provides more context as to why this algorithm is correct.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
abstractionCheck :: (Identifier -> Check s (Maybe Type.Binding)) -> Polytype -> Polytype -> Check s Bool
abstractionCheck prefix initialType1 initialType2 = do
  -- If the projections of the two types are not equal then the check fails by Property 2.1.3 (i).
  equal <- projectionsEqual prefix initialType1 initialType2
  if not equal then return False else loop initialType1 initialType2
  where
    loop type1 type2 =
      case (Type.polytypeDescription type1, Type.polytypeDescription type2) of
        -- If the left type is a monotype then we may shortcut the following steps and return true.
        (Monotype' _, _) -> return True

        -- If the right type is a variable our algorithm won’t work so try to find the bound of this
        -- variable and recurse with that bound’s type. If the bound is not rigid then our
        -- check fails.
        --
        -- In the presentation of the abstraction-check algorithm in figure 4.1 every time we
        -- recurse we also check whether or not the projections are equal again. However, this check
        -- is computationally wasteful. Projections will always be equal so we don’t perform that
        -- check again.
        (_, Monotype' (Type.monotypeDescription -> Variable name2)) -> do
          maybeBinding <- prefix name2
          case maybeBinding of
            Nothing -> return False
            Just binding | Type.bindingFlexibility binding /= Type.Rigid -> return False
            Just binding -> loop type1 (Type.bindingType binding)

        -- Finally, we expect the flexible binders to not have changed between the two types.
        _ -> return (flexibleBindersUnchanged type1 type2)
