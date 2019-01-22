module Brite.Semantics.Unify
  ( unify
  ) where

import Brite.Diagnostics
import Brite.Semantics.Type (Monotype, MonotypeDescription(..))
import qualified Brite.Semantics.Type as Type

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
-- The caller of the unification algorithm which depends on equivalent types for soundness must
-- handle the error locally.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
unify :: Monotype -> Monotype -> Either Diagnostic ()
unify type1 type2 =
  case (Type.monotypeDescription type1, Type.monotypeDescription type2) of
    -- Variables with the same ID unify without any further analysis. We make sure to give every
    -- type variable a globally unique ID so that they’ll never conflict.
    (Variable id1, Variable id2) | id1 == id2 -> Right ()

    (Variable _, _) -> error "TODO: unimplemented"
    (_, Variable _) -> error "TODO: unimplemented"

    -- Primitive intrinsic types unify with each other no problem.
    (Boolean, Boolean) -> Right ()
    (Integer, Integer) -> Right ()

    -- Functions unify if the parameters and bodies unify.
    --
    -- If both the unification of the parameters and bodies fail then we will report two error
    -- diagnostics. However, we will only return the first error from unify.
    (Function parameter1 body1, Function parameter2 body2) ->
      let
        result1 = unify parameter1 parameter2
        result2 = unify body1 body2
      in
        result1 `eitherOr` result2

    -- Exhaustive match for failure case. Don’t use a wildcard (`_`) since if we add a new type we
    -- want a compiler warning telling us to add a case for that type to unification.
    (Boolean, _) -> incompatibleTypes
    (Integer, _) -> incompatibleTypes
    (Function _ _, _) -> incompatibleTypes

  where
    incompatibleTypes = error "unimplemented"

-- If the first `Either` is `Right` we return the second. If the first `Either` is `Left` we return
-- the first. So we return the first `Either` with an error.
eitherOr :: Either a b -> Either a b -> Either a b
eitherOr (Right _) x = x
eitherOr x@(Left _) _ = x
