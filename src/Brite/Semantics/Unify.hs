module Brite.Semantics.Unify () where

import Brite.Diagnostics
import Brite.Semantics.Type

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
unify = error "unimplemented"
