{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.UnifySpec (spec) where

import Brite.Diagnostic
import qualified Brite.Semantics.AST as AST
import Brite.Semantics.Check (checkPolytype)
import Brite.Semantics.CheckMonad
import qualified Brite.Semantics.Prefix as Prefix
import qualified Brite.Semantics.Type as Type
import Brite.Semantics.TypePrinter
import Brite.Semantics.Unify
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Glyph
import Brite.Syntax.Identifier
import Brite.Syntax.Parser
import Brite.Syntax.ParserFramework
import Brite.Syntax.Printer
import Brite.Syntax.Range
import Brite.Syntax.TokenStream
import Data.Foldable (traverse_, toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder.Custom as Text.Builder
import System.IO
import Test.Hspec

-- In the [MLF thesis][1] Section 4.3 the unification algorithm is described as:
--
-- > The algorithm `unify` takes a prefix `Q` and two types `t1` and `t2` and returns a prefix that
-- > unifies `t1` and `t2` under `Q`.
--
-- So we write our tests like they are directly calling that unification algorithm so we can reason
-- about our tests in theory.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
testData :: [Text]
testData =
  [ "unify(<>, Bool, Bool)"
  , "unify(<>, Int, Int)"
  , "unify(<A>, Int, Int)"
  , "unify(<A, B>, Int, Int)"
  , "unify(<A, B, C>, Int, Int)"
  , "unify(<>, fun(Int) -> Int, fun(Int) -> Int)"
  , "unify(<>, fun(Int) -> Bool, fun(Int) -> Bool)"
  , "unify(<>, fun(Int) -> fun(Bool) -> Bool, fun(Int) -> fun(Bool) -> Bool)"
  , "unify(<>, fun(Int) -> Int, fun(Int) -> Bool)"
  , "unify(<>, fun(Int) -> Int, fun(Bool) -> Int)"
  , "unify(<>, fun(Int) -> Int, fun(Bool) -> Bool)"
  , "unify(<>, fun(Int) -> fun(Int) -> Int, fun(Bool) -> fun(Bool) -> Bool)"
  , "unify(<>, fun(fun(Int) -> Int) -> Int, fun(fun(Bool) -> Bool) -> Bool)"
  , "unify(<>, Int, fun(Int) -> Int)"
  , "unify(<>, fun(Int) -> Int, Int)"
  , "unify(<A = Int>, A, Int)"
  , "unify(<A = Int>, Int, A)"
  , "unify(<A = Int>, A, Bool)"
  , "unify(<A = Int>, Bool, A)"
  , "unify(<A: Int>, A, Int)"
  , "unify(<A: Int>, Int, A)"
  , "unify(<A: Int>, A, Bool)"
  , "unify(<A: Int>, Bool, A)"
  , "unify(<B = Int, A = B>, A, Int)"
  , "unify(<B = Int, A = B>, Int, A)"
  , "unify(<B = Int, A = B>, A, Bool)"
  , "unify(<B = Int, A = B>, Bool, A)"
  , "unify(<B = Int, A: B>, A, Int)"
  , "unify(<B = Int, A: B>, Int, A)"
  , "unify(<B = Int, A: B>, A, Bool)"
  , "unify(<B = Int, A: B>, Bool, A)"
  , "unify(<B: Int, A = B>, A, Int)"
  , "unify(<B: Int, A = B>, Int, A)"
  , "unify(<B: Int, A = B>, A, Bool)"
  , "unify(<B: Int, A = B>, Bool, A)"
  , "unify(<B: Int, A: B>, A, Int)"
  , "unify(<B: Int, A: B>, Int, A)"
  , "unify(<B: Int, A: B>, A, Bool)"
  , "unify(<B: Int, A: B>, Bool, A)"
  , "unify(<A>, A, A)"
  , "unify(<A: !>, A, A)"
  , "unify(<A = !>, A, A)"
  , "unify(<A, B = A, C = B>, B, C)"
  , "unify(<A, B = A, C = B>, C, B)"
  , "unify(<A, B = A, C = B>, A, B)"
  , "unify(<A, B = A, C = B>, A, C)"
  , "unify(<A, B = A, C = B>, B, A)"
  , "unify(<A, B = A, C = B>, C, A)"
  , "unify(<A, B = A, C = B>, B, A)"
  , "unify(<A, B = A, C = B>, B, B)"
  , "unify(<A, B = A, C = B>, C, C)"
  , "unify(<A, B = <Z> A, C = <Z> B>, B, C)"
  , "unify(<A, B = <Z> A, C = <Z> B>, C, B)"
  , "unify(<A, B = <Z> A, C = <Z> B>, A, B)"
  , "unify(<A, B = <Z> A, C = <Z> B>, A, C)"
  , "unify(<A, B = <Z> A, C = <Z> B>, B, A)"
  , "unify(<A, B = <Z> A, C = <Z> B>, C, A)"
  , "unify(<A, B = <Z> A, C = <Z> B>, B, A)"
  , "unify(<A, B = <Z> A, C = <Z> B>, B, B)"
  , "unify(<A, B = <Z> A, C = <Z> B>, C, C)"
  , "unify(<A>, A, Int)"
  , "unify(<A>, Int, A)"
  , "unify(<A: !>, A, Int)"
  , "unify(<A: !>, Int, A)"
  , "unify(<A = !>, A, Int)"
  , "unify(<A = !>, Int, A)"
  , "unify(<B: !, A: B>, A, Int)"
  , "unify(<B: !, A: B>, Int, A)"
  , "unify(<B: !, A = B>, A, Int)"
  , "unify(<B: !, A = B>, Int, A)"
  , "unify(<B = !, A: B>, A, Int)"
  , "unify(<B = !, A: B>, Int, A)"
  , "unify(<B = !, A = B>, A, Int)"
  , "unify(<B = !, A = B>, Int, A)"
  , "unify(<B, A = fun(B) -> B>, A, B)"
  , "unify(<B, A = fun(B) -> B>, B, A)"
  , "unify(<B, C = fun(B) -> B, A = C>, A, B)"
  , "unify(<B, C = fun(B) -> B, A = C>, B, A)"
  , "unify(<B, C = fun(B) -> B, A = fun(C) -> C>, A, B)"
  , "unify(<B, C = fun(B) -> B, A = fun(C) -> C>, B, A)"
  , "unify(<A, B>, A, B)"
  , "unify(<A, B>, B, A)"
  , "unify(<A, B>, A, fun(B) -> B)"
  , "unify(<A, B>, fun(B) -> B, A)"
  , "unify(<A: fun<X>(X) -> X>, A, fun(Int) -> Int)"
  , "unify(<A: fun<X>(X) -> X>, A, fun(Int) -> Bool)"
  , "unify(<A = fun<X>(X) -> X>, A, fun(Int) -> Int)"
  , "unify(<A = fun<X>(X) -> X>, A, fun(Int) -> Bool)"
  , "unify(<A: fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B = fun(Int) -> Int>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B = fun(Int) -> Int>, B, A)"
  , "unify(<A, B = fun(A) -> A>, A, B)"
  , "unify(<A, B = fun(A) -> A>, B, A)"
  , "unify(<A, B = fun<C>(C) -> A>, A, B)"
  , "unify(<A, B = fun<C>(C) -> A>, B, A)"
  , "unify(<A = !, B = !>, A, B)"
  , "unify(<A = !, B = !>, B, A)"
  , "unify(<A = !, B = fun<C>(C) -> C>, A, B)"
  , "unify(<A = !, B = fun<C>(C) -> C>, B, A)"
  , "unify(<A, B>, A, B)"
  , "unify(<A, B>, B, A)"
  , "unify(<A: !, B: !>, A, B)"
  , "unify(<A: !, B: !>, B, A)"
  , "unify(<A = !, B: !>, A, B)"
  , "unify(<A = !, B: !>, B, A)"
  , "unify(<A: !, B = !>, A, B)"
  , "unify(<A: !, B = !>, B, A)"
  , "unify(<A = !, B = !>, A, B)"
  , "unify(<A = !, B = !>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B: fun<X>(X) -> X>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B: fun<X>(X) -> X>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B: fun<X>(X) -> X>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B: fun<X>(X) -> X>, B, A)"
  , "unify(<A: fun<X>(X) -> X, B = fun<X>(X) -> X>, A, B)"
  , "unify(<A: fun<X>(X) -> X, B = fun<X>(X) -> X>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B = fun<X>(X) -> X>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B = fun<X>(X) -> X>, B, A)"
  , "unify(<X>, X, X)"
  , "unify(<A: Int>, A, Int)"
  , "unify(<A = Int>, A, Int)"
  , "unify(<A: Int>, A, Bool)"
  , "unify(<A = Int>, A, Bool)"
  , "unify(<A: Int>, Int, A)"
  , "unify(<A = Int>, Int, A)"
  , "unify(<A: Int>, Bool, A)"
  , "unify(<A = Int>, Bool, A)"
  , "unify(<A, B = A>, A, B)"
  , "unify(<A, B = A>, B, A)"
  , "unify(<A, B: A>, A, B)"
  , "unify(<A, B: A>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, B, A)"
  , "unify(<C, A: fun<X>(X) -> C, B: fun<Y>(Y) -> A>, A, B)"
  , "unify(<C, A: fun<X>(X) -> C, B: fun<Y>(Y) -> A>, B, A)"
  , "unify(<C, A = fun<X>(X) -> C, B: fun<Y>(Y) -> A>, A, B)"
  , "unify(<C, A = fun<X>(X) -> C, B: fun<Y>(Y) -> A>, B, A)"
  , "unify(<C, A: fun<X>(X) -> C, B = fun<Y>(Y) -> A>, A, B)"
  , "unify(<C, A: fun<X>(X) -> C, B = fun<Y>(Y) -> A>, B, A)"
  , "unify(<C, A = fun<X>(X) -> C, B = fun<Y>(Y) -> A>, A, B)"
  , "unify(<C, A = fun<X>(X) -> C, B = fun<Y>(Y) -> A>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B>, A, fun(B) -> Bool)"
  , "unify(<A: fun<X>(X) -> Int, B>, A, fun(B) -> Int)"
  , "unify(<A = fun<X>(X) -> Int, B>, A, fun(B) -> Bool)"
  , "unify(<A = fun<X>(X) -> Int, B>, A, fun(B) -> Int)"
  , "unify(<A: fun<X>(X) -> Int>, A, fun(Bool) -> Bool)"
  , "unify(<A: fun<X>(X) -> Int>, A, fun(Bool) -> Int)"
  , "unify(<A = fun<X>(X) -> Int>, A, fun(Bool) -> Bool)"
  , "unify(<A = fun<X>(X) -> Int>, A, fun(Bool) -> Int)"
  , "unify(<A: fun<X>(X) -> Int>, A, fun(A) -> Bool)"
  , "unify(<A: fun<X>(X) -> Int>, A, fun(A) -> Int)"
  , "unify(<A = fun<X>(X) -> Int>, A, fun(A) -> Bool)"
  , "unify(<A = fun<X>(X) -> Int>, A, fun(A) -> Int)"
  , "unify(<A: fun<X>(X) -> Int, B>, fun(B) -> Bool, A)"
  , "unify(<A: fun<X>(X) -> Int, B>, fun(B) -> Int, A)"
  , "unify(<A = fun<X>(X) -> Int, B>, fun(B) -> Bool, A)"
  , "unify(<A = fun<X>(X) -> Int, B>, fun(B) -> Int, A)"
  , "unify(<A: fun<X>(X) -> Int>, fun(Bool) -> Bool, A)"
  , "unify(<A: fun<X>(X) -> Int>, fun(Bool) -> Int, A)"
  , "unify(<A = fun<X>(X) -> Int>, fun(Bool) -> Bool, A)"
  , "unify(<A = fun<X>(X) -> Int>, fun(Bool) -> Int, A)"
  , "unify(<A: fun<X>(X) -> Int>, fun(A) -> Bool, A)"
  , "unify(<A: fun<X>(X) -> Int>, fun(A) -> Int, A)"
  , "unify(<A = fun<X>(X) -> Int>, fun(A) -> Bool, A)"
  , "unify(<A = fun<X>(X) -> Int>, fun(A) -> Int, A)"
  , "unify(<A, B = fun<X>(X) -> X>, A, B)"
  , "unify(<A, B = fun<X>(X) -> X>, B, A)"
  , "unify(<A, B: fun<X>(X) -> X>, A, B)"
  , "unify(<A, B: fun<X>(X) -> X>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, B, A)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, B, A)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, B, A)"
  , "unify(<A = fun<X>(X) -> X, B = fun<X>(X) -> X>, A, B)"
  , "unify(<A = fun<X>(X) -> X, B = fun<X>(X) -> X>, B, A)"
  , "unify(<A, B = A, C = B, D = C, E = D, F = fun(E) -> E>, A, F)"
  , "unify(<A, B = A, C = B, D = C, E = D, F = fun(E) -> E>, F, A)"
  , "unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, fun(A) -> A, fun(A) -> B)"
  , "unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, fun(A) -> B, fun(A) -> A)"
  , "unify(<T = <A = fun<X>(X) -> X> fun(A) -> A, U = <B = fun<Y>(Y) -> Y, C = fun<Z>(Z) -> Z> fun(B) -> C>, T, U)"
  , "unify(<T = <A = fun<X>(X) -> X> fun(A) -> A, U = <B = fun<Y>(Y) -> Y, C = fun<Z>(Z) -> Z> fun(B) -> C>, U, T)"
  , "unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B>, A, B)"
  , "unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B>, B, A)"
  , "unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B, C, D, E>, A, fun(fun(B) -> C) -> fun(D) -> E)"
  , "unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B, C, D, E>, fun(fun(B) -> C) -> fun(D) -> E, A)"
  , "unify(<A: <X, X: fun<Z>(Z) -> X> fun(X) -> X, B, C, D, E>, A, fun(fun(B) -> C) -> fun(D) -> E)"
  , "unify(<A: <X, X: fun<Z>(Z) -> X> fun(X) -> X, B, C, D, E>, fun(fun(B) -> C) -> fun(D) -> E, A)"
  , "unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B = <X, X = fun<Z>(Z) -> X> fun(X) -> X>, A, B)"
  , "unify(<A = <X, X2, X = fun<Z>(Z) -> fun(X2) -> X> fun(X) -> X, B = <X, X2, X = fun<Z>(Z) -> fun(X2) -> X> fun(X) -> X>, A, B)"
  , "unify(<A = <X1, X1 = fun<Z>(Z) -> X1> fun(X1) -> X1, B = <X1, X1 = fun<Z>(Z) -> X1> fun(X1) -> X1>, A, B)"
  , "unify(<A = <X1, X2, X1 = fun<Z>(Z) -> fun(X2) -> X1> fun(X1) -> X1, B = <X1, X2, X1 = fun<Z>(Z) -> fun(X2) -> X1> fun(X1) -> X1>, A, B)"
  , "unify(<A = <X5, X5 = fun<Z>(Z) -> X5> fun(X5) -> X5, B = <X5, X5 = fun<Z>(Z) -> X5> fun(X5) -> X5>, A, B)"
  , "unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B = <X, X = fun<Z>(Z) -> X> fun(X) -> X>, B, A)"
  , "unify(<A = <X, X2, X = fun<Z>(Z) -> fun(X2) -> X> fun(X) -> X, B = <X, X2, X = fun<Z>(Z) -> fun(X2) -> X> fun(X) -> X>, B, A)"
  , "unify(<A = <X1, X1 = fun<Z>(Z) -> X1> fun(X1) -> X1, B = <X1, X1 = fun<Z>(Z) -> X1> fun(X1) -> X1>, B, A)"
  , "unify(<A = <X1, X2, X1 = fun<Z>(Z) -> fun(X2) -> X1> fun(X1) -> X1, B = <X1, X2, X1 = fun<Z>(Z) -> fun(X2) -> X1> fun(X1) -> X1>, B, A)"
  , "unify(<A = <X5, X5 = fun<Z>(Z) -> X5> fun(X5) -> X5, B = <X5, X5 = fun<Z>(Z) -> X5> fun(X5) -> X5>, B, A)"
  , "unify(<A: fun<X>(X) -> X>, A, fun(Int) -> Int)"
  , "unify(<A: fun<X>(X) -> X, B>, A, fun(Int) -> B)"
  , "unify(<A = <X = Int> X>, A, Int)"
  , "unify(<A: <X: fun<Y>(Y) -> Y> X>, A, fun(Int) -> Int)"
  , "unify(<A = <X: fun<Y>(Y) -> Y> X>, A, fun(Int) -> Int)"
  , "unify(<A: <X = fun<Y>(Y) -> Y> X>, A, fun(Int) -> Int)"
  , "unify(<A = <X = fun<Y>(Y) -> Y> X>, A, fun(Int) -> Int)"
  , "unify(<A: fun<Y>(Y) -> Y>, A, fun(Int) -> Int)"
  , "unify(<A: <B = fun<X>(X) -> X> B>, A, fun(Int) -> Int)"
  , "unify(<A: <B = <C = fun<X>(X) -> X> C> B>, A, fun(Int) -> Int)"
  , "unify(<A: <B = <C = <D = fun<X>(X) -> X> D> C> B>, A, fun(Int) -> Int)"
  , "unify(<A: <B = fun<X>(X) -> X> fun(B) -> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)"
  , "unify(<A: <B = <C = fun<X>(X) -> X> C> fun(B) -> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)"
  , "unify(<A: <B = <C = <D = fun<X>(X) -> X> D> C> fun(B) -> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)"
  , "unify(<A: <B = fun<X>(X) -> X> fun(B) -> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)"
  , "unify(<A: <B = <C = fun<X>(X) -> X> fun(C) -> C> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)"
  , "unify(<A: <B = <C = <D = fun<X>(X) -> X> fun(D) -> D> C> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)"
  , "unify(<A: <X> X>, A, Int)"
  , "unify(<A: <X> X>, Int, A)"
  , "unify(<A: <X> X, B: <X> X>, A, B)"
  , "unify(<A: <X> X, B: <X> X>, B, A)"
  , "unify(<A = <X, Y> fun(Int) -> Int>, A, fun(Int) -> Int)"
  , "unify(<A = <X, Y> fun(Y) -> Int, B = <X, Y> fun(Y) -> Int>, A, B)"
  , "unify(<A = <X, Y> fun(Y) -> Int, B = fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A = <X, Y> fun(Y) -> Int, B = fun<Y>(Y) -> Int>, B, A)"
  , "unify(<A = <Y, X> fun(Y) -> Int, B = <Y, X> fun(Y) -> Int>, A, B)"
  , "unify(<A = <Y, X> fun(Y) -> Int, B = fun<Y>(Y) -> Int>, A, B)"
  , "unify(<A = <Y, X> fun(Y) -> Int, B = fun<Y>(Y) -> Int>, B, A)"
  , "unify(<X = <A: fun<B>(B) -> B> fun(A) -> A, Y = fun<C>(fun(C) -> C) -> (fun(C) -> C)>, X, Y)"
  , "unify(<X: <A: fun<B>(B) -> B> fun(A) -> A, Y = fun<C>(fun(C) -> C) -> (fun(C) -> C)>, X, Y)"
  , "unify(<X = <A: fun<B>(B) -> B> fun(A) -> A, Y: fun<C>(fun(C) -> C) -> (fun(C) -> C)>, X, Y)"
  , "unify(<X: <A: fun<B>(B) -> B> fun(A) -> A, Y: fun<C>(fun(C) -> C) -> (fun(C) -> C)>, X, Y)"
  , "unify(<X = <A: fun<B>(B) -> B> fun(A) -> A, Y = fun<C>(fun(C) -> C) -> (fun(C) -> C)>, Y, X)"
  , "unify(<X: <A: fun<B>(B) -> B> fun(A) -> A, Y = fun<C>(fun(C) -> C) -> (fun(C) -> C)>, Y, X)"
  , "unify(<X = <A: fun<B>(B) -> B> fun(A) -> A, Y: fun<C>(fun(C) -> C) -> (fun(C) -> C)>, Y, X)"
  , "unify(<X: <A: fun<B>(B) -> B> fun(A) -> A, Y: fun<C>(fun(C) -> C) -> (fun(C) -> C)>, Y, X)"
  , "unify(<T = nope>, T, T)"
  , "unify(<T1 = nope, T2 = nope>, T1, T2)"
  , "unify(<T1 = nope1, T2 = nope2>, T1, T2)"
  , "unify(<X, Y = Int>, X, Y)"
  , "unify(<X, Y = Int>, Y, X)"
  , "unify(<X, Y = nope>, X, Y)"
  , "unify(<X, Y = nope>, Y, X)"
  , "unify(<A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope>, A, B)"
  , "unify(<A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope>, B, A)"
  , "unify(<C = nope, A = fun<Z>(Z) -> C, B = fun<Z>(Z) -> C>, A, B)"
  , "unify(<C = nope, A = fun<Z>(Z) -> C, B = fun<Z>(Z) -> C>, B, A)"
  , "unify(<X, Y, T = <E = Int, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, T, fun(X) -> Y)"
  , "unify(<X, Y, T = <E = Int, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, fun(X) -> Y, T)"
  , "unify(<X, Y, T = <E = nope, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, T, fun(X) -> Y)"
  , "unify(<X, Y, T = <E = nope, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, fun(X) -> Y, T)"
  , "unify(<X, Y, T = <E1 = nope, E2 = nope, A = fun<Z>(Z) -> E1, B = fun<Z>(Z) -> E2> fun(A) -> B>, T, fun(X) -> Y)"
  , "unify(<X, Y, T = <E1 = nope, E2 = nope, A = fun<Z>(Z) -> E1, B = fun<Z>(Z) -> E2> fun(A) -> B>, fun(X) -> Y, T)"
  , "unify(<X, Y, T = <A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope> fun(A) -> B>, T, fun(X) -> Y)"
  , "unify(<X, Y, T = <A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope> fun(A) -> B>, fun(X) -> Y, T)"
  , "unify(<X, Y, T = <E = nope, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, fun(T) -> (fun(Y) -> X), fun(fun(X) -> Y) -> (fun(X) -> Y))"
  , "unify(<X, Y, T = <E1 = nope, E2 = nope, A = fun<Z>(Z) -> E1, B = fun<Z>(Z) -> E2> fun(A) -> B>, fun(T) -> (fun(Y) -> X), fun(fun(X) -> Y) -> (fun(X) -> Y))"
  , "unify(<X, Y, T = <A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope> fun(A) -> B>, fun(T) -> (fun(Y) -> X), fun(fun(X) -> Y) -> (fun(X) -> Y))"
  , "unify(<A, B>, A, B)"
  , "unify(<>, void, void)"
  , "unify(<>, Bool, void)"
  , "unify(<>, void, Bool)"
  , "unify(<>, { a: Int, b: Bool, c: void }, { a: Int, b: Int, c: void })"
  , "unify(<>, { a: Int, b: Bool, c: void }, { a: Bool, b: Bool, c: void })"
  , "unify(<>, { a: Int, b: Bool, c: void }, { a: Int, b: Bool, c: Bool })"
  , "unify(<>, { a: Int, b: Bool, c: void }, { a: Int, b: Bool, c: void })"
  , "unify(<>, { a: Int, b: Bool, c: void }, { a: Bool, b: Int, c: void })"
  , "unify(<>, { a: Int, b: Bool, c: void }, { a: Int, b: void, c: Bool })"
  , "unify(<>, { a: Int, b: Bool, c: void }, { a: void, b: Bool, c: Int })"
  , "unify(<>, { a: Int, b: Bool }, { a: Bool, b: Int })"
  , "unify(<>, { b: Bool, a: Int }, { b: Int, a: Bool })"
  , "unify(<>, { a: Int }, { b: Int })"
  , "unify(<>, { a: Int, a: Bool }, { b: Int, b: Bool })"
  , "unify(<>, { a: Int }, {})"
  , "unify(<>, {}, { a: Int })"
  , "unify(<>, { a: Int }, { b: Int })"
  , "unify(<>, { a: Int }, { b: Int | Int })"
  , "unify(<>, { a: Int }, { b: Int | {} })"
  , "unify(<>, { a: Int | {} }, { b: Int })"
  , "unify(<>, { a: Int }, { b: Int | { a: Int } })"
  , "unify(<>, { b: Int | { a: Int } }, { a: Int })"
  , "unify(<>, {| {}}, {})"
  , "unify(<>, {}, {| {}})"
  , "unify(<>, {| {}}, {| {}})"
  , "unify(<>, {| {a: Int}}, {| {a: Int}})"
  , "unify(<>, {| {a: Int}}, {| {a: Bool}})"
  , "unify(<>, {| {a: Int}}, {| {b: Int}})"
  , "unify(<>, {| {a: Int}}, {| {}})"
  , "unify(<>, {| {}}, {| {a: Int}})"
  , "unify(<>, {| {a: Int}}, {a: Int})"
  , "unify(<>, {a: Int}, {| {a: Int}})"
  , "unify(<>, {| {a: Int}}, {a: Bool})"
  , "unify(<>, {a: Bool}, {| {a: Int}})"
  , "unify(<>, {| {a: Int}}, {b: Bool})"
  , "unify(<>, {b: Bool}, {| {a: Int}})"
  , "unify(<>, {}, { a: Int })"
  , "unify(<>, { b: Int }, { a: Int })"
  , "unify(<>, { b: Int | {} }, { a: Int })"
  , "unify(<>, { a: Int, a: Bool, a: void }, { a: Int, a: Int, a: void })"
  , "unify(<>, { a: Int, a: Bool, a: void }, { a: Bool, a: Bool, a: void })"
  , "unify(<>, { a: Int, a: Bool, a: void }, { a: Int, a: Bool, a: Bool })"
  , "unify(<>, { a: Int, a: Bool, a: void }, { a: Int, a: Bool, a: void })"
  , "unify(<>, { a: Int, a: Bool, a: void }, { a: Bool, a: Int, a: void })"
  , "unify(<>, { a: Int, a: Bool, a: void }, { a: Int, a: void, a: Bool })"
  , "unify(<>, { a: Int, a: Bool, a: void }, { a: void, a: Bool, a: Int })"
  , "unify(<T>, { a: Int | T }, { a: Int, b: Bool })"
  , "unify(<T>, { a: Int, b: Bool }, { a: Int | T })"
  , "unify(<T>, { a: Int | T }, { a: Bool, b: Int })"
  , "unify(<T>, { a: Bool, b: Int }, { a: Int | T })"
  , "unify(<T>, { a: Int | T }, { a: Int, b: Bool, b: Int })"
  , "unify(<T>, { a: Int, b: Bool, b: Int }, { a: Int | T })"
  , "unify(<T>, { a: Int | T }, { a: Int, a: Bool })"
  , "unify(<T>, { a: Int, a: Bool }, { a: Int | T })"
  , "unify(<T>, { a: Int | T }, { a: Bool, a: Int })"
  , "unify(<T>, { a: Bool, a: Int }, { a: Int | T })"
  , "unify(<T>, { a: Int | T }, { a: Int, a: Bool, a: void })"
  , "unify(<T>, { a: Int, a: Bool, a: void }, { a: Int | T })"
  , "unify(<T>, fun({a: Int}) -> {a: Int}, fun({a: Int | T}) -> T)"
  , "unify(<T>, fun({a: Int, a: Int}) -> {a: Int}, fun({a: Int | T}) -> T)"
  , "unify(<T>, fun({a: Int}) -> {a: Int}, fun(T) -> {a: Int | T})"
  , "unify(<T>, fun({a: Int}) -> Int, fun({a: Int | T}) -> T)"
  , "unify(<T>, fun({a: Int}) -> Int, fun({a: Int, a: Int | T}) -> T)"
  , "unify(<T>, fun({a: Int}) -> Int, fun(T) -> {a: Int | T})"
  , "unify(<T>, fun({a: Int | T}) -> T, fun({a: Int}) -> {a: Int})"
  , "unify(<T>, fun(T) -> {a: Int | T}, fun({a: Int}) -> {a: Int})"
  , "unify(<T>, fun({a: Int | T}) -> T, fun({a: Int}) -> Int)"
  , "unify(<T>, fun(T) -> {a: Int | T}, fun({a: Int}) -> Int)"
  , "unify(<T>, {a: Int | T}, T)"
  , "unify(<T>, T, {a: Int | T})"
  , "unify(<T>, {b: Int}, {a: Int | T})"
  , "unify(<T>, {a: Int | T}, {b: Int})"
  , "unify(<T>, {a: Int, b: Int}, {a: Int | T})"
  , "unify(<T>, {a: Int | T}, {a: Int, b: Int})"
  ]

openSnapshotFile :: IO Handle
openSnapshotFile = do
  h <- openFile "test/Brite/Semantics/UnifySpecSnapshot.md" WriteMode
  hPutStrLn h "# UnifySpecSnapshot"
  return h

closeSnapshotFile :: Handle -> IO ()
closeSnapshotFile h = do
  hPutStrLn h ""
  hPutStrLn h (replicate 80 '-')
  hClose h

unifyParser :: Parser (Recover CST.QuantifierList, Recover CST.Type, Recover CST.Type)
unifyParser = identifier *> glyph ParenLeft *> args <* glyph ParenRight
  where
    args =
      (,,)
        <$> (retry tryQuantifierListParser <* glyph Comma)
        <*> (typeParser <* glyph Comma)
        <*> typeParser

spec :: Spec
spec = beforeAll openSnapshotFile $ afterAll closeSnapshotFile $
  flip traverse_ testData $ \input ->
    it (Text.unpack input) $ \h -> do
      let ((cqs, ct1, ct2), ds1) = runDiagnosticWriter (fst <$> (runParser unifyParser (tokenize input)))
      if null ds1 then return () else error (Text.Builder.toString (foldMap diagnosticMessageMarkdown ds1))
      let
        -- Use the quantifier list to quantify a boolean type. Could be anything really. We just
        -- need to send it through our conversion and type checking pipeline.
        (t3, ds2) = runDiagnosticWriter . checkPolytype mempty . AST.convertRecoverType . Ok $ case cqs of
          Recover _ _ _ -> undefined
          Fatal _ _ -> undefined
          Ok cqs' -> CST.QuantifiedType cqs' (Ok (CST.VariableType (CST.Name (unsafeIdentifier "Bool") undefined)))

        -- Run some code in the check monad...
        (allBindings, ds3) = runCheck $ do
          prefix <- Prefix.new
          Prefix.withLevel prefix $ do
            -- Instantiate the quantifications for the mock type we created.
            case Type.polytypeDescription t3 of
              Type.Quantify bindings body -> Prefix.instantiate prefix bindings body *> return ()
              _ -> return ()
            -- Get all the names currently bound in our prefix.
            ctx <- Prefix.allBindingNames prefix
            pt1 <- liftDiagnosticWriter (checkPolytype ctx (AST.convertRecoverType ct1))
            pt2 <- liftDiagnosticWriter (checkPolytype ctx (AST.convertRecoverType ct2))
            -- We need monotypes. We can’t take polytypes.
            let t1 = case Type.polytypeDescription pt1 of { Type.Monotype' t -> t; _ -> undefined }
            let t2 = case Type.polytypeDescription pt2 of { Type.Monotype' t -> t; _ -> undefined }
            -- Yay! We can actually call unify now 😉
            let r = currentRange (Type.monotypeRangeStack t1)
            _ <- unify (unifyTestStack r) prefix t1 t2
            -- Return a list of all the bindings in our prefix.
            Prefix.allBindings prefix

      -- Create the values we’re going to snapshot.
      let actualPrefix = Text.Builder.toStrictText (printCompactQuantifierList (map printBindingWithoutInlining (toList allBindings)))
      let actualDiagnostics = Text.Builder.toStrictText (foldMap diagnosticMessageMarkdown (ds2 <> ds3))

      -- Catch any errors before we print.
      seq actualPrefix $ return ()
      seq actualDiagnostics $ return ()

      hPutStrLn h ""
      hPutStrLn h (replicate 80 '-')
      hPutStrLn h ""
      hPutStrLn h "### Input"
      hPutStrLn h "```ite"
      hPutStrLn h (Text.unpack input)
      hPutStrLn h "```"
      hPutStrLn h ""
      hPutStrLn h "### Output"
      hPutStrLn h "```"
      hPutStrLn h (Text.unpack actualPrefix)
      hPutStrLn h "```"
      if Text.null actualDiagnostics then return () else (do
        hPutStrLn h ""
        hPutStrLn h "### Errors"
        hPutStr h (Text.unpack actualDiagnostics))
