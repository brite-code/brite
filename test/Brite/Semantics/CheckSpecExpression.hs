{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.CheckSpecExpression (spec) where

import Brite.Diagnostic
import qualified Brite.Semantics.AST as AST
import Brite.Semantics.Check
import Brite.Semantics.CheckMonad
import qualified Brite.Semantics.Prefix as Prefix
import qualified Brite.Semantics.Type as Type
import Brite.Semantics.TypePrinter
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Glyph
import Brite.Syntax.Identifier
import Brite.Syntax.Parser
import Brite.Syntax.ParserFramework
import Brite.Syntax.Printer
import Brite.Syntax.TokenStream
import Data.Foldable (traverse_, foldlM, toList)
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Custom as Text.Builder
import Test.Hspec hiding (context)

-- In the [MLF Thesis][1] Section 7.1 type inference is described as:
--
-- > A type inference problem is a triple `(Q, Γ, a)`, where all free type variables in `Γ` are
-- > bound in `Q`. A pair `(Q', t)` is a solution to this problem if Q ⊑ Q'
-- > and `(Q') Γ ⊢ a : t` holds.
--
-- So we write our tests directly in this form so we can reason about our tests in theory.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
testData :: [(Text, Text, [Text])]
testData =
  [ ("infer(<>, (x: Bool), x)", "(<>, Bool)", [])
  , ("infer(<>, (x: Int), x)", "(<>, Int)", [])
  , ("infer(<>, (x: fun<A>(A) -> A), x)", "(<>, fun<A>(A) -> A)", [])
  , ("infer(<>, (x: <A> A), x)", "(<>, <A> A)", [])
  , ("infer(<>, (x: <A> Int), x)", "(<>, <A> Int)", [])
  , ("infer(<>, (), fun(x) { x })", "(<>, fun<Type1>(Type1) -> Type1)", [])
  , ("infer(<>, (add1: fun(Int) -> Int), add1(42))", "(<>, Int)", [])
  , ("infer(<>, (add1: fun(Int) -> Int), add1(true))", "(<>, Int)", ["(0:40-0:44) Can not call `add1` because we have a boolean but we want an integer. [(0:21-0:24): `Int`]"])
  , ("infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { x }))", "(<>, Int)", [])
  , ("infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Int) }))", "(<>, Int)", [])
  , ("infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Bool) }))", "(<>, Int)", ["(0:59-0:63) Can not call `f` because we have a boolean but we want an integer. [(0:22-0:25): `Int`]", "(0:59-0:63) Can not call `f` because we have a boolean but we want an integer. [(0:30-0:33): `Int`]"])
  , ("infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Bool); 42 }))", "(<>, Int)", ["(0:59-0:63) Can not call `f` because we have a boolean but we want an integer. [(0:22-0:25): `Int`]"])
  , ("infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { true }))", "(<>, Int)", ["(0:55-0:59) Can not call `f` because we have a boolean but we want an integer. [(0:30-0:33): `Int`]"])
  , ("infer(<>, (f: fun(fun(fun(Int) -> Int) -> Int) -> Int), f(fun(g) { g(42) }))", "(<>, Int)", [])
  , ("infer(<>, (f: fun(fun(fun(Int) -> Int) -> Int) -> Int), f(fun(g) { g(true) }))", "(<>, Int)", ["(0:69-0:73) Can not call `f` because we have a boolean but we want an integer. [(0:26-0:29): `Int`]"])
  , ("infer(<A>, (add1: fun(Int) -> Int, x: A), add1(x))", "(<A = Int>, Int)", [])
  , ("infer(<>, (add1: fun(Int) -> Int, x: !), add1(x))", "(<>, Int)", [])
  , ("infer(<>, (), do { let id = fun(x) { x }; fun(x) { x } })", "(<>, fun<Type1>(Type1) -> Type1)", [])
  , ("infer(<>, (add1: fun(Int) -> Int), fun(x) { add1(x) })", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (), fun(x) { 42 })", "(<>, fun<Type1>(Type1) -> Int)", [])
  , ("infer(<>, (), fun(z) { fun(x) { x } })", "(<>, fun<Type1, Type2: fun<Type2>(Type2) -> Type2>(Type1) -> Type2)", [])
  , ("infer(<>, (id: fun<A>(A) -> A), id(42))", "(<>, Int)", [])
  , ("infer(<>, (id: fun<A>(A) -> A), id(id))", "(<>, fun<A>(A) -> A)", [])
  , ("infer(<>, (id: fun<A>(A) -> A), do { let x = id(42); id })", "(<>, fun<A>(A) -> A)", [])
  , ("infer(<A>, (x: A), x(x))", "(<A>, !)", ["(0:19-0:23) Can not call `x` because the type checker infers an infinite type."])
  , ("infer(<A>, (x: A), do { let x = (x: fun<A>(A) -> A); x(x) })", "(<A = fun<A>(A) -> A>, fun<A>(A) -> A)", [])
  , ("infer(<>, (), fun(x) { x(x) })", "(<>, fun<Type1, Type2>(Type1) -> Type2)", ["(0:23-0:27) Can not call `x` because the type checker infers an infinite type."])
  , ("infer(<>, (), fun(x) { let x = (x: fun<A>(A) -> A); x(x) })", "(<>, fun<Type1 = fun<A>(A) -> A, Type2: fun<A>(A) -> A>(Type1) -> Type2)", [])
  , ("infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) }: fun<A = fun<A>(A) -> A>(A) -> A))", "(<>, fun<A = fun<A>(A) -> A>(A) -> A)", [])
  , ("infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(add1))", "(<>, fun<A>(A) -> A)", ["(0:82-0:86) Can not call function because `fun(Int) -> Int` is not `fun<A>(A) -> A`. [(0:17-0:32): `fun(Int) -> Int`, (0:57-0:71): `fun<A>(A) -> A`]"])
  , ("infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(42))", "(<>, fun<A>(A) -> A)", ["fun(A) -> A ≢ Int"])
  , ("infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x }))", "(<>, fun<A>(A) -> A)", [])
  , ("infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x })(42))", "(<>, Int)", [])
  , ("infer(<>, (), nope)", "(<>, !)", ["Unbound variable `nope`."])
  , ("infer(<>, (), true)", "(<>, Bool)", [])
  , ("infer(<>, (), do { let x = true; x })", "(<>, Bool)", [])
  , ("infer(<>, (), do { let x = true; let y = x; y })", "(<>, Bool)", [])
  , ("infer(<>, (), do { let x = true; let y = x; x })", "(<>, Bool)", [])
  , ("infer(<>, (), fun(x) { true })", "(<>, fun<Type1>(Type1) -> Bool)", [])
  , ("infer(<>, (), fun(x) { let y = x; y })", "(<>, fun<Type1>(Type1) -> Type1)", [])
  , ("infer(<>, (), true(42))", "(<>, !)", [])
  , ("infer(<>, (), 42(true))", "(<>, !)", ["Bool ≢ fun(Int) -> Type1"])
  , ("infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, x: B), choose(x)(42))", "(<B = Int>, Int)", [])
  , ("infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, x: B), choose(42)(x))", "(<B = Int>, Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { choose(x)(42) })", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { choose(42)(x) })", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { let y = choose(x)(42); x })", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { let y = choose(42)(x); x })", "(<>, fun(Int) -> Int)", [])
  , ("infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, x: B), choose(x)(id))", "(<B: fun<A>(A) -> A>, B)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { choose(x)(id) })", "(<>, fun<Type1: fun<A>(A) -> A>(Type1) -> Type1)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { choose(id)(x) })", "(<>, fun<Type1: fun<A>(A) -> A>(Type1) -> Type1)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { let y = choose(x)(id); x })", "(<>, fun<Type1: fun<A>(A) -> A>(Type1) -> Type1)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { let y = choose(id)(x); x })", "(<>, fun<Type1: fun<A>(A) -> A>(Type1) -> Type1)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(add1))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(id))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (fun(x) { choose(x)(id) })(add1))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (fun(x) { choose(id)(x) })(add1))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(x) { choose(x)(true) })(42))", "(<>, Bool)", ["Bool ≢ Int"])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(x) { choose(true)(x) })(42))", "(<>, Bool)", ["Bool ≢ Int"])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(f) { fun(x) { choose(f(x))(x) } })", "(<>, fun<Type3>(fun(Type3) -> Type3) -> fun(Type3) -> Type3)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(f) { fun(x) { choose(f(x))(x) } })(fun(x) { x }))", "(<>, fun<Type4>(Type4) -> Type4)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, undefined: !), (fun(f) { fun(x) { choose(f(x))(x) } })(fun(x) { undefined }))", "(<>, fun<Type4>(Type4) -> Type4)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, undefined: !), fun(f) { fun(x) { choose(f(x))(undefined) } })", "(<>, fun<Type2, Type3>(fun(Type2) -> Type3) -> fun(Type2) -> Type3)", [])
  , ("infer(<>, (), (42: Bool))", "(<>, Bool)", ["Int ≢ Bool"])
  , ("infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Int) -> Bool) })", "(<>, fun(Int) -> Bool)", ["Bool ≢ Int", "Int ≢ Bool"])
  , ("infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Bool) -> Int) })", "(<>, fun(Bool) -> Int)", [])
  , ("infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Int) -> Int) })", "(<>, fun(Int) -> Int)", ["Bool ≢ Int"])
  , ("infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Bool) -> Bool) })", "(<>, fun(Bool) -> Bool)", ["Int ≢ Bool"])
  , ("infer(<>, (), (fun(x) { 42 }: fun(Bool) -> Int))", "(<>, fun(Bool) -> Int)", [])
  , ("infer(<>, (), (fun(x) { x }: fun(Bool) -> Int))", "(<>, fun(Bool) -> Int)", ["Bool ≢ Int"])
  , ("infer(<>, (), (fun(x) { x }: fun(Bool) -> Bool))", "(<>, fun(Bool) -> Bool)", [])
  , ("infer(<>, (), ((fun(x) { x }: fun(Bool) -> Bool): fun<A>(A) -> A))", "(<>, fun<A>(A) -> A)", ["fun<A>(A) -> A ≢ fun(Bool) -> Bool"])
  , ("infer(<A, B>, (f: A, x: B), f(x))", "(<B, Type1, A = fun(B) -> Type1>, Type1)", [])
  , ("infer(<>, (), fun(f) { fun(x) { f(x) } })", "(<>, fun<Type2, Type3>(fun(Type2) -> Type3) -> fun(Type2) -> Type3)", [])
  , ("infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, add1: fun(Int) -> Int), app(add1))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, add1: fun(Int) -> Int), app(add1)(0))", "(<>, Int)", [])
  , ("infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B), app(fun(x) { x }))", "(<>, fun<B>(B) -> B)", [])
  , ("infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B), app(fun(x) { x })(42))", "(<>, Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(fun(x) { x }))", "(<>, fun<Type2: fun<Type1>(Type1) -> Type1>(Type2) -> Type2)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(fun(x) { x })(42))", "(<>, fun<Type1>(Type1) -> Type1)", ["fun(Type4) -> Type4 ≢ Int"])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(42)(fun(x) { x }))", "(<>, Int)", ["Int ≢ fun(Type3) -> Type3"])
  , ("infer(<>, (), fun(x) { x(x) })", "(<>, fun<Type1, Type2>(Type1) -> Type2)", ["(0:23-0:27) Can not call `x` because the type checker infers an infinite type."])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(x)(y); x(y) } })", "(<>, fun<Type1, Type2: fun<Type3>(Type1) -> Type3>(Type1) -> Type2)", ["Infinite type since `Type1` occurs in `fun(Type2) -> Type3`."])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(y)(x); x(y) } })", "(<>, fun<Type1, Type2: fun<Type3>(Type1) -> Type3>(Type1) -> Type2)", ["Infinite type since `Type1` occurs in `fun(Type2) -> Type3`."])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(x)(y); y(x) } })", "(<>, fun<Type1, Type2: fun<Type3>(Type1) -> Type3>(Type1) -> Type2)", ["Infinite type since `Type1` occurs in `fun(Type1) -> Type3`."])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(y)(x); y(x) } })", "(<>, fun<Type1, Type2: fun<Type3>(Type1) -> Type3>(Type1) -> Type2)", ["Infinite type since `Type1` occurs in `fun(Type1) -> Type3`."])
  , ("infer(<>, (), fun(x) { let x = (x: fun<A>(A) -> A); x(x) })", "(<>, fun<Type1 = fun<A>(A) -> A, Type2: fun<A>(A) -> A>(Type1) -> Type2)", [])
  , ("infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(add1))", "(<>, fun<A>(A) -> A)", ["(0:82-0:86) Can not call function because `fun(Int) -> Int` is not `fun<A>(A) -> A`. [(0:17-0:32): `fun(Int) -> Int`, (0:57-0:71): `fun<A>(A) -> A`]"])
  , ("infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) }: fun<A = fun<A>(A) -> A>(A) -> A)(add1))", "(<>, fun<A>(A) -> A)", ["fun<A>(A) -> A ≢ fun(Int) -> Int"])
  , ("infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), auto(add1))", "(<>, fun<A>(A) -> A)", ["fun<A>(A) -> A ≢ fun(Int) -> Int"])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(id))", "(<>, fun<A>(A) -> A)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(add1))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id))", "(<>, fun<Type2: fun<A>(A) -> A>(Type2) -> Type2)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1))", "(<>, fun(fun(Int) -> Int) -> fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(add1))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(id))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), choose(id)(auto))", "(<>, fun<A = fun<A>(A) -> A>(A) -> A)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), choose(auto)(id))", "(<>, fun<A = fun<A>(A) -> A>(A) -> A)", [])
  , ("infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), id(auto))", "(<>, fun<A = fun<A>(A) -> A>(A) -> A)", [])
  , ("infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), auto(id))", "(<>, fun<A2>(A2) -> A2)", [])
  , ("infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), (fun(x) { x(id) }))", "(<>, fun<Type2: fun<A>(A) -> A, Type3>(fun(Type2) -> Type3) -> Type3)", [])
  , ("infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), (fun(x) { x(id) })(auto))", "(<>, fun<A2>(A2) -> A2)", [])
  , ("infer(<>, (), (fun(x) { x(fun(x) { x }) })(fun(x) { let x = (x: fun<A>(A) -> A); x(x) }))", "(<>, fun<A>(A) -> A)", [])
  , ("infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, auto: fun<A = fun<A>(A) -> A>(A) -> A, id: fun<A>(A) -> A), app(auto)(id))", "(<>, fun<A>(A) -> A)", [])
  , ("infer(<>, (), (fun(f) { fun(x) { f(x) } })(fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x }))", "(<>, fun<A>(A) -> A)", [])
  , ("infer(<>, (undefined: !), fun(x) { undefined })", "(<>, fun<Type1, Type2>(Type1) -> Type2)", [])
  , ("infer(<>, (), do { let id = fun(x) { x }; (id: fun<x>(x) -> Int) })", "(<>, fun<x>(x) -> Int)", ["fun<x>(x) -> Int ≢ fun(Int) -> Int"])
  , ("infer(<>, (), do { let id = fun(x) { x }; let id = (id: fun<X>(X) -> Int); (id: fun<X>(X) -> X) })", "(<>, fun<X>(X) -> X)", ["fun<x>(x) -> Int ≢ fun(Int) -> Int", "fun<x>(x) -> x ≢ fun(Int) -> Int"])
  , ("infer(<>, (), do { let f = fun(x) { 42 }; (f: fun<X>(X) -> Bool) })", "(<>, fun<X>(X) -> Bool)", ["Int ≢ Bool"])
  , ("infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), auto(add1))", "(<>, fun<A>(A) -> A)", ["fun<A>(A) -> A ≢ fun(Int) -> Int"])
  , ("infer(<>, (add1: fun(Int) -> Int), do { let id = (fun(x) { x }: fun<X>(X) -> Int); add1(id(true)) })", "(<>, Int)", ["fun<x>(x) -> Int ≢ fun(Int) -> Int"])
  , ("infer(<>, (add1: fun(Int) -> Int), do { let id = (fun(x) { true }: fun<x>(x) -> x); add1(id(42)) })", "(<>, Int)", ["fun<x>(x) -> x ≢ fun(Bool) -> Bool"])
  , ("infer(<>, (add1: fun(Int) -> Int), do { let id = (fun(x) { true }: fun<x>(x) -> x); add1(id(42)) })", "(<>, Int)", ["fun<x>(x) -> x ≢ fun(Bool) -> Bool"])
  , ("infer(<>, (add1: fun(Int) -> Int), do { let id = fun(x) { x }; add1(id(true)) })", "(<>, Int)", ["Int ≢ Bool"])
  , ("infer(<>, (add1: fun(Int) -> Int), do { let id = fun(x) { x }; let add1 = fun(x) { add1(id(x)) }; add1(true) })", "(<>, Int)", ["Int ≢ Bool"])
  , ("infer(<>, (undefined: !, choose: fun<A>(A) -> fun(A) -> A), choose((undefined: fun<X: fun<A: !, B = !>(A) -> B>(X) -> X))((undefined: fun<X: fun<A = !, B: !>(A) -> B>(X) -> X)))", "(<>, fun<X: fun<A = !, B = !>(A) -> B>(X) -> X)", [])
  , ("infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), (auto: fun<X: fun<A>(A) -> A>(X) -> X))", "(<>, fun<X: fun<A>(A) -> A>(X) -> X)", ["fun<X: fun<A>(A) -> A>(X) -> X ≢ fun<A = fun<A2>(A2) -> A2>(A) -> A"])
  , ("infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), (auto: fun<X: fun<A>(A) -> A>(X) -> X)(add1))", "(<>, fun(Int) -> Int)", ["fun<X: fun<A>(A) -> A>(X) -> X ≢ fun<A = fun<A2>(A2) -> A2>(A) -> A"])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X = fun<A>(A) -> A>(X) -> X))", "(<>, fun<X = fun<A>(A) -> A>(X) -> X)", [])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X = fun<A>(A) -> A>(X) -> X)(add1))", "(<>, fun<A>(A) -> A)", ["fun<A>(A) -> A ≢ fun(Int) -> Int"])
  , ("infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X: fun<A>(A) -> A>(X) -> X)(add1))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (add1: fun(Int) -> Int), (add1: fun<A>(A) -> A))", "(<>, fun<A>(A) -> A)", ["fun<A>(A) -> A ≢ fun(Int) -> Int"])
  , ("infer(<>, (), (fun(x) { x }: fun(Int) -> Int))", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (), (fun(x) { x }: fun<A>(A) -> A)(42))", "(<>, Int)", [])
  , ("infer(<A, B, C>, (A: A, B: B, C: C), if A { B } else { C })", "(<A = Bool, C, B = C>, B)", [])
  , ("infer(<>, (), fun(x) { fun(y) { if true { x } else { y } } })", "(<>, fun<Type1>(Type1) -> fun(Type1) -> Type1)", [])
  , ("infer(<>, (), if 42 { true } else { false })", "(<>, Bool)", ["Int ≢ Bool"])
  , ("infer(<>, (), if true { true } else { false })", "(<>, Bool)", [])
  , ("infer(<>, (), if true { 1 } else { 0 })", "(<>, Int)", [])
  , ("infer(<>, (), if true { 1 } else { false })", "(<>, Int)", ["Int ≢ Bool"])
  , ("infer(<>, (), if true { true } else { 0 })", "(<>, Bool)", ["Bool ≢ Int"])
  , ("infer(<>, (id: fun<A>(A) -> A, add1: fun(Int) -> Int), if true { id } else { add1 })", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (id: fun<A>(A) -> A, add1: fun(Int) -> Int), if true { add1 } else { id })", "(<>, fun(Int) -> Int)", [])
  , ("infer(<>, (), (42: nope))", "(<>, !)", ["Unbound variable `nope`."])
  , ("infer(<>, (x: nope), x)", "(<>, !)", ["Unbound variable `nope`."])
  , ("infer(<T = nope>, (x: T, y: T, choose: fun<A>(A) -> fun(A) -> A), choose(x)(y))", "(<T = !>, T)", ["Unbound variable `nope`."])
  , ("infer(<T1 = nope, T2 = nope>, (x: T1, y: T2, choose: fun<A>(A) -> fun(A) -> A), choose(x)(y))", "(<T1 = !, T2 = T1>, T1)", ["Unbound variable `nope`.", "Unbound variable `nope`."])
  , ("infer(<>, (), (42: !))", "(<>, !)", ["! ≢ Int"])
  , ("infer(<>, (), fun(id) { let id = (id: fun<A>(A) -> A); id })", "(<>, fun<Type1 = fun<A>(A) -> A, Type2: fun<A>(A) -> A>(Type1) -> Type2)", [])
  , ("infer(<>, (f: fun<A = fun<B>(B) -> B>(Int) -> A), f(0))", "(<>, fun<B>(B) -> B)", [])
  , ("infer(<>, (f: fun<A = fun<B>(B) -> B>(Int) -> A), f(0)(1))", "(<>, Int)", [])
  , ("infer(<>, (), (fun(x) { x }: fun<X>(X) -> Int))", "(<>, fun<X>(X) -> Int)", ["(0:15-0:27) Can not change this type because we have `fun(Int) -> Int` but we want `fun<X>(X) -> Int`."])
  ]

inferParser :: Parser (Recover CST.QuantifierList, CST.CommaList (Identifier, Recover CST.Type), Recover CST.Expression)
inferParser = identifier *> glyph ParenLeft *> args <* glyph ParenRight
  where
    args =
      (,,)
        <$> (retry tryQuantifierListParser <* glyph Comma)
        <*> (glyph ParenLeft *> context <* glyph ParenRight <* glyph Comma)
        <*> expressionParser

    context = commaList $
      (,) <$> (fst <$> tryIdentifier) <&> (glyph Colon *> typeParser)

spec :: Spec
spec = do
  flip traverse_ testData $ \(input, expectedSolution, expectedDiagnostics) ->
    it (Text.unpack input) $ do
      let ((cqs, cts, ce), ds1) = runDiagnosticWriter (fst <$> (runParser inferParser (tokenize input)))
      if null ds1 then return () else error (Text.Builder.toString (foldMap diagnosticMessageMarkdown ds1))
      let
        -- Use the quantifier list to quantify a boolean type. Could be anything really. We just
        -- need to send it through our conversion and type checking pipeline.
        (t3, ds2) = runDiagnosticWriter . checkPolytype mempty . AST.convertRecoverType . Ok $ case cqs of
          Recover _ _ _ -> undefined
          Fatal _ _ -> undefined
          Ok cqs' -> CST.QuantifiedType cqs' (Ok (CST.VariableType (CST.Name (unsafeIdentifier "Bool") undefined)))

        -- Run some code in the check monad...
        ((expressionType, allBindings), ds3) = runCheck $ do
          prefix <- Prefix.new
          Prefix.withLevel prefix $ do
            -- Instantiate the quantifications for the mock type we created.
            case Type.polytypeDescription t3 of
              Type.Quantify bindings body -> Prefix.instantiate prefix bindings body *> return ()
              _ -> return ()
            -- Get all the names currently bound in our prefix.
            typeContext <- Prefix.allBindingNames prefix
            -- Now that we have our type context, take the comma separated list of names to types
            -- that we want in our context and check all of those types. Provide the type context
            -- to those types so that we won’t panic if it references a name in context.
            context <-
              foldlM
                (\context recoverItem ->
                  case recoverItem of
                    Recover _ _ _ -> undefined
                    Fatal _ _ -> undefined
                    Ok (name, t') -> do
                      t <- liftDiagnosticWriter $ checkPolytype typeContext (AST.convertRecoverType t')
                      return (HashMap.insert name t context))
                HashMap.empty
                (commaListItems cts)
            -- Yay! We can actually do our type inference now 😉
            (expressionType', _) <- checkExpression prefix context (AST.convertRecoverExpression ce)
            -- Get all the bindings in our prefix.
            allBindings' <- Prefix.allBindings prefix
            -- Return the expression type and a list of all the bindings in our prefix.
            return (expressionType', allBindings')

      -- Compare the actual solution to the expected solution.
      let
        actualSolution = Text.Builder.toStrictText $
          Text.Builder.singleton '(' <>
          printCompactQuantifierList (map printBindingWithoutInlining (toList allBindings)) <>
          Text.Builder.fromText ", " <>
          printCompactType (printPolytypeWithoutInlining expressionType) <>
          Text.Builder.singleton ')'

      actualSolution `shouldBe` expectedSolution

      -- Compare all the expected diagnostics to each other.
      let actualDiagnostics = map (Text.Builder.toStrictText . diagnosticMessageCompact) (toList (ds2 <> ds3))
      actualDiagnostics `shouldBe` expectedDiagnostics
