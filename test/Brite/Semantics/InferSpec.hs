{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.InferSpec (spec) where

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
import System.IO
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
testData :: [Text]
testData =
  [ "infer(<>, (x: Bool), x)"
  , "infer(<>, (x: Int), x)"
  , "infer(<>, (x: fun<A>(A) -> A), x)"
  , "infer(<>, (x: <A> A), x)"
  , "infer(<>, (x: <A> Int), x)"
  , "infer(<>, (), fun(x) { x })"
  , "infer(<>, (add1: fun(Int) -> Int), add1(42))"
  , "infer(<>, (add1: fun(Int) -> Int), add1(true))"
  , "infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { x }))"
  , "infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Int) }))"
  , "infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Bool) }))"
  , "infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Bool); 42 }))"
  , "infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { true }))"
  , "infer(<>, (f: fun(fun(fun(Int) -> Int) -> Int) -> Int), f(fun(g) { g(42) }))"
  , "infer(<>, (f: fun(fun(fun(Int) -> Int) -> Int) -> Int), f(fun(g) { g(true) }))"
  , "infer(<A>, (add1: fun(Int) -> Int, x: A), add1(x))"
  , "infer(<>, (add1: fun(Int) -> Int, x: !), add1(x))"
  , "infer(<>, (), do { let id = fun(x) { x }; fun(x) { x } })"
  , "infer(<>, (add1: fun(Int) -> Int), fun(x) { add1(x) })"
  , "infer(<>, (), fun(x) { 42 })"
  , "infer(<>, (), fun(z) { fun(x) { x } })"
  , "infer(<>, (id: fun<A>(A) -> A), id(42))"
  , "infer(<>, (id: fun<A>(A) -> A), id(id))"
  , "infer(<>, (id: fun<A>(A) -> A), do { let x = id(42); id })"
  , "infer(<A>, (x: A), x(x))"
  , "infer(<A>, (x: A), do { let x = (x: fun<A>(A) -> A); x(x) })"
  , "infer(<>, (), fun(x) { x(x) })"
  , "infer(<>, (), fun(x) { let x = (x: fun<A>(A) -> A); x(x) })"
  , "infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) }: fun<A = fun<A>(A) -> A>(A) -> A))"
  , "infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(add1))"
  , "infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(42))"
  , "infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x }))"
  , "infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x })(42))"
  , "infer(<>, (), nope)"
  , "infer(<>, (), true)"
  , "infer(<>, (), do { let x = true; x })"
  , "infer(<>, (), do { let x = true; let y = x; y })"
  , "infer(<>, (), do { let x = true; let y = x; x })"
  , "infer(<>, (), fun(x) { true })"
  , "infer(<>, (), fun(x) { let y = x; y })"
  , "infer(<>, (), true(42))"
  , "infer(<>, (), 42(true))"
  , "infer(<>, (), 0b101(true))"
  , "infer(<>, (), 0xFFF(true))"
  , "infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, x: B), choose(x)(42))"
  , "infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, x: B), choose(42)(x))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { choose(x)(42) })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { choose(42)(x) })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { let y = choose(x)(42); x })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { let y = choose(42)(x); x })"
  , "infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, x: B), choose(x)(id))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { choose(x)(id) })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { choose(id)(x) })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { let y = choose(x)(id); x })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { let y = choose(id)(x); x })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(id))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (fun(x) { choose(x)(id) })(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (fun(x) { choose(id)(x) })(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(x) { choose(x)(true) })(42))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(x) { choose(true)(x) })(42))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(f) { fun(x) { choose(f(x))(x) } })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(f) { fun(x) { choose(f(x))(x) } })(fun(x) { x }))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, undefined: !), (fun(f) { fun(x) { choose(f(x))(x) } })(fun(x) { undefined }))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, undefined: !), fun(f) { fun(x) { choose(f(x))(undefined) } })"
  , "infer(<>, (), (42: Bool))"
  , "infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Int) -> Bool) })"
  , "infer(<>, (id: fun<T>(T) -> T), do { let x = true; (id(x): Int) })"
  , "infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Bool) -> Int) })"
  , "infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Int) -> Int) })"
  , "infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Bool) -> Bool) })"
  , "infer(<>, (), (fun(x) { 42 }: fun(Bool) -> Int))"
  , "infer(<>, (), (fun(x) { x }: fun(Bool) -> Int))"
  , "infer(<>, (), (fun(x) { x }: fun(Bool) -> Bool))"
  , "infer(<>, (), ((fun(x) { x }: fun(Bool) -> Bool): fun<A>(A) -> A))"
  , "infer(<A, B>, (f: A, x: B), f(x))"
  , "infer(<>, (), fun(f) { fun(x) { f(x) } })"
  , "infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, add1: fun(Int) -> Int), app(add1))"
  , "infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, add1: fun(Int) -> Int), app(add1)(0))"
  , "infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B), app(fun(x) { x }))"
  , "infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B), app(fun(x) { x })(42))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(fun(x) { x }))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(fun(x) { x })(42))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(42)(fun(x) { x }))"
  , "infer(<>, (), fun(x) { x(x) })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(x)(y); x(y) } })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(y)(x); x(y) } })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(x)(y); y(x) } })"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(y)(x); y(x) } })"
  , "infer(<>, (), fun(x) { let x = (x: fun<A>(A) -> A); x(x) })"
  , "infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(add1))"
  , "infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) }: fun<A = fun<A>(A) -> A>(A) -> A)(add1))"
  , "infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), auto(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(id))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(id))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), choose(id)(auto))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), choose(auto)(id))"
  , "infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), id(auto))"
  , "infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), auto(id))"
  , "infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), (fun(x) { x(id) }))"
  , "infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), (fun(x) { x(id) })(auto))"
  , "infer(<>, (), (fun(x) { x(fun(x) { x }) })(fun(x) { let x = (x: fun<A>(A) -> A); x(x) }))"
  , "infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, auto: fun<A = fun<A>(A) -> A>(A) -> A, id: fun<A>(A) -> A), app(auto)(id))"
  , "infer(<>, (), (fun(f) { fun(x) { f(x) } })(fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x }))"
  , "infer(<>, (undefined: !), fun(x) { undefined })"
  , "infer(<>, (), do { let id = fun(x) { x }; (id: fun<X>(X) -> Int) })"
  , "infer(<>, (), do { let id = fun(x) { x }; let id = (id: fun<X>(X) -> Int); (id: fun<X>(X) -> X) })"
  , "infer(<>, (), do { let f = fun(x) { 42 }; (f: fun<X>(X) -> Bool) })"
  , "infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), auto(add1))"
  , "infer(<>, (add1: fun(Int) -> Int), do { let id = fun(x) { x }; (id: fun<X>(X) -> Int); id })"
  , "infer(<>, (add1: fun(Int) -> Int), do { let id = (fun(x) { x }: fun<X>(X) -> Int); add1(id(true)) })"
  , "infer(<>, (add1: fun(Int) -> Int), do { let id = (fun(x) { true }: fun<X>(X) -> X); add1(id(42)) })"
  , "infer(<>, (add1: fun(Int) -> Int), do { let id = fun(x) { x }; add1(id(true)) })"
  , "infer(<>, (add1: fun(Int) -> Int), do { let id = fun(x) { x }; let add1 = fun(x) { add1(id(x)) }; add1(true) })"
  , "infer(<>, (undefined: !, choose: fun<A>(A) -> fun(A) -> A), choose((undefined: fun<X: fun<A: !, B = !>(A) -> B>(X) -> X))((undefined: fun<X: fun<A = !, B: !>(A) -> B>(X) -> X)))"
  , "infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), (auto: fun<X: fun<A>(A) -> A>(X) -> X))"
  , "infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), (auto: fun<X: fun<A>(A) -> A>(X) -> X)(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X = fun<A>(A) -> A>(X) -> X))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X = fun<A>(A) -> A>(X) -> X)(add1))"
  , "infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X: fun<A>(A) -> A>(X) -> X)(add1))"
  , "infer(<>, (add1: fun(Int) -> Int), (add1: fun<A>(A) -> A))"
  , "infer(<>, (), (fun(x) { x }: fun(Int) -> Int))"
  , "infer(<>, (), (fun(x) { x }: fun<A>(A) -> A)(42))"
  , "infer(<A, B, C>, (A: A, B: B, C: C), if A { B } else { C })"
  , "infer(<>, (), fun(x) { fun(y) { if true { x } else { y } } })"
  , "infer(<>, (), if 42 { true } else { false })"
  , "infer(<>, (), if true { true } else { false })"
  , "infer(<>, (), if true { 1 } else { 0 })"
  , "infer(<>, (), if true { 1 } else { false })"
  , "infer(<>, (), if true { true } else { 0 })"
  , "infer(<>, (), if true { 1 } else {})"
  , "infer(<>, (), if true {} else { 0 })"
  , "infer(<>, (id: fun<A>(A) -> A, add1: fun(Int) -> Int), if true { id } else { add1 })"
  , "infer(<>, (id: fun<A>(A) -> A, add1: fun(Int) -> Int), if true { add1 } else { id })"
  , "infer(<>, (), (42: nope))"
  , "infer(<>, (x: nope), x)"
  , "infer(<T = nope>, (x: T, y: T, choose: fun<A>(A) -> fun(A) -> A), choose(x)(y))"
  , "infer(<T1 = nope, T2 = nope>, (x: T1, y: T2, choose: fun<A>(A) -> fun(A) -> A), choose(x)(y))"
  , "infer(<>, (), (42: !))"
  , "infer(<>, (), fun(id) { let id = (id: fun<A>(A) -> A); id })"
  , "infer(<>, (f: fun<A = fun<B>(B) -> B>(Int) -> A), f(0))"
  , "infer(<>, (f: fun<A = fun<B>(B) -> B>(Int) -> A), f(0)(1))"
  , "infer(<>, (), (fun(x) { x }: fun<X>(X) -> Int))"
  , "infer(<>, (), {})"
  , "infer(<>, (), {a: 42})"
  , "infer(<>, (), {a: true})"
  , "infer(<>, (), {a: 42, b: true})"
  , "infer(<>, (), {a: 42, b: true, c: void})"
  , "infer(<>, (), {a: 42, a: true})"
  , "infer(<>, (), {a: 42, a: true, a: void})"
  , "infer(<>, (), {a: (42: Bool), b: (true: Int)})"
  , "infer(<>, (), {a: (42: Bool), a: (true: Bool)})"
  , "infer(<>, (), ({}: Int))"
  , "infer(<>, (), ({a: 42}: Int))"
  , "infer(<>, (), ({a: 42, b: true}: Int))"
  , "infer(<>, (), ({a: 42, b: true, c: void}: Int))"
  , "infer(<>, (), ({c: void, a: 42, b: true}: Int))"
  , "infer(<>, (), ({a: 42, b: true, c: void, d: 42}: Int))"
  , "infer(<>, (), ({| {}}: Int))"
  , "infer(<>, (), ({a: 42 | {}}: Int))"
  , "infer(<>, (), ({a: 42, b: true | {}}: Int))"
  , "infer(<>, (), ({a: 42, b: true, c: void | {}}: Int))"
  , "infer(<>, (), ({c: void, a: 42, b: true | {}}: Int))"
  , "infer(<>, (), {| {}})"
  , "infer(<>, (), {| ({}: Int)})"
  , "infer(<>, (), {a: 42 | {}})"
  , "infer(<>, (), {a: 42, b: true | {}})"
  , "infer(<>, (), {a: 42 | {b: true}})"
  , "infer(<>, (), {a: 42, b: true, c: void | {}})"
  , "infer(<>, (), {a: 42, b: true | {c: void}})"
  , "infer(<>, (), {a: 42 | {b: true, c: void}})"
  , "infer(<>, (), {a: 42, a: true, a: void | {}})"
  , "infer(<>, (), {a: 42, a: true | {a: void}})"
  , "infer(<>, (), {a: 42 | {a: true, a: void}})"
  , "infer(<>, (), {id: fun(x) { x }})"
  , "infer(<>, (), ({id: fun(x) { x }}: {id: fun<T>(T) -> T}))"
  , "infer(<>, (), ({id: fun(x) { (x: Int) }}: {id: fun<T>(T) -> T}))"
  , "infer(<>, (), fun(r) { if true { { x: 2 | r } } else { { y: 2 | r } } })"
  , "infer(<>, (), do { let o = {id: fun(x) { x }}; o.id(42); o })"
  , "infer(<>, (), fun(o) { o.a })"
  , "infer(<>, (), fun(o) { o.a; o.b })"
  , "infer(<>, (), fun(o) { o.a; o.b; o.c })"
  , "infer(<>, (), fun(o) { o.b; o.a })"
  , "infer(<>, (), fun(o) { (o.a: Int) })"
  , "infer(<>, (), fun(o) { (o.a: Int); (o.b: Bool) })"
  , "infer(<>, (), fun(o) { (o.a: Int); (o.b: Bool); (o.c: void) })"
  , "infer(<>, (), fun(o) { o.a; o.a })"
  , "infer(<>, (), fun(o) { (o.a: Int); (o.a: Bool) })"
  , "infer(<>, (), (fun(o) { o.a })({a: 42}))"
  , "infer(<>, (), (fun(o) { (o.a: Bool) })({a: 42}))"
  , "infer(<>, (), (fun(o) { o.a; o.b })({a: 42}))"
  , "infer(<>, (), {a: 42}.a)"
  , "infer(<>, (), {a: 42, b: true}.a)"
  , "infer(<>, (), {a: 42, b: true, c: void}.a)"
  , "infer(<>, (), {a: 42, b: true, c: void}.b)"
  , "infer(<>, (), {a: 42, b: true, c: void}.c)"
  , "infer(<>, (), {a: 42}.b)"
  , "infer(<>, (), ({hidden: 42}: <T> {hidden: T}))"
  , "infer(<>, (f: fun<T: {}>(T) -> void), f({}))"
  , "infer(<>, (f: fun<T: {}>(T) -> void), f({a: 42}))"
  , "infer(<>, (f: fun<T: {}>(T) -> void), f(42))"
  , "infer(<>, (f: fun<T: { | ! }>(T) -> void), f({}))"
  , "infer(<>, (f: fun<T: { | ! }>(T) -> void), f({a: 42}))"
  , "infer(<>, (f: fun<T: { | ! }>(T) -> void), f(42))"
  , "infer(<>, (), fun(o) { o.p })"
  , "infer(<>, (), ({a: 42, b: false}: {a: Int | !}))"
  , "infer(<T>, (o: T), o.p)"
  , "infer(<>, (error: fun(Bool) -> !), error(true))"
  , "infer(<>, (error: fun(Bool) -> !), (error(true): Int))"
  , "infer(<>, (error: fun(Bool) -> !), (error(true): !))"
  , "infer(<>, (error: fun(Bool) -> !, nope: fun(!) -> void), nope(42))"
  , "infer(<>, (error: fun(Bool) -> !, nope: fun(!) -> void), nope(error(true)))"
  ]

openSnapshotFile :: IO Handle
openSnapshotFile = do
  h <- openFile "test/Brite/Semantics/InferSpecSnapshot.md" WriteMode
  hPutStrLn h "# InferSpec"
  return h

closeSnapshotFile :: Handle -> IO ()
closeSnapshotFile h = do
  hPutStrLn h ""
  hPutStrLn h (replicate 80 '-')
  hClose h

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
spec = beforeAll openSnapshotFile $ afterAll closeSnapshotFile $
  flip traverse_ testData $ \input ->
    it (Text.unpack input) $ \h -> do
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

      -- Build the expected results.
      let
        actualSolution = Text.Builder.toStrictText $
          Text.Builder.singleton '(' <>
          printCompactQuantifierList (map printBindingWithoutInlining (toList allBindings)) <>
          Text.Builder.fromText ", " <>
          printCompactType (printPolytypeWithoutInlining expressionType) <>
          Text.Builder.singleton ')'

      let actualDiagnostics = Text.Builder.toStrictText (foldMap diagnosticMessageMarkdown (ds2 <> ds3))

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
      hPutStrLn h (Text.unpack actualSolution)
      hPutStrLn h "```"
      if Text.null actualDiagnostics then return () else (do
        hPutStrLn h ""
        hPutStrLn h "### Errors"
        hPutStr h (Text.unpack actualDiagnostics))
