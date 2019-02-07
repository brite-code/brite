{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.CheckTypeSpec (spec) where

import Brite.Diagnostic
import Brite.Semantics.AST (convertRecoverType)
import Brite.Semantics.Check (checkPolytype)
import Brite.Semantics.TypePrinter (printPolytypeWithoutInlining)
import Brite.Syntax.Identifier
import Brite.Syntax.Parser (parseType)
import Brite.Syntax.Printer (printCompactType)
import Brite.Syntax.TokenStream (tokenize)
import Data.Foldable (traverse_, toList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import Test.Hspec

testData :: [(Text, Text, [Text])]
testData =
  [ ("X", "X", [])
  , ("void", "void", [])
  , ("Bool", "Bool", [])
  , ("Int", "Int", [])
  , ("!", "!", [])
  , ("Nope", "!", ["(0:0-0:4) Can not find `Nope`."])
  , ("fun(Bool) -> Int", "fun(Bool) -> Int", [])
  , ("fun(X) -> Nope", "fun<Type1>(X) -> Type1", ["(0:10-0:14) Can not find `Nope`."])
  , ("fun(Nope) -> Nope", "fun<Type1 = !, Type2>(Type1) -> Type2", ["(0:4-0:8) Can not find `Nope`.", "(0:13-0:17) Can not find `Nope`."])
  , ("fun(Nope1) -> Nope2", "fun<Type1 = !, Type2>(Type1) -> Type2", ["(0:4-0:9) Can not find `Nope1`.", "(0:14-0:19) Can not find `Nope2`."])
  , ("fun(X) -> Y", "fun(X) -> Y", [])
  , ("fun(X) -> fun(Y) -> Z", "fun(X) -> fun(Y) -> Z", [])
  , ("(X)", "X", [])
  , ("(!)", "!", [])
  , ("(Nope)", "!", ["(0:1-0:5) Can not find `Nope`."])
  , ("fun(X) -> (Nope)", "fun<Type1>(X) -> Type1", ["(0:11-0:15) Can not find `Nope`."])
  , ("(fun(X) -> Y)", "fun(X) -> Y", [])
  , ("(fun(X) -> fun(Y) -> Z)", "fun(X) -> fun(Y) -> Z", [])
  , ("fun(X) -> (fun(Y) -> Z)", "fun(X) -> fun(Y) -> Z", [])
  , ("fun<T>(T) -> T", "fun<T>(T) -> T", [])
  , ("fun<T, U>(T) -> T", "fun<T, U>(T) -> T", [])
  , ("fun<T, U: !>(T) -> T", "fun<T, U>(T) -> T", [])
  , ("fun<T, U = !>(T) -> T", "fun<T, U = !>(T) -> T", [])
  , ("fun<T, U: X>(T) -> T", "fun<T, U: X>(T) -> T", [])
  , ("fun<T, U = X>(T) -> T", "fun<T, U = X>(T) -> T", [])
  , ("fun<T, U: Nope>(T) -> T", "fun<T, U>(T) -> T", ["(0:10-0:14) Can not find `Nope`."])
  , ("fun<T, U = Nope>(T) -> T", "fun<T, U = !>(T) -> T", ["(0:11-0:15) Can not find `Nope`."])
  , ("fun<U, T>(T) -> T", "fun<U, T>(T) -> T", [])
  , ("fun<U: !, T>(T) -> T", "fun<U, T>(T) -> T", [])
  , ("fun<U = !, T>(T) -> T", "fun<U = !, T>(T) -> T", [])
  , ("fun<U: X, T>(T) -> T", "fun<U: X, T>(T) -> T", [])
  , ("fun<U = X, T>(T) -> T", "fun<U = X, T>(T) -> T", [])
  , ("fun<U: Nope, T>(T) -> T", "fun<U, T>(T) -> T", ["(0:7-0:11) Can not find `Nope`."])
  , ("fun<U = Nope, T>(T) -> T", "fun<U = !, T>(T) -> T", ["(0:8-0:12) Can not find `Nope`."])
  , ("<T> T", "<T> T", [])
  , ("<T, U> T", "<T, U> T", [])
  , ("<T, U: !> T", "<T, U> T", [])
  , ("<T, U = !> T", "<T, U = !> T", [])
  , ("<T, U: X> T", "<T, U: X> T", [])
  , ("<T, U = X> T", "<T, U = X> T", [])
  , ("<T, U: Nope> T", "<T, U> T", ["(0:7-0:11) Can not find `Nope`."])
  , ("<T, U = Nope> T", "<T, U = !> T", ["(0:8-0:12) Can not find `Nope`."])
  , ("<U, T> T", "<U, T> T", [])
  , ("<U: !, T> T", "<U, T> T", [])
  , ("<U = !, T> T", "<U = !, T> T", [])
  , ("<U: X, T> T", "<U: X, T> T", [])
  , ("<U = X, T> T", "<U = X, T> T", [])
  , ("<U: Nope, T> T", "<U, T> T", ["(0:4-0:8) Can not find `Nope`."])
  , ("<U = Nope, T> T", "<U = !, T> T", ["(0:5-0:9) Can not find `Nope`."])
  , ("fun(X) -> !", "fun<Type1>(X) -> Type1", [])
  , ("fun(!) -> X", "fun<Type1 = !>(Type1) -> X", [])
  , ("fun(!) -> !", "fun<Type1 = !, Type2>(Type1) -> Type2", [])
  , ("fun(<T> T) -> X", "fun<Type1 = <T> T>(Type1) -> X", [])
  , ("fun(X) -> <T> T", "fun<Type1: <T> T>(X) -> Type1", [])
  , ("fun(<X> X) -> X", "fun<Type1 = <X> X>(Type1) -> X", [])
  , ("fun(X) -> <X> X", "fun<Type1: <X> X>(X) -> Type1", [])
  , ("fun(<T> T) -> <T> T", "fun<Type1 = <T> T, Type2: <T> T>(Type1) -> Type2", [])
  , ("fun(fun<T>(T) -> T) -> X", "fun<Type1 = fun<T>(T) -> T>(Type1) -> X", [])
  , ("fun(X) -> fun<T>(T) -> T", "fun<Type1: fun<T>(T) -> T>(X) -> Type1", [])
  , ("fun(fun<X>(X) -> X) -> X", "fun<Type1 = fun<X>(X) -> X>(Type1) -> X", [])
  , ("fun(X) -> fun<X>(X) -> X", "fun<Type1: fun<X>(X) -> X>(X) -> Type1", [])
  , ("fun(fun<T>(T) -> T) -> fun<T>(T) -> T", "fun<Type1 = fun<T>(T) -> T, Type2: fun<T>(T) -> T>(Type1) -> Type2", [])
  , ("fun(fun(!) -> !) -> !", "fun<Type1 = !, Type2, Type3>(fun(Type1) -> Type2) -> Type3", [])
  , ("fun(fun(fun(!) -> !) -> !) -> !", "fun<Type1 = !, Type2, Type3, Type4>(fun(fun(Type1) -> Type2) -> Type3) -> Type4", [])
  , ("fun(fun(fun(fun(!) -> !) -> !) -> !) -> !", "fun<Type1 = !, Type2, Type3, Type4, Type5>(fun(fun(fun(Type1) -> Type2) -> Type3) -> Type4) -> Type5", [])
  , ("fun(X) -> fun<T: fun(X) -> !>(T) -> T", "fun<Type1: fun<T: fun<Type1>(X) -> Type1>(T) -> T>(X) -> Type1", [])
  , ("fun(X) -> fun<T: fun(X) -> fun<T: fun(X) -> !>(T) -> T>(T) -> T", "fun<Type1: fun<T: fun<Type1: fun<T: fun<Type1>(X) -> Type1>(T) -> T>(X) -> Type1>(T) -> T>(X) -> Type1", [])
  , ("fun(T) -> fun<T: fun(T) -> !>(T) -> T", "fun<Type1 = !, Type2: fun<T: fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> T>(Type1) -> Type2", ["(0:4-0:5) Can not find `T`.", "(0:21-0:22) Can not find `T`."])
  , ("fun(T) -> fun<T: fun(T) -> fun<T: fun(T) -> !>(T) -> T>(T) -> T", "fun<Type1 = !, Type2: fun<T: fun<Type1 = !, Type2: fun<T: fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> T>(Type1) -> Type2>(T) -> T>(Type1) -> Type2", ["(0:4-0:5) Can not find `T`.", "(0:21-0:22) Can not find `T`.", "(0:38-0:39) Can not find `T`."])
  , ("fun(fun<T = fun(!) -> X>(T) -> T) -> X", "fun<Type1 = fun<T = fun<Type1 = !>(Type1) -> X>(T) -> T>(Type1) -> X", [])
  , ("fun(fun<T = fun(fun<T = fun(!) -> X>(T) -> T) -> X>(T) -> T) -> X", "fun<Type1 = fun<T = fun<Type1 = fun<T = fun<Type1 = !>(Type1) -> X>(T) -> T>(Type1) -> X>(T) -> T>(Type1) -> X", [])
  , ("fun(fun<T = fun(!) -> T>(T) -> T) -> T", "fun<Type1 = fun<T = fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> T, Type2>(Type1) -> Type2", ["(0:22-0:23) Can not find `T`.", "(0:37-0:38) Can not find `T`."])
  , ("fun(fun<T = fun(fun<T = fun(!) -> T>(T) -> T) -> T>(T) -> T) -> T", "fun<Type1 = fun<T = fun<Type1 = fun<T = fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> T, Type2>(Type1) -> Type2>(T) -> T, Type2>(Type1) -> Type2", ["(0:34-0:35) Can not find `T`.", "(0:49-0:50) Can not find `T`.", "(0:64-0:65) Can not find `T`."])
  , ("fun(fun<T: fun(!) -> !>(T) -> Int) -> Int", "fun<Type1 = fun<T: fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> Int>(Type1) -> Int", [])
  , ("fun<Type1>(Type1) -> !", "fun<Type1, Type2>(Type1) -> Type2", [])
  , ("fun<Type2>(Type2) -> !", "fun<Type2, Type1>(Type2) -> Type1", [])
  , ("fun(Type1) -> !", "fun<Type1 = !, Type2>(Type1) -> Type2", ["(0:4-0:9) Can not find `Type1`."])
  , ("fun(Type1) -> <T> T", "fun<Type1 = !, Type2: <T> T>(Type1) -> Type2", ["(0:4-0:9) Can not find `Type1`."])
  , ("fun(Type1) -> fun<T>(T) -> T", "fun<Type1 = !, Type2: fun<T>(T) -> T>(Type1) -> Type2", ["(0:4-0:9) Can not find `Type1`."])
  , ("fun(!) -> Type1", "fun<Type1 = !, Type2>(Type1) -> Type2", ["(0:10-0:15) Can not find `Type1`."])
  , ("fun(<T> T) -> Type1", "fun<Type1 = <T> T, Type2>(Type1) -> Type2", ["(0:14-0:19) Can not find `Type1`."])
  , ("fun(fun<T>(T) -> T) -> Type1", "fun<Type1 = fun<T>(T) -> T, Type2>(Type1) -> Type2", ["(0:23-0:28) Can not find `Type1`."])
  , ("fun<T>(!) -> !", "fun<T, Type1 = !, Type2>(Type1) -> Type2", [])
  , ("fun<Type2>(!) -> !", "fun<Type2, Type1 = !, Type3>(Type1) -> Type3", [])
  , ("😈", "!", [])
  , ("fun(😈) -> 😈", "fun<Type1 = !, Type2>(Type1) -> Type2", [])
  , ("😈 X", "X", [])
  , ("fun(😈 X) -> 😈 X", "fun(X) -> X", [])
  , ("fun<T, U = fun<V>(V) -> T, T: fun<V>(V) -> V>(U) -> T", "fun<T, U = fun<V>(V) -> T, T: fun<V>(V) -> V>(U) -> T", [])
  , ("fun(fun<V>(V) -> !) -> fun<V>(V) -> V", "fun<Type1 = fun<V, Type1>(V) -> Type1, Type2: fun<V>(V) -> V>(Type1) -> Type2", [])
  , ("{p: Int}", "{p: Int}", [])
  , ("{id: fun<T>(T) -> T}", "<Type1: fun<T>(T) -> T> {id: Type1}", [])
  , ("fun({id: fun<T>(T) -> T}) -> {id: fun<T>(T) -> T}", "fun<Type1: fun<T>(T) -> T, Type2: fun<T>(T) -> T>({id: Type1}) -> {id: Type2}", [])
  , ("{}", "{}", [])
  , ("{| X}", "{ | X}", [])
  , ("{a: Int, b: Bool}", "{a: Int, b: Bool}", [])
  , ("{b: Bool, a: Int}", "{b: Bool, a: Int}", [])
  , ("{a: Int | {a: Bool}}", "{a: Int, a: Bool}", [])
  , ("{a: Bool | {a: Int}}", "{a: Bool, a: Int}", [])
  , ("{a: Int, a: Bool}", "{a: Int, a: Bool}", [])
  , ("{b: void, a: Int, a: Bool}", "{b: void, a: Int, a: Bool}", [])
  , ("{a: Int, b: void, a: Bool}", "{a: Int, b: void, a: Bool}", [])
  , ("{a: Int, a: Bool, b: void}", "{a: Int, a: Bool, b: void}", [])
  , ("{a: nope1, b: nope2}", "<Type1, Type2> {a: Type1, b: Type2}", ["(0:4-0:9) Can not find `nope1`.", "(0:14-0:19) Can not find `nope2`."])
  , ("fun({}) -> {}", "fun({}) -> {}", [])
  , ("fun({}) -> Int", "fun({}) -> Int", [])
  , ("fun(Int) -> {}", "fun(Int) -> {}", [])
  , ("{a: Int}", "{a: Int}", [])
  , ("{a: Int | {}}", "{a: Int}", [])
  , ("{a: Int, b: Int}", "{a: Int, b: Int}", [])
  , ("{b: Int, a: Int}", "{b: Int, a: Int}", [])
  , ("{a: Int | T}", "<Type1> {a: Int | Type1}", ["(0:10-0:11) Can not find `T`."])
  , ("{a: Int | X}", "{a: Int | X}", [])
  , ("{a: Int | Int}", "{a: Int | Int}", [])
  , ("{a: {}}", "{a: {}}", [])
  , ("<T> {a: Int | T}", "<T> {a: Int | T}", [])
  , ("<T = Int> {a: Int | T}", "<T = Int> {a: Int | T}", [])
  , ("<T: Int> {a: Int | T}", "<T: Int> {a: Int | T}", [])
  , ("fun<T>(T) -> Int", "fun<T>(T) -> Int", [])
  , ("fun<T>(Int) -> T", "fun<T>(Int) -> T", [])
  , ("<T> {a: T | T}", "<T> {a: T | T}", [])
  , ("fun<T>({a: Int | T}) -> T", "fun<T>({a: Int | T}) -> T", [])
  , ("fun<T>(T) -> {a: Int | T}", "fun<T>(T) -> {a: Int | T}", [])
  , ("fun<T = Int>({a: Int | T}) -> T", "fun<T = Int>({a: Int | T}) -> T", [])
  , ("fun<T: Int>(T) -> {a: Int | T}", "fun<T: Int>(T) -> {a: Int | T}", [])
  , ("fun<T = <X> {a: X, b: !}>(T) -> T", "fun<T = <X, Type1> {a: X, b: Type1}>(T) -> T", [])
  ]

initialContext :: HashSet Identifier
initialContext = HashSet.fromList [unsafeIdentifier "X", unsafeIdentifier "Y", unsafeIdentifier "Z"]

spec :: Spec
spec = do
  flip traverse_ testData $ \(input, expectedOutput, expectedDiagnostics) ->
    it (Text.unpack input) $ do
      let (type1, _) = runDiagnosticWriter (parseType (tokenize input))
      let (type2, ds) = runDiagnosticWriter (checkPolytype initialContext (convertRecoverType type1))
      let actualOutput = Text.Lazy.toStrict (Text.Builder.toLazyText (printCompactType (printPolytypeWithoutInlining type2)))
      let actualDiagnostics = map (Text.Lazy.toStrict . Text.Builder.toLazyText . diagnosticMessageCompact) (toList ds)
      let (type3, _) = runDiagnosticWriter (parseType (tokenize actualOutput))
      let (type4, _) = runDiagnosticWriter (checkPolytype initialContext (convertRecoverType type3))
      let actualOutput2 = Text.Lazy.toStrict (Text.Builder.toLazyText (printCompactType (printPolytypeWithoutInlining type4)))
      actualOutput `shouldBe` expectedOutput
      actualDiagnostics `shouldBe` expectedDiagnostics
      actualOutput `shouldBe` actualOutput2
