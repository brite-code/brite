{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.CheckSpec (spec) where

import Brite.Diagnostic
import Brite.Semantics.AST (convertRecoverType)
import Brite.Semantics.Check (checkPolytype)
import Brite.Semantics.TypePrinter (printPolytypeWithoutInlining)
import Brite.Syntax.Parser (parseType)
import Brite.Syntax.Printer (printCompactType)
import Brite.Syntax.Token (Identifier, unsafeIdentifier, tokenize)
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
  , ("Nope", "!", ["We could not find type `Nope`."])
  , ("fun(Bool) -> Int", "fun(Bool) -> Int", [])
  , ("fun(X) -> Nope", "fun<Type1>(X) -> Type1", ["We could not find type `Nope`."])
  , ("fun(Nope) -> Nope", "fun<Type1 = !, Type2>(Type1) -> Type2", ["We could not find type `Nope`.", "We could not find type `Nope`."])
  , ("fun(Nope1) -> Nope2", "fun<Type1 = !, Type2>(Type1) -> Type2", ["We could not find type `Nope1`.", "We could not find type `Nope2`."])
  , ("fun(X) -> Y", "fun(X) -> Y", [])
  , ("fun(X) -> fun(Y) -> Z", "fun(X) -> fun(Y) -> Z", [])
  , ("(X)", "X", [])
  , ("(!)", "!", [])
  , ("(Nope)", "!", ["We could not find type `Nope`."])
  , ("fun(X) -> (Nope)", "fun<Type1>(X) -> Type1", ["We could not find type `Nope`."])
  , ("(fun(X) -> Y)", "fun(X) -> Y", [])
  , ("(fun(X) -> fun(Y) -> Z)", "fun(X) -> fun(Y) -> Z", [])
  , ("fun(X) -> (fun(Y) -> Z)", "fun(X) -> fun(Y) -> Z", [])
  , ("fun<T>(T) -> T", "fun<T>(T) -> T", [])
  , ("fun<T, U>(T) -> T", "fun<T, U>(T) -> T", [])
  , ("fun<T, U: !>(T) -> T", "fun<T, U>(T) -> T", [])
  , ("fun<T, U = !>(T) -> T", "fun<T, U = !>(T) -> T", [])
  , ("fun<T, U: X>(T) -> T", "fun<T, U: X>(T) -> T", [])
  , ("fun<T, U = X>(T) -> T", "fun<T, U = X>(T) -> T", [])
  , ("fun<T, U: Nope>(T) -> T", "fun<T, U>(T) -> T", ["We could not find type `Nope`."])
  , ("fun<T, U = Nope>(T) -> T", "fun<T, U = !>(T) -> T", ["We could not find type `Nope`."])
  , ("fun<U, T>(T) -> T", "fun<U, T>(T) -> T", [])
  , ("fun<U: !, T>(T) -> T", "fun<U, T>(T) -> T", [])
  , ("fun<U = !, T>(T) -> T", "fun<U = !, T>(T) -> T", [])
  , ("fun<U: X, T>(T) -> T", "fun<U: X, T>(T) -> T", [])
  , ("fun<U = X, T>(T) -> T", "fun<U = X, T>(T) -> T", [])
  , ("fun<U: Nope, T>(T) -> T", "fun<U, T>(T) -> T", ["We could not find type `Nope`."])
  , ("fun<U = Nope, T>(T) -> T", "fun<U = !, T>(T) -> T", ["We could not find type `Nope`."])
  , ("<T> T", "<T> T", [])
  , ("<T, U> T", "<T, U> T", [])
  , ("<T, U: !> T", "<T, U> T", [])
  , ("<T, U = !> T", "<T, U = !> T", [])
  , ("<T, U: X> T", "<T, U: X> T", [])
  , ("<T, U = X> T", "<T, U = X> T", [])
  , ("<T, U: Nope> T", "<T, U> T", ["We could not find type `Nope`."])
  , ("<T, U = Nope> T", "<T, U = !> T", ["We could not find type `Nope`."])
  , ("<U, T> T", "<U, T> T", [])
  , ("<U: !, T> T", "<U, T> T", [])
  , ("<U = !, T> T", "<U = !, T> T", [])
  , ("<U: X, T> T", "<U: X, T> T", [])
  , ("<U = X, T> T", "<U = X, T> T", [])
  , ("<U: Nope, T> T", "<U, T> T", ["We could not find type `Nope`."])
  , ("<U = Nope, T> T", "<U = !, T> T", ["We could not find type `Nope`."])
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
  , ("fun(T) -> fun<T: fun(T) -> !>(T) -> T", "fun<Type1 = !, Type2: fun<T: fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> T>(Type1) -> Type2", ["We could not find type `T`.", "We could not find type `T`."])
  , ("fun(T) -> fun<T: fun(T) -> fun<T: fun(T) -> !>(T) -> T>(T) -> T", "fun<Type1 = !, Type2: fun<T: fun<Type1 = !, Type2: fun<T: fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> T>(Type1) -> Type2>(T) -> T>(Type1) -> Type2", ["We could not find type `T`.", "We could not find type `T`.", "We could not find type `T`."])
  , ("fun(fun<T = fun(!) -> X>(T) -> T) -> X", "fun<Type1 = fun<T = fun<Type1 = !>(Type1) -> X>(T) -> T>(Type1) -> X", [])
  , ("fun(fun<T = fun(fun<T = fun(!) -> X>(T) -> T) -> X>(T) -> T) -> X", "fun<Type1 = fun<T = fun<Type1 = fun<T = fun<Type1 = !>(Type1) -> X>(T) -> T>(Type1) -> X>(T) -> T>(Type1) -> X", [])
  , ("fun(fun<T = fun(!) -> T>(T) -> T) -> T", "fun<Type1 = fun<T = fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> T, Type2>(Type1) -> Type2", ["We could not find type `T`.", "We could not find type `T`."])
  , ("fun(fun<T = fun(fun<T = fun(!) -> T>(T) -> T) -> T>(T) -> T) -> T", "fun<Type1 = fun<T = fun<Type1 = fun<T = fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> T, Type2>(Type1) -> Type2>(T) -> T, Type2>(Type1) -> Type2", ["We could not find type `T`.", "We could not find type `T`.", "We could not find type `T`."])
  , ("fun(fun<T: fun(!) -> !>(T) -> Int) -> Int", "fun<Type1 = fun<T: fun<Type1 = !, Type2>(Type1) -> Type2>(T) -> Int>(Type1) -> Int", [])
  , ("fun<Type1>(Type1) -> !", "fun<Type1, Type2>(Type1) -> Type2", [])
  , ("fun<Type2>(Type2) -> !", "fun<Type2, Type1>(Type2) -> Type1", [])
  , ("fun(Type1) -> !", "fun<Type1 = !, Type2>(Type1) -> Type2", ["We could not find type `Type1`."])
  , ("fun(Type1) -> <T> T", "fun<Type1 = !, Type2: <T> T>(Type1) -> Type2", ["We could not find type `Type1`."])
  , ("fun(Type1) -> fun<T>(T) -> T", "fun<Type1 = !, Type2: fun<T>(T) -> T>(Type1) -> Type2", ["We could not find type `Type1`."])
  , ("fun(!) -> Type1", "fun<Type1 = !, Type2>(Type1) -> Type2", ["We could not find type `Type1`."])
  , ("fun(<T> T) -> Type1", "fun<Type1 = <T> T, Type2>(Type1) -> Type2", ["We could not find type `Type1`."])
  , ("fun(fun<T>(T) -> T) -> Type1", "fun<Type1 = fun<T>(T) -> T, Type2>(Type1) -> Type2", ["We could not find type `Type1`."])
  , ("fun<T>(!) -> !", "fun<T, Type1 = !, Type2>(Type1) -> Type2", [])
  , ("fun<Type2>(!) -> !", "fun<Type2, Type1 = !, Type3>(Type1) -> Type3", [])
  , ("😈", "!", [])
  , ("fun(😈) -> 😈", "fun<Type1 = !, Type2>(Type1) -> Type2", [])
  , ("😈 X", "X", [])
  , ("fun(😈 X) -> 😈 X", "fun(X) -> X", [])
  , ("fun<T, U = fun<V>(V) -> T, T: fun<V>(V) -> V>(U) -> T", "fun<T, U = fun<V>(V) -> T, T: fun<V>(V) -> V>(U) -> T", [])
  , ("fun(fun<V>(V) -> !) -> fun<V>(V) -> V", "fun<Type1 = fun<V, Type1>(V) -> Type1, Type2: fun<V>(V) -> V>(Type1) -> Type2", [])
  ]

initialContext :: HashSet Identifier
initialContext = HashSet.fromList [unsafeIdentifier "X", unsafeIdentifier "Y", unsafeIdentifier "Z"]

spec :: Spec
spec = do
  describe "checkType" $ do
    flip traverse_ testData $ \(input, expectedOutput, expectedDiagnostics) ->
      it (Text.unpack input) $ do
        let (type1, _) = runDiagnosticWriter (parseType (tokenize input))
        let (type2, ds) = runDiagnosticWriter (checkPolytype initialContext (convertRecoverType type1))
        let actualOutput = Text.Lazy.toStrict (Text.Builder.toLazyText (printCompactType (printPolytypeWithoutInlining type2)))
        let actualDiagnostics = map (Text.Lazy.toStrict . Text.Builder.toLazyText . diagnosticMessageText) (toList ds)
        let (type3, _) = runDiagnosticWriter (parseType (tokenize actualOutput))
        let (type4, _) = runDiagnosticWriter (checkPolytype initialContext (convertRecoverType type3))
        let actualOutput2 = Text.Lazy.toStrict (Text.Builder.toLazyText (printCompactType (printPolytypeWithoutInlining type4)))
        actualOutput `shouldBe` expectedOutput
        actualDiagnostics `shouldBe` expectedDiagnostics
        actualOutput `shouldBe` actualOutput2
