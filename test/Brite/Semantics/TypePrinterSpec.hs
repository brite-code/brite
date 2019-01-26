{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypePrinterSpec (spec) where

import Brite.Diagnostics
import Brite.Semantics.AST (convertRecoverType)
import Brite.Semantics.Check (checkPolytype)
import Brite.Semantics.Type (Polarity(..))
import Brite.Semantics.TypePrinter (printPolytype)
import Brite.Syntax.Parser (parseType)
import Brite.Syntax.Printer (printCompactType)
import Brite.Syntax.Tokens (Identifier, unsafeIdentifier, tokenize)
import Data.Foldable (traverse_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import Test.Hspec

testData :: [(Text, Text)]
testData =
  [ ("X", "X")
  , ("Bool", "Bool")
  , ("Int", "Int")
  , ("!", "!")
  , ("T", "!")
  , ("fun(X) -> Y", "fun(X) -> Y")
  , ("fun(X) -> fun(Y) -> Z", "fun(X) -> fun(Y) -> Z")
  , ("<T> T", "!")
  , ("<T, U> T", "!")
  , ("fun<T>(T) -> T", "fun<T>(T) -> T")
  , ("fun<T>(T) -> fun(T) -> T", "fun<T>(T) -> fun(T) -> T")
  , ("fun<T>(T) -> Int", "fun<T>(T) -> Int")
  , ("fun<T>(Int) -> T", "fun(Int) -> !")
  , ("fun<T = !>(T) -> Int", "fun(!) -> Int")
  , ("fun<T = !>(Int) -> T", "fun<T = !>(Int) -> T")
  , ("fun(Int) -> !", "fun(Int) -> !")
  , ("fun(!) -> Int", "fun(!) -> Int")
  , ("fun(!) -> !", "fun(!) -> !")
  , ("fun<A, B>(A) -> fun(A) -> fun(B) -> B", "fun<A, B>(A) -> fun(A) -> fun(B) -> B")
  , ("fun<A, B>(A) -> B", "fun<A>(A) -> !")
  , ("fun<T = !, U: fun<V>(T) -> V, T>(U) -> T", "fun<U: fun(!) -> !>(U) -> !")
  , ("fun<T, U: fun<V>(V) -> T>(U) -> U", "fun<U: fun<V>(V) -> !>(U) -> U")
  , ("fun<T = !, U: fun<V>(T) -> V>(U) -> U", "fun<U: fun(!) -> !>(U) -> U")
  , ("fun<T, U: fun<V>(V) -> fun(T) -> T, T>(U) -> T", "fun<T, U: fun<V>(V) -> fun(T) -> T>(U) -> !")
  , ("fun<T, U: fun<V>(V) -> T, T = fun<V>(V) -> V>(U) -> T", "fun<U: fun<V>(V) -> !, T = fun<V>(V) -> V>(U) -> T")
  , ("fun<T, U = fun<V>(V) -> T, T: fun<V>(V) -> V>(U) -> T", "fun(fun<V>(V) -> !) -> fun<V>(V) -> V")
  , ("fun<T, U: fun<V>(V) -> T, T: fun<V>(V) -> V>(U) -> T", "fun<U: fun<V>(V) -> !>(U) -> fun<V>(V) -> V")
  , ("fun<T, U = fun<V>(V) -> T, T = fun<V>(V) -> V>(U) -> T", "fun<T = fun<V>(V) -> V>(fun<V>(V) -> !) -> T")
  , ("fun<T, U: fun<V>(V) -> T>(Int) -> U", "fun(Int) -> fun<V>(V) -> !")
  , ("fun<T, U: fun<V>(V) -> T>(T) -> U", "fun<T>(T) -> fun<V>(V) -> T")
  , ("fun<T, T: fun<V>(V) -> T>(Int) -> T", "fun(Int) -> fun<V>(V) -> !")
  , ("fun<T, T: fun<V>(V) -> T>(T) -> T", "fun<T: fun<V>(V) -> !>(T) -> T")
  , ("fun<T, U: fun<V>(V) -> fun(T) -> T, T>(T) -> U", "fun<T, T2>(T2) -> fun<V>(V) -> fun(T) -> T")
  , ("fun<T, U: fun<V>(V) -> fun(T) -> T, T: fun<V>(V) -> V>(T) -> U", "fun<T, T2: fun<V>(V) -> V>(T2) -> fun<V>(V) -> fun(T) -> T")
  , ("fun<T, V: fun<U: fun<V>(V) -> fun(T) -> T, T>(T) -> U>(V) -> V", "fun<T, V: fun<T2>(T2) -> fun<V>(V) -> fun(T) -> T>(V) -> V")
  , ("fun<T2, T, U: fun<V>(V) -> fun(T) -> T, T>(T2) -> fun(T) -> U", "fun<T2, T, T3>(T2) -> fun(T3) -> fun<V>(V) -> fun(T) -> T")
  , ("fun<T2, T, V: fun<U: fun<V>(V) -> fun(T) -> T, T>(T2) -> fun(T) -> U>(V) -> V", "fun<T2, T, V: fun<T3>(T2) -> fun(T3) -> fun<V>(V) -> fun(T) -> T>(V) -> V")
  , ("fun<A = !, B = fun<V>(V) -> A, A, A2>(A) -> fun(A2) -> B", "fun<A = !, B = fun<V>(V) -> A, A, A2>(A) -> fun(A2) -> B")
  , ("fun<A = !, B = fun<V>(V) -> A, A2, A>(A) -> fun(A2) -> B", "fun<A = !, B = fun<V>(V) -> A, A2, A>(A) -> fun(A2) -> B")
  , ("fun<T = !, U: fun<V>(V) -> T, T, T2>(T) -> fun(T2) -> U", "fun<T = !, T2, T3>(T2) -> fun(T3) -> fun<V>(V) -> T")
  , ("fun<T = !, U: fun<V>(V) -> T, T2, T>(T) -> fun(T2) -> U", "fun<T = !, T2, T3>(T3) -> fun(T2) -> fun<V>(V) -> T")
  ]

initialContext :: HashSet Identifier
initialContext = HashSet.fromList [unsafeIdentifier "X", unsafeIdentifier "Y", unsafeIdentifier "Z"]

spec :: Spec
spec = do
  flip traverse_ testData $ \(input, expectedOutput) ->
    it (Text.unpack input) $ do
      let (type1, ds1) = runDiagnosticWriter (parseType (tokenize input))
      mapM_ (error . Text.Lazy.unpack . Text.Builder.toLazyText . debugDiagnostic) ds1
      let (type2, _) = runDiagnosticWriter (checkPolytype Positive initialContext (convertRecoverType type1))
      let actualOutput = Text.Lazy.toStrict (Text.Builder.toLazyText (printCompactType (printPolytype Positive type2)))
      let (type3, _) = runDiagnosticWriter (parseType (tokenize actualOutput))
      let (type4, _) = runDiagnosticWriter (checkPolytype Positive initialContext (convertRecoverType type3))
      let actualOutput2 = Text.Lazy.toStrict (Text.Builder.toLazyText (printCompactType (printPolytype Positive type4)))
      actualOutput `shouldBe` expectedOutput
      actualOutput2 `shouldBe` actualOutput
