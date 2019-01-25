{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypeSpec (spec) where

import Brite.Diagnostics
import Brite.Semantics.AST (convertRecoverType)
import Brite.Semantics.Check (checkPolytype)
import Brite.Semantics.Type (Polarity(..), normal)
import Brite.Semantics.TypePrinter (printPolytypeWithoutInlining)
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
  , ("!", "!")
  , ("fun<A, B = Int>(A) -> B", "fun<A>(A) -> Int")
  , ("fun<A, B: Int>(A) -> B", "fun<A>(A) -> Int")
  , ("fun<B = Int, C = fun<A>(A) -> B>(C) -> C", "fun<C = fun<A>(A) -> Int>(C) -> C")
  , ("fun<A, B = Int, C = fun(A) -> B>(C) -> C", "fun<A>(fun(A) -> Int) -> fun(A) -> Int")
  , ("fun<A = Int, B = fun<C>(C) -> A, A>(A) -> B", "fun<B = fun<C>(C) -> Int, A>(A) -> B")
  , ("fun<A = Int, B = fun(A) -> A, A>(A) -> B", "fun<A>(A) -> fun(Int) -> Int")
  , ("fun<A, B = fun<C>(A) -> C, A>(A) -> B", "fun<A, B = fun<C>(A) -> C, A>(A) -> B")
  , ("fun<A, B = A, A>(A) -> B", "fun<A, A2>(A2) -> A")
  , ("fun<A, B = A, A, A = A>(A) -> B", "fun<A, A2>(A2) -> A")
  , ("fun<A, B = A, A, C = A, A>(A) -> fun(B) -> C", "fun<A, A2, A3>(A3) -> fun(A) -> A2")
  , ("fun<A1, B = A1, A1>(A1) -> B", "fun<A1, A2>(A2) -> A1")
  , ("fun<A1, B = A1, A1, C = A1, A1>(A1) -> fun(B) -> C", "fun<A1, A2, A3>(A3) -> fun(A1) -> A2")
  , ("fun<A2, B = A2, A2>(A2) -> B", "fun<A2, A3>(A3) -> A2")
  , ("fun<A2, B = A2, A2, C = A2, A2>(A2) -> fun(B) -> C", "fun<A2, A3, A4>(A4) -> fun(A2) -> A3")
  , ("fun<A, B = A, A2, A>(A) -> fun(A2) -> B", "fun<A, A2, A3>(A3) -> fun(A2) -> A")
  , ("fun<A, B = A, A, A2>(A) -> fun(A2) -> B", "fun<A, A2, A3>(A2) -> fun(A3) -> A")
  , ("fun<A, A = fun<Z>(Z) -> A, A = fun<Z>(Z) -> A>(A) -> A", "fun<A, A = fun<Z>(Z) -> A, A = fun<Z>(Z) -> A>(A) -> A")
  , ("fun<A, A = fun(A) -> A, A = fun(A) -> A, A = fun(A) -> A>(A) -> A", "fun<A>(fun(fun(fun(A) -> A) -> fun(A) -> A) -> fun(fun(A) -> A) -> fun(A) -> A) -> fun(fun(fun(A) -> A) -> fun(A) -> A) -> fun(fun(A) -> A) -> fun(A) -> A")
  , ("<A> Int", "Int")
  , ("<A, B> Int", "Int")
  , ("fun<A>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<A, B>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<B, A>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<A, B, C>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<B, A, C>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<B, C, A>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<A, A>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<A, A, A>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<A, A = fun<Z>(Z) -> A>(A) -> Int", "fun<A, A = fun<Z>(Z) -> A>(A) -> Int")
  , ("fun<A, A = fun<Z>(Z) -> A, A>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<A, A, A = fun<Z>(Z) -> A>(A) -> Int", "fun<A, A = fun<Z>(Z) -> A>(A) -> Int")
  , ("fun<A, A = A>(A) -> Int", "fun<A>(A) -> Int")
  , ("fun<A, A = A, A>(A) -> Int", "fun<A2>(A2) -> Int")
  , ("fun<B, A = fun<Z>(Z) -> B>(A) -> Int", "fun<B, A = fun<Z>(Z) -> B>(A) -> Int")
  , ("fun<C, B, A = fun<Z>(Z) -> B>(A) -> Int", "fun<B, A = fun<Z>(Z) -> B>(A) -> Int")
  , ("fun<B, C, A = fun<Z>(Z) -> B>(A) -> Int", "fun<B, A = fun<Z>(Z) -> B>(A) -> Int")
  , ("fun<B, A = fun<Z>(Z) -> B, C>(A) -> Int", "fun<B, A = fun<Z>(Z) -> B>(A) -> Int")
  , ("fun<B, A = fun<Z>(Z) -> Z>(A) -> Int", "fun<A = fun<Z>(Z) -> Z>(A) -> Int")
  , ("fun<B, A = fun<Z>(Z) -> B, B>(A) -> Int", "fun<B, A = fun<Z>(Z) -> B>(A) -> Int")
  , ("fun<A = fun<Z>(Z) -> X>(A) -> Int", "fun<A = fun<Z>(Z) -> X>(A) -> Int")
  , ("fun<A = fun<Z>(Z) -> X, X>(A) -> Int", "fun<A = fun<Z>(Z) -> X>(A) -> Int")
  , ("fun<A = <Z> X>(A) -> A", "fun(X) -> X")
  , ("fun<A = fun<X, Z>(X) -> X>(A) -> A", "fun<A = fun<X>(X) -> X>(A) -> A")
  , ("fun<A = fun<Z, X>(X) -> X>(A) -> A", "fun<A = fun<X>(X) -> X>(A) -> A")
  , ("<A> A", "!")
  , ("<A = <B> B> A", "!")
  , ("<A: <B> B> A", "!")
  , ("<A = <B = <C> C> B> A", "!")
  , ("<A: <B = <C> C> B> A", "!")
  , ("<A = <B: <C> C> B> A", "!")
  , ("<A: <B: <C> C> B> A", "!")
  , ("<A = <B = <C = <D> D> C> B> A", "!")
  , ("<A: <B: <C: <D> D> C> B> A", "!")
  , ("<A = <B: <C: <D> D> C> B> A", "!")
  , ("<A: <B: <C = <D> D> C> B> A", "!")
  , ("<A: <B = <C: <D> D> C> B> A", "!")
  , ("<A = <B = <C: <D> D> C> B> A", "!")
  , ("<A: <B = <C = <D> D> C> B> A", "!")
  , ("<A = <B: <C = <D> D> C> B> A", "!")
  , ("<A = fun<A>(A) -> A> A", "fun<A>(A) -> A")
  , ("<A = fun<A>(A) -> A, B> A", "fun<A>(A) -> A")
  , ("<B, A = fun<A>(A) -> A> A", "fun<A>(A) -> A")
  , ("<A = fun<A>(A) -> A, B, C = fun<Z>(Z) -> B> A", "fun<A>(A) -> A")
  , ("fun<A = fun<A>(A) -> A, B, C = fun<Z>(Z) -> B>(A) -> A", "fun<A = fun<A>(A) -> A>(A) -> A")
  , ("fun<A = <Z> Int>(A) -> A", "fun(Int) -> Int")
  , ("<B = fun<A>(A) -> A, C = <Z> B> C", "fun<A>(A) -> A")
  , ("<Z> X", "X")
  , ("<A, A = fun<A>(A) -> A> A", "fun<A>(A) -> A")
  , ("<B, A = fun<A>(A) -> B> A", "fun<B, A>(A) -> B")
  , ("<A, B, X = fun<C, D>(A) -> fun(B) -> fun(C) -> D> X", "fun<A, B, C, D>(A) -> fun(B) -> fun(C) -> D")
  , ("fun<A, B = fun(A) -> A, A = fun<Z>(Z) -> B>(A) -> A", "fun<A, A2 = fun<Z>(Z) -> fun(A) -> A>(A2) -> A2")
  , ("fun<A, B = fun(A) -> A, A = fun<Z>(Z) -> B>(A) -> fun(A) -> B", "fun<A, A2 = fun<Z>(Z) -> fun(A) -> A>(A2) -> fun(A2) -> fun(A) -> A")
  , ("<X, X = X> X", "!")
  ]

initialContext :: HashSet Identifier
initialContext = HashSet.fromList [unsafeIdentifier "X", unsafeIdentifier "Y", unsafeIdentifier "Z"]

spec :: Spec
spec = do
  describe "normal" $ do
    flip traverse_ testData $ \(input, expectedOutput) ->
      it (Text.unpack input) $ do
        let (type1, ds1) = runDiagnosticWriter (parseType (tokenize input))
        mapM_ (error . Text.Lazy.unpack . Text.Builder.toLazyText . debugDiagnostic) ds1
        let (type2, _) = runDiagnosticWriter (normal <$> checkPolytype Positive initialContext (convertRecoverType type1))
        let actualOutput = Text.Lazy.toStrict (Text.Builder.toLazyText (printCompactType (printPolytypeWithoutInlining type2)))
        actualOutput `shouldBe` expectedOutput
