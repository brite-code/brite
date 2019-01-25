{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.CheckSpec (spec) where

import Brite.Diagnostics
import Brite.Semantics.AST (convertRecoverType)
import Brite.Semantics.Check (checkType)
import Brite.Semantics.Type (Polarity(..))
import Brite.Semantics.TypePrinter (printPolytypeWithoutInlining)
import Brite.Syntax.Parser (parseType)
import Brite.Syntax.Printer (printCompactType)
import Brite.Syntax.Tokens (tokenize)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import Test.Hspec

testData :: [(Text, Text)]
testData =
  [ ("T", "T")
  , ("Bool", "Bool")
  , ("Int", "Int")
  , ("!", "!")
  , ("fun(T) -> U", "fun(T) -> U")
  , ("fun(T) -> fun(U) -> V", "fun(T) -> fun(U) -> V")
  , ("(T)", "T")
  , ("(!)", "!")
  , ("(fun(T) -> U)", "fun(T) -> U")
  , ("(fun(T) -> fun(U) -> V)", "fun(T) -> fun(U) -> V")
  , ("fun(T) -> (fun(U) -> V)", "fun(T) -> fun(U) -> V")
  , ("fun<T>(T) -> T", "fun<T>(T) -> T")
  , ("fun<T, U>(T) -> T", "fun<T, U>(T) -> T")
  , ("fun<T, U: !>(T) -> T", "fun<T, U>(T) -> T")
  , ("fun<T, U = !>(T) -> T", "fun<T, U = !>(T) -> T")
  , ("fun<T, U: V>(T) -> T", "fun<T, U: V>(T) -> T")
  , ("fun<T, U = V>(T) -> T", "fun<T, U = V>(T) -> T")
  , ("fun<U, T>(T) -> T", "fun<U, T>(T) -> T")
  , ("fun<U: !, T>(T) -> T", "fun<U, T>(T) -> T")
  , ("fun<U = !, T>(T) -> T", "fun<U = !, T>(T) -> T")
  , ("fun<U: V, T>(T) -> T", "fun<U: V, T>(T) -> T")
  , ("fun<U = V, T>(T) -> T", "fun<U = V, T>(T) -> T")
  , ("<T> T", "<T> T")
  , ("<T, U> T", "<T, U> T")
  , ("<T, U: !> T", "<T, U> T")
  , ("<T, U = !> T", "<T, U = !> T")
  , ("<T, U: V> T", "<T, U: V> T")
  , ("<T, U = V> T", "<T, U = V> T")
  , ("<U, T> T", "<U, T> T")
  , ("<U: !, T> T", "<U, T> T")
  , ("<U = !, T> T", "<U = !, T> T")
  , ("<U: V, T> T", "<U: V, T> T")
  , ("<U = V, T> T", "<U = V, T> T")
  , ("fun(T) -> !", "fun<Type1>(T) -> Type1")
  , ("fun(!) -> T", "fun<Type1 = !>(Type1) -> T")
  , ("fun(!) -> !", "fun<Type1 = !, Type2>(Type1) -> Type2")
  , ("fun(<T> T) -> T", "fun<Type1 = <T> T>(Type1) -> T")
  , ("fun(T) -> <T> T", "fun<Type1: <T> T>(T) -> Type1")
  , ("fun(<T> T) -> <T> T", "fun<Type1 = <T> T, Type2: <T> T>(Type1) -> Type2")
  , ("fun(fun<T>(T) -> T) -> T", "fun<Type1 = fun<T>(T) -> T>(Type1) -> T")
  , ("fun(T) -> fun<T>(T) -> T", "fun<Type1: fun<T>(T) -> T>(T) -> Type1")
  , ("fun(fun<T>(T) -> T) -> fun<T>(T) -> T", "fun<Type1 = fun<T>(T) -> T, Type2: fun<T>(T) -> T>(Type1) -> Type2")
  , ("fun(fun(!) -> !) -> !", "fun<Type1, Type2 = !, Type3>(fun(Type1) -> Type2) -> Type3")
  , ("fun(fun(fun(!) -> !) -> !) -> !", "fun<Type1 = !, Type2, Type3 = !, Type4>(fun(fun(Type1) -> Type2) -> Type3) -> Type4")
  , ("fun(fun(fun(fun(!) -> !) -> !) -> !) -> !", "fun<Type1, Type2 = !, Type3, Type4 = !, Type5>(fun(fun(fun(Type1) -> Type2) -> Type3) -> Type4) -> Type5")
  , ("fun(T) -> fun<T: fun(T) -> !>(T) -> T", "fun<Type1: fun<T: fun<Type1>(T) -> Type1>(T) -> T>(T) -> Type1")
  , ("fun(T) -> fun<T: fun(T) -> fun<T: fun(T) -> !>(T) -> T>(T) -> T", "fun<Type1: fun<T: fun<Type1: fun<T: fun<Type1>(T) -> Type1>(T) -> T>(T) -> Type1>(T) -> T>(T) -> Type1")
  , ("fun(fun<T = fun(!) -> T>(T) -> T) -> T", "fun<Type1 = fun<T = fun<Type1>(Type1) -> T>(T) -> T>(Type1) -> T")
  , ("fun(fun<T = fun(fun<T = fun(!) -> T>(T) -> T) -> T>(T) -> T) -> T", "fun<Type1 = fun<T = fun<Type1: fun<T = fun<Type1 = !>(Type1) -> T>(T) -> T>(Type1) -> T>(T) -> T>(Type1) -> T")
  , ("fun(fun<T: fun(!) -> !>(T) -> Int) -> Int", "fun<Type1 = fun<T: fun<Type1, Type2 = !>(Type1) -> Type2>(T) -> Int>(Type1) -> Int")
  , ("fun<Type1>(Type1) -> !", "fun<Type1, Type2>(Type1) -> Type2")
  , ("fun<Type2>(Type2) -> !", "fun<Type2, Type1>(Type2) -> Type1")
  -- TODO: This is not good.
  , ("fun(Type1) -> !", "")
  ]

spec :: Spec
spec = do
  describe "checkType" $ do
    flip traverse_ testData $ \(input, output) ->
      it (Text.unpack input) $ do
        let (type1, ds1) = runDiagnosticWriter (parseType (tokenize input))
        mapM_ (error . Text.Lazy.unpack . Text.Builder.toLazyText . debugDiagnostic) ds1
        let (type2, ds2) = runDiagnosticWriter (checkType Positive (convertRecoverType type1))
        mapM_ (error . Text.Lazy.unpack . Text.Builder.toLazyText . debugDiagnostic) ds2
        let expectedOutput = printCompactType (printPolytypeWithoutInlining type2)
        Text.Lazy.toStrict (Text.Builder.toLazyText expectedOutput) `shouldBe` output
