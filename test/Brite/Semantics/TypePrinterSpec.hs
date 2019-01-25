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
      actualOutput `shouldBe` expectedOutput
