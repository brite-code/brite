{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.NamerSpec (spec) where

import Brite.Semantics.Namer
import Brite.Syntax.Identifier
import Data.Text (Text)
import Test.Hspec

testIsFreshTypeName :: Text -> Bool
testIsFreshTypeName = isFreshTypeName . unsafeIdentifier

spec :: Spec
spec =
  describe "isFreshTypeName" $ do
    it "Type" $ testIsFreshTypeName "Type" `shouldBe` True
    it "Type0" $ testIsFreshTypeName "Type0" `shouldBe` True
    it "Type1" $ testIsFreshTypeName "Type1" `shouldBe` True
    it "Type2" $ testIsFreshTypeName "Type2" `shouldBe` True
    it "Type42" $ testIsFreshTypeName "Type42" `shouldBe` True
    it "Typ42e" $ testIsFreshTypeName "Typ42e" `shouldBe` False
    it "Nope" $ testIsFreshTypeName "Nope" `shouldBe` False
    it "T" $ testIsFreshTypeName "T" `shouldBe` False
    it "Ty" $ testIsFreshTypeName "Ty" `shouldBe` False
    it "Typ" $ testIsFreshTypeName "Typ" `shouldBe` False
    it "TYPE" $ testIsFreshTypeName "TYPE" `shouldBe` False
    it "TYPE42" $ testIsFreshTypeName "TYPE42" `shouldBe` False
