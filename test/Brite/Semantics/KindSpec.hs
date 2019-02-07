module Brite.Semantics.KindSpec (spec) where

import Brite.Semantics.CheckMonad
import Brite.Semantics.Kind
import Data.Either
import Test.Hspec hiding (context)

testSubtype :: Kind -> Kind -> Bool
testSubtype kind1 kind2 = fst (runCheck (do
  context <- newContext
  isRight <$> subtype context kind1 kind2))

testLeastUpperBound :: HasCallStack => Kind -> Kind -> Kind -> Expectation
testLeastUpperBound kind1 kind2 kind3 = do
  kind3A `shouldBe` kind3
  kind3B `shouldBe` kind3
  testSubtype kind1 kind3 `shouldBe` True
  testSubtype kind2 kind3 `shouldBe` True
  where
    (kind3A, _) = runCheck $ do
      context <- newContext
      leastUpperBound context kind1 kind2

    (kind3B, _) = runCheck $ do
      context <- newContext
      leastUpperBound context kind2 kind1

testGreatestLowerBound :: HasCallStack => Kind -> Kind -> Kind -> Expectation
testGreatestLowerBound kind1 kind2 kind3 = do
  kind3A `shouldBe` kind3
  kind3B `shouldBe` kind3
  testSubtype kind3 kind1 `shouldBe` True
  testSubtype kind3 kind2 `shouldBe` True
  where
    (kind3A, _) = runCheck $ do
      context <- newContext
      greatestLowerBound context kind1 kind2

    (kind3B, _) = runCheck $ do
      context <- newContext
      greatestLowerBound context kind2 kind1

spec :: Spec
spec = do
  describe "subtype" $ do
    specify "bottom is the subtype of everything" $ do
      testSubtype bottom bottom `shouldBe` True
      testSubtype bottom top `shouldBe` True
      testSubtype bottom value `shouldBe` True
      testSubtype bottom numberValue `shouldBe` True
      testSubtype bottom objectValue `shouldBe` True

    specify "top is the supertype of everything" $ do
      testSubtype top top `shouldBe` True
      testSubtype bottom top `shouldBe` True
      testSubtype value top `shouldBe` True
      testSubtype numberValue top `shouldBe` True
      testSubtype objectValue top `shouldBe` True

    specify "bottom is the supertype of nothing" $ do
      testSubtype top bottom `shouldBe` False
      testSubtype value bottom `shouldBe` False
      testSubtype numberValue bottom `shouldBe` False
      testSubtype objectValue bottom `shouldBe` False

    specify "top is the subtype of nothing" $ do
      testSubtype top bottom `shouldBe` False
      testSubtype top value `shouldBe` True -- NOTE: Currently value serves as top.
      testSubtype top numberValue `shouldBe` False
      testSubtype top objectValue `shouldBe` False

    specify "object is a subtype of value" $ do
      testSubtype objectValue objectValue `shouldBe` True
      testSubtype objectValue value `shouldBe` True
      testSubtype value objectValue `shouldBe` False

    specify "number is a subtype of value" $ do
      testSubtype numberValue numberValue `shouldBe` True
      testSubtype numberValue value `shouldBe` True
      testSubtype value numberValue `shouldBe` False

    specify "number and object are not related by subtyping" $ do
      testSubtype numberValue objectValue `shouldBe` False
      testSubtype objectValue numberValue `shouldBe` False

  describe "leastUpperBound" $ do
    specify "bottom returns the other one" $ do
      testLeastUpperBound bottom bottom bottom
      testLeastUpperBound bottom top top
      testLeastUpperBound bottom numberValue numberValue
      testLeastUpperBound bottom objectValue objectValue

    specify "top returns itself" $ do
      testLeastUpperBound top top top
      testLeastUpperBound top bottom top
      testLeastUpperBound top numberValue top
      testLeastUpperBound top objectValue top

    specify "number and value returns value" $ do
      testLeastUpperBound value numberValue value
      testLeastUpperBound numberValue numberValue numberValue

    specify "object and value returns value" $ do
      testLeastUpperBound value objectValue value
      testLeastUpperBound objectValue objectValue objectValue

    specify "number and object and returns value" $ do
      testLeastUpperBound numberValue objectValue value

  describe "greatestLowerBound" $ do
    specify "bottom returns itself" $ do
      testGreatestLowerBound bottom bottom bottom
      testGreatestLowerBound bottom top bottom
      testGreatestLowerBound bottom numberValue bottom
      testGreatestLowerBound bottom objectValue bottom

    specify "top returns the other one" $ do
      testGreatestLowerBound top top top
      testGreatestLowerBound top bottom bottom
      testGreatestLowerBound top numberValue numberValue
      testGreatestLowerBound top objectValue objectValue

    specify "number and value returns number" $ do
      testGreatestLowerBound value numberValue numberValue
      testGreatestLowerBound numberValue numberValue numberValue

    specify "object and value returns object" $ do
      testGreatestLowerBound value objectValue objectValue
      testGreatestLowerBound objectValue objectValue objectValue

    specify "number and object and returns bottom" $ do
      testGreatestLowerBound numberValue objectValue bottom
