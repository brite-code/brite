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

    specify "a fresh unknown is the subtype of anything" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            a <- isRight <$> (unknown ctx >>= \k -> subtype ctx k top)
            b <- isRight <$> (unknown ctx >>= \k -> subtype ctx k value)
            c <- isRight <$> (unknown ctx >>= \k -> subtype ctx k numberValue)
            d <- isRight <$> (unknown ctx >>= \k -> subtype ctx k objectValue)
            e <- isRight <$> (unknown ctx >>= \k -> subtype ctx k bottom)
            return (a && b && c && d && e)

      result `shouldBe` True

    specify "a fresh unknown is the supertype of anything" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            a <- isRight <$> (unknown ctx >>= \k -> subtype ctx top k)
            b <- isRight <$> (unknown ctx >>= \k -> subtype ctx value k)
            c <- isRight <$> (unknown ctx >>= \k -> subtype ctx numberValue k)
            d <- isRight <$> (unknown ctx >>= \k -> subtype ctx objectValue k)
            e <- isRight <$> (unknown ctx >>= \k -> subtype ctx bottom k)
            return (a && b && c && d && e)

      result `shouldBe` True

    specify "an unknown may be the subtype of many different types" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k numberValue
            b <- isRight <$> subtype ctx k objectValue
            return (a && b)

      result `shouldBe` True

    specify "an unknown may be the supertype of many different types" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx numberValue k
            b <- isRight <$> subtype ctx objectValue k
            return (a && b)

      result `shouldBe` True

    specify "an unknown may be the subtype of top and bottom" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k bottom
            b <- isRight <$> subtype ctx k top
            return (a && b)

      result `shouldBe` True

    specify "an unknown may be the supertype of top and bottom" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx top k
            b <- isRight <$> subtype ctx bottom k
            return (a && b)

      result `shouldBe` True

    specify "an unknown allows compatible subtypes to flow through it" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k value
            b <- isRight <$> subtype ctx numberValue k
            c <- isRight <$> subtype ctx objectValue k
            return (a && b && c)

      result `shouldBe` True

    specify "an unknown allows compatible supertypes to flow through it" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx numberValue k
            b <- isRight <$> subtype ctx k value
            c <- isRight <$> subtype ctx objectValue k
            d <- isRight <$> subtype ctx k value
            return (a && b && c && d)

      result `shouldBe` True

    specify "an unknown does not allow incompatible subtypes to flow through it" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k objectValue
            b <- isRight <$> subtype ctx value k
            c <- isRight <$> subtype ctx numberValue k
            d <- isRight <$> subtype ctx objectValue k
            return (a && not b && not c && d)

      result `shouldBe` True

    specify "an unknown does not allow incompatible supertypes to flow through it" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx value k
            b <- isRight <$> subtype ctx k numberValue
            c <- isRight <$> subtype ctx k objectValue
            d <- isRight <$> subtype ctx k value
            return (a && not b && not c && d)

      result `shouldBe` True

    specify "an unknown will not allow many subtypes after an incompatible supertype" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx numberValue k
            b <- isRight <$> subtype ctx k numberValue
            c <- isRight <$> subtype ctx objectValue k
            d <- isRight <$> subtype ctx objectValue k
            return (a && b && not c && not d)

      result `shouldBe` True

    specify "an unknown will not allow many supertypes after an incompatible subtype" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            b <- isRight <$> subtype ctx k numberValue
            a <- isRight <$> subtype ctx numberValue k
            c <- isRight <$> subtype ctx k objectValue
            d <- isRight <$> subtype ctx k objectValue
            return (a && b && not c && not d)

      result `shouldBe` True

    specify "an unknown will not allow a subtype after many incompatible supertypes" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k numberValue
            b <- isRight <$> subtype ctx k objectValue
            c <- isRight <$> subtype ctx numberValue k
            d <- isRight <$> subtype ctx objectValue k
            return (a && b && not c && not d)

      result `shouldBe` True

    specify "an unknown will not allow a supertype after many incompatible subtypes" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx numberValue k
            b <- isRight <$> subtype ctx objectValue k
            c <- isRight <$> subtype ctx k numberValue
            d <- isRight <$> subtype ctx k objectValue
            return (a && b && not c && not d)

      result `shouldBe` True

    specify "an unknown is a subtype of itself" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k k
            b <- isRight <$> subtype ctx k k
            return (a && b)

      result `shouldBe` True

    specify "an unknown is a subtype of itself before adding bounds" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k k
            b <- isRight <$> subtype ctx k k
            c <- isRight <$> subtype ctx k value
            d <- isRight <$> subtype ctx numberValue k
            return (a && b && c && d)

      result `shouldBe` True

    specify "an unknown is a subtype of itself after adding bounds" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k value
            b <- isRight <$> subtype ctx numberValue k
            c <- isRight <$> subtype ctx k k
            d <- isRight <$> subtype ctx k k
            return (a && b && c && d)

      result `shouldBe` True

    specify "an unknown specific supertype will override a more generic supertype" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            a <- isRight <$> subtype ctx k value
            b <- isRight <$> subtype ctx k objectValue
            c <- isRight <$> subtype ctx value k
            d <- isRight <$> subtype ctx numberValue k
            e <- isRight <$> subtype ctx objectValue k
            return (a && b && not c && not d && e)

      result `shouldBe` True

    specify "an unknown specific supertype will not be overriden by a more generic supertype" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k <- unknown ctx
            b <- isRight <$> subtype ctx k objectValue
            a <- isRight <$> subtype ctx k value
            c <- isRight <$> subtype ctx value k
            d <- isRight <$> subtype ctx numberValue k
            e <- isRight <$> subtype ctx objectValue k
            return (a && b && not c && not d && e)

      result `shouldBe` True

    specify "two unknowns with number bounds may flow into each other" $ do
      fst $ runCheck $ do
        ctx <- newContext
        k1 <- unknown ctx
        k2 <- unknown ctx
        a <- isRight <$> subtype ctx k1 numberValue
        b <- isRight <$> subtype ctx numberValue k2
        c <- isRight <$> subtype ctx k2 k1
        d <- isRight <$> subtype ctx k2 objectValue
        e <- isRight <$> subtype ctx objectValue k2
        f <- isRight <$> subtype ctx k1 objectValue
        g <- isRight <$> subtype ctx objectValue k1
        h <- isRight <$> subtype ctx k2 value
        i <- isRight <$> subtype ctx value k2
        j <- isRight <$> subtype ctx k1 value
        k <- isRight <$> subtype ctx value k1
        return $ do
          a `shouldBe` True
          b `shouldBe` True
          c `shouldBe` True
          d `shouldBe` False
          e `shouldBe` False
          f `shouldBe` False
          g `shouldBe` False
          h `shouldBe` True
          i `shouldBe` False
          j `shouldBe` True
          k `shouldBe` False

    specify "two unknowns with number bounds in opposite directions flow into each other" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 numberValue
            b <- isRight <$> subtype ctx numberValue k2
            c <- isRight <$> subtype ctx k1 k2
            d <- isRight <$> subtype ctx k2 objectValue
            e <- isRight <$> subtype ctx objectValue k2
            f <- isRight <$> subtype ctx k1 objectValue
            g <- isRight <$> subtype ctx objectValue k1
            h <- isRight <$> subtype ctx k2 value
            i <- isRight <$> subtype ctx value k2
            j <- isRight <$> subtype ctx k1 value
            k <- isRight <$> subtype ctx value k1
            return (a && b && c && not d && not e && not f && not g && h && not i && j && not k)

      result `shouldBe` True

    specify "two unknowns with incompatible bounds may not flow into each other" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 numberValue
            b <- isRight <$> subtype ctx objectValue k2
            c <- isRight <$> subtype ctx k2 k1
            return (a && b && not c)

      result `shouldBe` True

    specify "two unknowns with incompatible bounds on opposite sides may not flow into each other" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 numberValue
            b <- isRight <$> subtype ctx objectValue k2
            c <- isRight <$> subtype ctx k1 k2
            return (a && b && not c)

      result `shouldBe` True

    specify "top may not flow into bottom with unknowns" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 bottom
            b <- isRight <$> subtype ctx top k2
            c <- isRight <$> subtype ctx k1 k2
            return (a && b && not c)

      result `shouldBe` True

    specify "two unknowns may link to each other" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            return a

      result `shouldBe` True

    specify "two unknowns may link to each other and then take a subtype on the first type" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx numberValue k1
            c <- isRight <$> subtype ctx k2 objectValue
            d <- isRight <$> subtype ctx k1 objectValue
            return (a && b && not c && not d)

      result `shouldBe` True

    specify "two unknowns may link to each other and then take a subtype on the second type" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx numberValue k2
            c <- isRight <$> subtype ctx k2 objectValue
            d <- isRight <$> subtype ctx k1 objectValue
            e <- isRight <$> subtype ctx objectValue k1
            return (a && b && not c && not d && e)

      result `shouldBe` True

    specify "two unknowns may link to each other and then take a supertype on the second type" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx k2 numberValue
            c <- isRight <$> subtype ctx objectValue k1
            d <- isRight <$> subtype ctx objectValue k2
            return (a && b && not c && not d)

      result `shouldBe` True

    specify "two unknowns may link to each other and then take a supertype on the first type" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx k1 numberValue
            c <- isRight <$> subtype ctx objectValue k1
            d <- isRight <$> subtype ctx objectValue k2
            return (a && b && not c && not d)

      result `shouldBe` True

    specify "two unknowns may link to each other and then link a subtype on the first type" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            k3 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx numberValue k3
            c <- isRight <$> subtype ctx k3 k1
            d <- isRight <$> subtype ctx k2 objectValue
            e <- isRight <$> subtype ctx k1 objectValue
            f <- isRight <$> subtype ctx k3 objectValue
            return (a && b && c && not d && not e && not f)

      result `shouldBe` True

    specify "two unknowns may link to each other and then link a subtype on the second type" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            k3 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx numberValue k3
            c <- isRight <$> subtype ctx k3 k2
            d <- isRight <$> subtype ctx k2 objectValue
            e <- isRight <$> subtype ctx k1 objectValue
            f <- isRight <$> subtype ctx k3 objectValue
            return (a && b && c && not d && not e && not f)

      result `shouldBe` True

    specify "two unknowns may link to each other and then link a supertype on the second type" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            k3 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx k3 numberValue
            c <- isRight <$> subtype ctx k2 k3
            d <- isRight <$> subtype ctx objectValue k2
            e <- isRight <$> subtype ctx objectValue k1
            f <- isRight <$> subtype ctx objectValue k3
            return (a && b && c && not d && not e && not f)

      result `shouldBe` True

    specify "two unknowns may link to each other and then link a supertype on the first type" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            k3 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx k3 numberValue
            c <- isRight <$> subtype ctx k1 k3
            d <- isRight <$> subtype ctx objectValue k2
            e <- isRight <$> subtype ctx objectValue k1
            f <- isRight <$> subtype ctx objectValue k3
            return (a && b && c && not d && not e && not f)

      result `shouldBe` True

    specify "subtyping two kinds with each other repeatedly works" $ do
      let
        (result, _) =
          runCheck $ do
            ctx <- newContext
            k1 <- unknown ctx
            k2 <- unknown ctx
            a <- isRight <$> subtype ctx k1 k2
            b <- isRight <$> subtype ctx k1 k2
            c <- isRight <$> subtype ctx k2 k1
            d <- isRight <$> subtype ctx k1 k2
            e <- isRight <$> subtype ctx k2 k1
            return (a && b && c && d && e)

      result `shouldBe` True

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
