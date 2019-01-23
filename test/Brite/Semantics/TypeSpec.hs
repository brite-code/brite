module Brite.Semantics.TypeSpec (spec) where

import Brite.Semantics.Lite
import Data.Foldable (traverse_)
import Test.Hspec
import Text.Parsec hiding (Parsec)

testData :: [(String, String)]
testData =
  [ ("nf(x)", "x")
  , ("nf(⊥)", "⊥")
  , ("nf(∀(a, b = number).a → b)", "∀a.a → number")
  , ("nf(∀(a, b ≥ number).a → b)", "∀a.a → number")
  , ("nf(∀(b = number, c = ∀a.a → b).c → c)", "∀(c = ∀a.a → number).c → c")
  , ("nf(∀(a, b = number, c = a → b).c → c)", "∀a.(a → number) → a → number")
  , ("nf(∀(a = number, b = ∀c.c → a, a).a → b)", "∀(b = ∀c.c → number, a).a → b")
  , ("nf(∀(a = number, b = a → a, a).a → b)", "∀a.a → number → number")
  , ("nf(∀(a, b = a, a).a → b)", "∀(a, a2).a2 → a")
  , ("nf(∀(a, b = a, a, a = a).a → b)", "∀(a, a2).a2 → a")
  , ("nf(∀(a, b = a, a, c = a, a).a → b → c)", "∀(a, a2, a3).a3 → a → a2")
  , ("nf(∀(a1, b = a1, a1).a1 → b)", "∀(a1, a2).a2 → a1")
  , ("nf(∀(a1, b = a1, a1, c = a1, a1).a1 → b → c)", "∀(a1, a2, a3).a3 → a1 → a2")
  , ("nf(∀(a2, b = a2, a2).a2 → b)", "∀(a2, a3).a3 → a2")
  , ("nf(∀(a2, b = a2, a2, c = a2, a2).a2 → b → c)", "∀(a2, a3, a4).a4 → a2 → a3")
  , ("nf(∀(a, b = a, a2, a).a → a2 → b)", "∀(a, a2, a3).a3 → a2 → a")
  , ("nf(∀(a, b = a, a, a2).a → a2 → b)", "∀(a, a2, a3).a2 → a3 → a")
  , ("nf(∀(a, a = ∀z.z → a, a = ∀z.z → a).a → a)", "∀(a, a = ∀z.z → a, a = ∀z.z → a).a → a")
  , ("nf(∀(a, a = a → a, a = a → a, a = a → a).a → a)", "∀a.(((a → a) → a → a) → (a → a) → a → a) → ((a → a) → a → a) → (a → a) → a → a")
  , ("nf(∀a.number)", "number")
  , ("nf(∀(a, b).number)", "number")
  , ("nf(∀a.a → number)", "∀a.a → number")
  , ("nf(∀(a, b).a → number)", "∀a.a → number")
  , ("nf(∀(b, a).a → number)", "∀a.a → number")
  , ("nf(∀(a, b, c).a → number)", "∀a.a → number")
  , ("nf(∀(b, a, c).a → number)", "∀a.a → number")
  , ("nf(∀(b, c, a).a → number)", "∀a.a → number")
  , ("nf(∀(a, a).a → number)", "∀a.a → number")
  , ("nf(∀(a, a, a).a → number)", "∀a.a → number")
  , ("nf(∀(a, a = ∀z.z → a).a → number)", "∀(a, a = ∀z.z → a).a → number")
  , ("nf(∀(a, a = ∀z.z → a, a).a → number)", "∀a.a → number")
  , ("nf(∀(a, a, a = ∀z.z → a).a → number)", "∀(a, a = ∀z.z → a).a → number")
  , ("nf(∀(a, a = a).a → number)", "∀a.a → number")
  , ("nf(∀(a, a = a, a).a → number)", "∀a2.a2 → number")
  , ("nf(∀(b, a = ∀z.z → b).a → number)", "∀(b, a = ∀z.z → b).a → number")
  , ("nf(∀(c, b, a = ∀z.z → b).a → number)", "∀(b, a = ∀z.z → b).a → number")
  , ("nf(∀(b, c, a = ∀z.z → b).a → number)", "∀(b, a = ∀z.z → b).a → number")
  , ("nf(∀(b, a = ∀z.z → b, c).a → number)", "∀(b, a = ∀z.z → b).a → number")
  , ("nf(∀(b, a = ∀z.z → z).a → number)", "∀(a = ∀z.z → z).a → number")
  , ("nf(∀(b, a = ∀z.z → b, b).a → number)", "∀(b, a = ∀z.z → b).a → number")
  , ("nf(∀(a = ∀z.z → x).a → number)", "∀(a = ∀z.z → x).a → number")
  , ("nf(∀(a = ∀z.z → x, x).a → number)", "∀(a = ∀z.z → x).a → number")
  , ("nf(∀(a = ∀z.x).a → a)", "x → x")
  , ("nf(∀(a = ∀(x, z).x → x).a → a)", "∀(a = ∀x.x → x).a → a")
  , ("nf(∀(a = ∀(z, x).x → x).a → a)", "∀(a = ∀x.x → x).a → a")
  , ("nf(∀a.a)", "⊥")
  , ("nf(∀(a = ∀b.b).a)", "⊥")
  , ("nf(∀(a ≥ ∀b.b).a)", "⊥")
  , ("nf(∀(a = ∀(b = ∀c.c).b).a)", "⊥")
  , ("nf(∀(a ≥ ∀(b = ∀c.c).b).a)", "⊥")
  , ("nf(∀(a = ∀(b ≥ ∀c.c).b).a)", "⊥")
  , ("nf(∀(a ≥ ∀(b ≥ ∀c.c).b).a)", "⊥")
  , ("nf(∀(a = ∀(b = ∀(c = ∀d.d).c).b).a)", "⊥")
  , ("nf(∀(a ≥ ∀(b ≥ ∀(c ≥ ∀d.d).c).b).a)", "⊥")
  , ("nf(∀(a = ∀(b ≥ ∀(c ≥ ∀d.d).c).b).a)", "⊥")
  , ("nf(∀(a ≥ ∀(b ≥ ∀(c = ∀d.d).c).b).a)", "⊥")
  , ("nf(∀(a ≥ ∀(b = ∀(c ≥ ∀d.d).c).b).a)", "⊥")
  , ("nf(∀(a = ∀(b = ∀(c ≥ ∀d.d).c).b).a)", "⊥")
  , ("nf(∀(a ≥ ∀(b = ∀(c = ∀d.d).c).b).a)", "⊥")
  , ("nf(∀(a = ∀(b ≥ ∀(c = ∀d.d).c).b).a)", "⊥")
  , ("nf(∀(a = ∀a.a → a).a)", "∀a.a → a")
  , ("nf(∀(a = ∀a.a → a, b).a)", "∀a.a → a")
  , ("nf(∀(b, a = ∀a.a → a).a)", "∀a.a → a")
  , ("nf(∀(a = ∀a.a → a, b, c = ∀z.z → b).a)", "∀a.a → a")
  , ("nf(∀(a = ∀a.a → a, b, c = ∀z.z → b).a → a)", "∀(a = ∀a.a → a).a → a")
  , ("nf(∀(a = ∀z.number).a → a)", "number → number")
  , ("nf(∀(b = ∀a.a → a, c = ∀z.b).c)", "∀a.a → a")
  , ("nf(∀z.x)", "x")
  , ("nf(∀(a, a = ∀a.a → a).a)", "∀a.a → a")
  , ("nf(∀(b, a = ∀a.a → b).a)", "∀(b, a).a → b")
  , ("nf(∀(a, b, x = ∀(c, d).a → b → c → d).x)", "∀(a, b, c, d).a → b → c → d")
  , ("nf(∀(a, b = a → a, a = ∀z.z → b).a → a)", "∀(a, a2 = ∀z.z → a → a).a2 → a2")
  , ("nf(∀(a, b = a → a, a = ∀z.z → b).a → a → b)", "∀(a, a2 = ∀z.z → a → a).a2 → a2 → a → a")
  , ("nf(∀(x, x = x).x)", "⊥")
  ]

testRunParser :: Parsec a -> String -> a
testRunParser p s =
  case runParser p () "test.lite" s of
    Right a -> a
    Left e -> error (show e)

spec :: Spec
spec = do
  describe "normal" $ do
    flip traverse_ testData $ \(input, output) ->
      it input $ do
        let x = testRunParser (callParsec "nf" typeParsec) input
        seq x (True `shouldBe` True)
