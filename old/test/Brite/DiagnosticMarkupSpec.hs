{-# LANGUAGE OverloadedStrings #-}

module Brite.DiagnosticMarkupSpec (spec) where

import Brite.DiagnosticMarkup
import Test.Hspec
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

testToANSIDoc :: Markup -> String
testToANSIDoc x = Doc.displayS (Doc.renderPretty 1 500 (toANSIDoc x)) ""

spec :: Spec
spec = do
  describe "toText" $ do
    it "wraps code in \"`\"" $ do
      toText (code "") `shouldBe` "``"
      toText (code "foo()") `shouldBe` "`foo()`"
      toText (plain "Hello " <> code "World" <> plain "!") `shouldBe` "Hello `World`!"

    it "escapes \"`\" in plain text" $ do
      toText (plain "`") `shouldBe` "\\`"
      toText (plain "``") `shouldBe` "\\`\\`"
      toText (plain "Hello `World`") `shouldBe` "Hello \\`World\\`"

    it "escapes \"`\" in code" $ do
      toText (code "`") `shouldBe` "`\\``"
      toText (code "``") `shouldBe` "`\\`\\``"
      toText (code "Hello `World`") `shouldBe` "`Hello \\`World\\``"

    it "escapes \"\\\" in plain text" $ do
      toText (plain "\\") `shouldBe` "\\\\"
      toText (plain "\\\\") `shouldBe` "\\\\\\\\"
      toText (plain "Hello \\World\\") `shouldBe` "Hello \\\\World\\\\"

    it "escapes \"\\\" in code" $ do
      toText (code "\\") `shouldBe` "`\\\\`"
      toText (code "\\\\") `shouldBe` "`\\\\\\\\`"
      toText (code "Hello \\World\\") `shouldBe` "`Hello \\\\World\\\\`"

  describe "toANSIDoc" $ do
    it "wraps code in bold ANSI styles" $ do
      testToANSIDoc (code "") `shouldBe` "\ESC[1m\ESC[0m"
      testToANSIDoc (code "foo()") `shouldBe` "\ESC[1mfoo()\ESC[0m"
      testToANSIDoc (plain "Hello " <> code "World" <> plain "!") `shouldBe` "Hello \ESC[1mWorld\ESC[0m!"

    it "renders \"`\" in plain text" $ do
      testToANSIDoc (plain "`") `shouldBe` "`"
      testToANSIDoc (plain "``") `shouldBe` "``"
      testToANSIDoc (plain "Hello `World`") `shouldBe` "Hello `World`"

    it "renders \"`\" in code" $ do
      testToANSIDoc (code "`") `shouldBe` "\ESC[1m`\ESC[0m"
      testToANSIDoc (code "``") `shouldBe` "\ESC[1m``\ESC[0m"
      testToANSIDoc (code "Hello `World`") `shouldBe` "\ESC[1mHello `World`\ESC[0m"

    it "renders \"\\\" in plain text" $ do
      testToANSIDoc (plain "\\") `shouldBe` "\\"
      testToANSIDoc (plain "\\\\") `shouldBe` "\\\\"
      testToANSIDoc (plain "Hello \\World\\") `shouldBe` "Hello \\World\\"

    it "renders \"\\\" in code" $ do
      testToANSIDoc (code "\\") `shouldBe` "\ESC[1m\\\ESC[0m"
      testToANSIDoc (code "\\\\") `shouldBe` "\ESC[1m\\\\\ESC[0m"
      testToANSIDoc (code "Hello \\World\\") `shouldBe` "\ESC[1mHello \\World\\\ESC[0m"
