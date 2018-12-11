{-# LANGUAGE OverloadedStrings #-}

module Brite.ParserSpec (spec) where

import Brite.AST
import Brite.Diagnostics
import Brite.Parser
import Brite.Parser.Framework
import Brite.Source
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Test.Hspec

testParse :: HasCallStack => T.Text -> T.Text -> Expectation
testParse input expected =
  let
    tokens = tokenize initialPosition input
    ((statement, _), diagnostics) = runDiagnosticWriter (runParser bindingStatement tokens)
    actual = L.toStrict $ B.toLazyText $
      (if null diagnostics then "" else
        mconcat (map debugDiagnostic diagnostics) <> B.singleton '\n')
      <> debugStatement statement
      <> B.singleton '\n'
  in
    actual `shouldBe` expected

spec =
  it "parses a binding statement" $
    testParse
      "let x = y"
      "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
