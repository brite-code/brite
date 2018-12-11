{-# LANGUAGE OverloadedStrings #-}

module Brite.SourceSpec (spec) where

import Brite.Source
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as T.Lazy
import Test.Hspec

testTokenize :: HasCallStack => T.Text -> T.Text -> Expectation
testTokenize input expected =
  let
    tokens = tokenize initialPosition input
    actual = T.Lazy.toStrict (debugTokens tokens)
  in
    actual `shouldBe` expected

spec = describe "tokenize" $ do
  it "parses glyphs" $
    testTokenize
      "{},.=();/"
      "0:0-0:1   | Glyph `{`\n\
      \0:1-0:2   | Glyph `}`\n\
      \0:2-0:3   | Glyph `,`\n\
      \0:3-0:4   | Glyph `.`\n\
      \0:4-0:5   | Glyph `=`\n\
      \0:5-0:6   | Glyph `(`\n\
      \0:6-0:7   | Glyph `)`\n\
      \0:7-0:8   | Glyph `;`\n\
      \0:8-0:9   | Glyph `/`\n\
      \0:9       | End\n"

  it "parses unexpected characters" $
    testTokenize
      "€"
      "0:0-0:1   | Unexpected `€`\n\
      \0:1       | End\n"

  it "parses unexpected characters made up of two UTF-16 code units" $
    testTokenize
      "😈"
      "0:0-0:2   | Unexpected `😈`\n\
      \0:2       | End\n"

  it "skips whitespace" $ do
    testTokenize
      ""
      "0:0       | End\n"
    testTokenize
      " "
      "0:1       | End\n"
    testTokenize
      "  "
      "0:2       | End\n"
    testTokenize
      "   "
      "0:3       | End\n"
    testTokenize
      "\t"
      "0:1       | End\n"
    testTokenize
      "\f"
      "0:1       | End\n"
    testTokenize
      "\v"
      "0:1       | End\n"
    testTokenize
      "\x00A0"
      "0:1       | End\n"
    testTokenize
      "\x2002"
      "0:1       | End\n"
    testTokenize
      "\x2003"
      "0:1       | End\n"
    testTokenize
      "\x2009"
      "0:1       | End\n"

  it "skips newlines" $ do
    testTokenize
      ""
      "0:0       | End\n"
    testTokenize
      "\n"
      "1:0       | End\n"
    testTokenize
      "\n\n"
      "2:0       | End\n"
    testTokenize
      " \n"
      "1:0       | End\n"
    testTokenize
      "\n "
      "1:1       | End\n"
    testTokenize
      "\r"
      "1:0       | End\n"
    testTokenize
      "\r\r"
      "2:0       | End\n"
    testTokenize
      " \r"
      "1:0       | End\n"
    testTokenize
      "\r "
      "1:1       | End\n"
    testTokenize
      "\r\n"
      "1:0       | End\n"
    testTokenize
      "\r\n\r\n"
      "2:0       | End\n"
    testTokenize
      " \r\n"
      "1:0       | End\n"
    testTokenize
      "\r\n "
      "1:1       | End\n"
    testTokenize
      "\n\r\r\n"
      "3:0       | End\n"
    testTokenize
      "\r\n\r\n"
      "2:0       | End\n"
    testTokenize
      "\r\r\n\n"
      "3:0       | End\n"
    testTokenize
      "\n\r\n\r"
      "3:0       | End\n"
    testTokenize
      "\n\n\r\r"
      "4:0       | End\n"
    testTokenize
      "\n\r"
      "2:0       | End\n"

  it "parses identifiers" $ do
    testTokenize
      "x"
      "0:0-0:1   | Identifier `x`\n\
      \0:1       | End\n"
    testTokenize
      "foo"
      "0:0-0:3   | Identifier `foo`\n\
      \0:3       | End\n"
    testTokenize
      "Bar"
      "0:0-0:3   | Identifier `Bar`\n\
      \0:3       | End\n"
    testTokenize
      "_42"
      "0:0-0:3   | Identifier `_42`\n\
      \0:3       | End\n"
    testTokenize
      "Θ"
      "0:0-0:1   | Identifier `Θ`\n\
      \0:1       | End\n"
    testTokenize
      "𐐷"
      "0:0-0:2   | Identifier `𐐷`\n\
      \0:2       | End\n"
    testTokenize
      "u𐐷"
      "0:0-0:3   | Identifier `u𐐷`\n\
      \0:3       | End\n"
    testTokenize
      "𐐷w"
      "0:0-0:3   | Identifier `𐐷w`\n\
      \0:3       | End\n"

  it "parses keywords" $ do
    testTokenize
      "_"
      "0:0-0:1   | Glyph `_`\n\
      \0:1       | End\n"
    testTokenize
      "true"
      "0:0-0:4   | Glyph `true`\n\
      \0:4       | End\n"
    testTokenize
      "false"
      "0:0-0:5   | Glyph `false`\n\
      \0:5       | End\n"
    testTokenize
      "let"
      "0:0-0:3   | Glyph `let`\n\
      \0:3       | End\n"
    testTokenize
      "if"
      "0:0-0:2   | Glyph `if`\n\
      \0:2       | End\n"
    testTokenize
      "else"
      "0:0-0:4   | Glyph `else`\n\
      \0:4       | End\n"
    testTokenize
      "do"
      "0:0-0:2   | Glyph `do`\n\
      \0:2       | End\n"

  it "parses single line comments" $ do
    testTokenize
      "/"
      "0:0-0:1   | Glyph `/`\n\
      \0:1       | End\n"
    testTokenize
      "//"
      "0:2       | End\n"
    testTokenize
      "// abc"
      "0:6       | End\n"
    testTokenize
      "// abc\n"
      "1:0       | End\n"
    testTokenize
      "// abc\nx"
      "1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    testTokenize
      "// abc\rx"
      "1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    testTokenize
      "// abc\r\nx"
      "1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    testTokenize
      "// 😈"
      "0:5       | End\n"

  it "parses multi-line comments" $ do
    testTokenize
      "/"
      "0:0-0:1   | Glyph `/`\n\
      \0:1       | End\n"
    testTokenize
      "/*"
      "0:2       | End\n"
    testTokenize
      "/* "
      "0:3       | End\n"
    testTokenize
      "/* *"
      "0:4       | End\n"
    testTokenize
      "/* */"
      "0:5       | End\n"
    testTokenize
      "/* **"
      "0:5       | End\n"
    testTokenize
      "/* **/"
      "0:6       | End\n"
    testTokenize
      "/* \n */"
      "1:3       | End\n"
    testTokenize
      "/* \r */"
      "1:3       | End\n"
    testTokenize
      "/* \r\n */"
      "1:3       | End\n"
    testTokenize
      "/* 😈 */"
      "0:8       | End\n"
    testTokenize
      "/* */ x"
      "0:6-0:7   | Identifier `x`\n\
      \0:7       | End\n"
    testTokenize
      "/* **/ x"
      "0:7-0:8   | Identifier `x`\n\
      \0:8       | End\n"
