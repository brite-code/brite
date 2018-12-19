{-# LANGUAGE OverloadedStrings #-}

module Brite.SourceSpec (spec) where

import Brite.Source
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Test.Hspec

runTest :: T.Text -> T.Text -> Spec
runTest input expected =
  it (T.unpack (escape input)) $
    let
      tokens = tokenize' input
      actual = L.toStrict (B.toLazyText (debugTokens tokens))
    in
      actual `shouldBe` expected

escape :: T.Text -> T.Text
escape = T.concatMap
  (\c ->
    case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\f' -> "\\f"
      '\v' -> "\\v"
      '\x00A0' -> "\\x00A0"
      '\x2002' -> "\\x2002"
      '\x2003' -> "\\x2003"
      '\x2009' -> "\\x2009"
      _ -> T.singleton c)

spec :: Spec
spec = mapM_ (uncurry runTest)
  [ ( "{},.=();/"
    , "0:0-0:1   | Glyph `{`\n\
      \0:1-0:2   | Glyph `}`\n\
      \0:2-0:3   | Glyph `,`\n\
      \0:3-0:4   | Glyph `.`\n\
      \0:4-0:5   | Glyph `=`\n\
      \0:5-0:6   | Glyph `(`\n\
      \0:6-0:7   | Glyph `)`\n\
      \0:7-0:8   | Glyph `;`\n\
      \0:8-0:9   | Glyph `/`\n\
      \0:9       | End\n"
    )
  , ( "€"
    , "0:0-0:1   | Unexpected `€`\n\
      \0:1       | End\n"
    )
  , ( "😈"
    , "0:0-0:2   | Unexpected `😈`\n\
      \0:2       | End\n"
    )
  , ( "//"
    , "0:2       | End\n"
    )
  , ( " //"
    , "0:3       | End\n"
    )
  , ( "  //"
    , "0:4       | End\n"
    )
  , ( "   //"
    , "0:5       | End\n"
    )
  , ( "\t"
    , "0:1       | End\n"
    )
  , ( "\f"
    , "0:1       | End\n"
    )
  , ( "\v"
    , "0:1       | End\n"
    )
  , ( "\x00A0"
    , "0:1       | End\n"
    )
  , ( "\x2002"
    , "0:1       | End\n"
    )
  , ( "\x2003"
    , "0:1       | End\n"
    )
  , ( "\x2009"
    , "0:1       | End\n"
    )
  , ( "\n"
    , "1:0       | End\n"
    )
  , ( "\n\n"
    , "2:0       | End\n"
    )
  , ( " \n"
    , "1:0       | End\n"
    )
  , ( "\n "
    , "1:1       | End\n"
    )
  , ( "\r"
    , "1:0       | End\n"
    )
  , ( "\r\r"
    , "2:0       | End\n"
    )
  , ( " \r"
    , "1:0       | End\n"
    )
  , ( "\r "
    , "1:1       | End\n"
    )
  , ( "\r\n"
    , "1:0       | End\n"
    )
  , ( "\r\n\r\n"
    , "2:0       | End\n"
    )
  , ( " \r\n"
    , "1:0       | End\n"
    )
  , ( "\r\n "
    , "1:1       | End\n"
    )
  , ( "\n\r\r\n"
    , "3:0       | End\n"
    )
  , ( "\r\n\r\n"
    , "2:0       | End\n"
    )
  , ( "\r\r\n\n"
    , "3:0       | End\n"
    )
  , ( "\n\r\n\r"
    , "3:0       | End\n"
    )
  , ( "\n\n\r\r"
    , "4:0       | End\n"
    )
  , ( "\n\r"
    , "2:0       | End\n"
    )
  , ( "x"
    , "0:0-0:1   | Identifier `x`\n\
      \0:1       | End\n"
    )
  , ( "foo"
    , "0:0-0:3   | Identifier `foo`\n\
      \0:3       | End\n"
    )
  , ( "Bar"
    , "0:0-0:3   | Identifier `Bar`\n\
      \0:3       | End\n"
    )
  , ( "_42"
    , "0:0-0:3   | Identifier `_42`\n\
      \0:3       | End\n"
    )
  , ( "Θ"
    , "0:0-0:1   | Identifier `Θ`\n\
      \0:1       | End\n"
    )
  , ( "𐐷"
    , "0:0-0:2   | Identifier `𐐷`\n\
      \0:2       | End\n"
    )
  , ( "u𐐷"
    , "0:0-0:3   | Identifier `u𐐷`\n\
      \0:3       | End\n"
    )
  , ( "𐐷w"
    , "0:0-0:3   | Identifier `𐐷w`\n\
      \0:3       | End\n"
    )
  , ( "_"
    , "0:0-0:1   | Glyph `_`\n\
      \0:1       | End\n"
    )
  , ( "true"
    , "0:0-0:4   | Glyph `true`\n\
      \0:4       | End\n"
    )
  , ( "false"
    , "0:0-0:5   | Glyph `false`\n\
      \0:5       | End\n"
    )
  , ( "let"
    , "0:0-0:3   | Glyph `let`\n\
      \0:3       | End\n"
    )
  , ( "if"
    , "0:0-0:2   | Glyph `if`\n\
      \0:2       | End\n"
    )
  , ( "else"
    , "0:0-0:4   | Glyph `else`\n\
      \0:4       | End\n"
    )
  , ( "do"
    , "0:0-0:2   | Glyph `do`\n\
      \0:2       | End\n"
    )
  , ( "/"
    , "0:0-0:1   | Glyph `/`\n\
      \0:1       | End\n"
    )
  , ( "//"
    , "0:2       | End\n"
    )
  , ( "// abc"
    , "0:6       | End\n"
    )
  , ( "// abc\n"
    , "1:0       | End\n"
    )
  , ( "// abc\nx"
    , "1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    )
  , ( "// abc\rx"
    , "1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    )
  , ( "// abc\r\nx"
    , "1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    )
  , ( "// 😈"
    , "0:5       | End\n"
    )
  , ( "/"
    , "0:0-0:1   | Glyph `/`\n\
      \0:1       | End\n"
    )
  , ( "/*"
    , "0:2       | End\n"
    )
  , ( "/* "
    , "0:3       | End\n"
    )
  , ( "/* /"
    , "0:4       | End\n"
    )
  , ( "/* *"
    , "0:4       | End\n"
    )
  , ( "/* */"
    , "0:5       | End\n"
    )
  , ( "/* **"
    , "0:5       | End\n"
    )
  , ( "/* **/"
    , "0:6       | End\n"
    )
  , ( "/* * */"
    , "0:7       | End\n"
    )
  , ( "/* / */"
    , "0:7       | End\n"
    )
  , ( "/* \n */"
    , "1:3       | End\n"
    )
  , ( "/* \n\n */"
    , "2:3       | End\n"
    )
  , ( "/* \r */"
    , "1:3       | End\n"
    )
  , ( "/* \r\r */"
    , "2:3       | End\n"
    )
  , ( "/* \r\n */"
    , "1:3       | End\n"
    )
  , ( "/* \r\n\r\n */"
    , "2:3       | End\n"
    )
  , ( "/* 😈 */"
    , "0:8       | End\n"
    )
  , ( "/* € */"
    , "0:7       | End\n"
    )
  , ( "/* 😈 😈 */"
    , "0:11      | End\n"
    )
  , ( "/* € € */"
    , "0:9       | End\n"
    )
  , ( "/* */ x"
    , "0:6-0:7   | Identifier `x`\n\
      \0:7       | End\n"
    )
  , ( "/* **/ x"
    , "0:7-0:8   | Identifier `x`\n\
      \0:8       | End\n"
    )
  ]
