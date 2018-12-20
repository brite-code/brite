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
      rebuiltInput = L.toStrict (B.toLazyText (rebuildSource tokens))
    in do
      actual `shouldBe` expected
      rebuiltInput `shouldBe` input

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
    , "+         | LineComment\n\
      \0:2       | End\n"
    )
  , ( " //"
    , "+         | Spaces 1\n\
      \+         | LineComment\n\
      \0:3       | End\n"
    )
  , ( "  //"
    , "+         | Spaces 2\n\
      \+         | LineComment\n\
      \0:4       | End\n"
    )
  , ( "   //"
    , "+         | Spaces 3\n\
      \+         | LineComment\n\
      \0:5       | End\n"
    )
  , ( "\t"
    , "+         | Tabs 1\n\
      \0:1       | End\n"
    )
  , ( "\t\t"
    , "+         | Tabs 2\n\
      \0:2       | End\n"
    )
  , ( "\t\t\t"
    , "+         | Tabs 3\n\
      \0:3       | End\n"
    )
  , ( "\f"
    , "+         | OtherWhitespace `\f`\n\
      \0:1       | End\n"
    )
  , ( "\v"
    , "+         | OtherWhitespace `\v`\n\
      \0:1       | End\n"
    )
  , ( "\x00A0"
    , "+         | OtherWhitespace `\x00A0`\n\
      \0:1       | End\n"
    )
  , ( "\x2002"
    , "+         | OtherWhitespace `\x2002`\n\
      \0:1       | End\n"
    )
  , ( "\x2003"
    , "+         | OtherWhitespace `\x2003`\n\
      \0:1       | End\n"
    )
  , ( "\x2009"
    , "+         | OtherWhitespace `\x2009`\n\
      \0:1       | End\n"
    )
  , ( "\n"
    , "+         | Newlines LF 1\n\
      \1:0       | End\n"
    )
  , ( "\n\n"
    , "+         | Newlines LF 2\n\
      \2:0       | End\n"
    )
  , ( " \n"
    , "+         | Spaces 1\n\
      \+         | Newlines LF 1\n\
      \1:0       | End\n"
    )
  , ( "\n "
    , "+         | Newlines LF 1\n\
      \+         | Spaces 1\n\
      \1:1       | End\n"
    )
  , ( "\r"
    , "+         | Newlines CR 1\n\
      \1:0       | End\n"
    )
  , ( "\r\r"
    , "+         | Newlines CR 2\n\
      \2:0       | End\n"
    )
  , ( " \r"
    , "+         | Spaces 1\n\
      \+         | Newlines CR 1\n\
      \1:0       | End\n"
    )
  , ( "\r "
    , "+         | Newlines CR 1\n\
      \+         | Spaces 1\n\
      \1:1       | End\n"
    )
  , ( "\r\n"
    , "+         | Newlines CRLF 1\n\
      \1:0       | End\n"
    )
  , ( "\r\n\r\n"
    , "+         | Newlines CRLF 2\n\
      \2:0       | End\n"
    )
  , ( " \r\n"
    , "+         | Spaces 1\n\
      \+         | Newlines CRLF 1\n\
      \1:0       | End\n"
    )
  , ( "\r\n "
    , "+         | Newlines CRLF 1\n\
      \+         | Spaces 1\n\
      \1:1       | End\n"
    )
  , ( "\n\r\r\n"
    , "+         | Newlines LF 1\n\
      \+         | Newlines CR 1\n\
      \+         | Newlines CRLF 1\n\
      \3:0       | End\n"
    )
  , ( "\r\n\r\n"
    , "+         | Newlines CRLF 2\n\
      \2:0       | End\n"
    )
  , ( "\r\r\n\n"
    , "+         | Newlines CR 1\n\
      \+         | Newlines CRLF 1\n\
      \+         | Newlines LF 1\n\
      \3:0       | End\n"
    )
  , ( "\n\r\n\r"
    , "+         | Newlines LF 1\n\
      \+         | Newlines CRLF 1\n\
      \+         | Newlines CR 1\n\
      \3:0       | End\n"
    )
  , ( "\n\n\r\r"
    , "+         | Newlines LF 2\n\
      \+         | Newlines CR 2\n\
      \4:0       | End\n"
    )
  , ( "\n\r"
    , "+         | Newlines LF 1\n\
      \+         | Newlines CR 1\n\
      \2:0       | End\n"
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
    , "+         | LineComment\n\
      \0:2       | End\n"
    )
  , ( "// abc"
    , "+         | LineComment\n\
      \0:6       | End\n"
    )
  , ( "// abc\n"
    , "+         | LineComment\n\
      \+         | Newlines LF 1\n\
      \1:0       | End\n"
    )
  , ( "// abc\nx"
    , "+         | LineComment\n\
      \+         | Newlines LF 1\n\
      \1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    )
  , ( "// abc\rx"
    , "+         | LineComment\n\
      \+         | Newlines CR 1\n\
      \1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    )
  , ( "// abc\r\nx"
    , "+         | LineComment\n\
      \+         | Newlines CRLF 1\n\
      \1:0-1:1   | Identifier `x`\n\
      \1:1       | End\n"
    )
  , ( "// 😈"
    , "+         | LineComment\n\
      \0:5       | End\n"
    )
  , ( "/"
    , "0:0-0:1   | Glyph `/`\n\
      \0:1       | End\n"
    )
  , ( "/*"
    , "+         | BlockComment\n\
      \0:2       | End\n"
    )
  , ( "/* "
    , "+         | BlockComment\n\
      \0:3       | End\n"
    )
  , ( "/* /"
    , "+         | BlockComment\n\
      \0:4       | End\n"
    )
  , ( "/* *"
    , "+         | BlockComment\n\
      \0:4       | End\n"
    )
  , ( "/* */"
    , "+         | BlockComment\n\
      \0:5       | End\n"
    )
  , ( "/* **"
    , "+         | BlockComment\n\
      \0:5       | End\n"
    )
  , ( "/* **/"
    , "+         | BlockComment\n\
      \0:6       | End\n"
    )
  , ( "/* * */"
    , "+         | BlockComment\n\
      \0:7       | End\n"
    )
  , ( "/* / */"
    , "+         | BlockComment\n\
      \0:7       | End\n"
    )
  , ( "/* \n */"
    , "+         | BlockComment\n\
      \1:3       | End\n"
    )
  , ( "/* \n\n */"
    , "+         | BlockComment\n\
      \2:3       | End\n"
    )
  , ( "/* \r */"
    , "+         | BlockComment\n\
      \1:3       | End\n"
    )
  , ( "/* \r\r */"
    , "+         | BlockComment\n\
      \2:3       | End\n"
    )
  , ( "/* \r\n */"
    , "+         | BlockComment\n\
      \1:3       | End\n"
    )
  , ( "/* \r\n\r\n */"
    , "+         | BlockComment\n\
      \2:3       | End\n"
    )
  , ( "/* 😈 */"
    , "+         | BlockComment\n\
      \0:8       | End\n"
    )
  , ( "/* € */"
    , "+         | BlockComment\n\
      \0:7       | End\n"
    )
  , ( "/* 😈 😈 */"
    , "+         | BlockComment\n\
      \0:11      | End\n"
    )
  , ( "/* € € */"
    , "+         | BlockComment\n\
      \0:9       | End\n"
    )
  , ( "/* */ x"
    , "+         | BlockComment\n\
      \+         | Spaces 1\n\
      \0:6-0:7   | Identifier `x`\n\
      \0:7       | End\n"
    )
  , ( "/* **/ x"
    , "+         | BlockComment\n\
      \+         | Spaces 1\n\
      \0:7-0:8   | Identifier `x`\n\
      \0:8       | End\n"
    )
  , ( "/*/ x"
    , "+         | BlockComment\n\
      \+         | Spaces 1\n\
      \0:4-0:5   | Identifier `x`\n\
      \0:5       | End\n"
    )
  , ( "/**/ x"
    , "+         | BlockComment\n\
      \+         | Spaces 1\n\
      \0:5-0:6   | Identifier `x`\n\
      \0:6       | End\n"
    )
  , ( "a   b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 3\n\
      \0:4-0:5   | Identifier `b`\n\
      \0:5       | End\n"
    )
  , ( "a \n b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines LF 1\n\
      \+         | Spaces 1\n\
      \1:1-1:2   | Identifier `b`\n\
      \1:2       | End\n"
    )
  , ( "a \r b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines CR 1\n\
      \+         | Spaces 1\n\
      \1:1-1:2   | Identifier `b`\n\
      \1:2       | End\n"
    )
  , ( "a \r\n b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines CRLF 1\n\
      \+         | Spaces 1\n\
      \1:1-1:2   | Identifier `b`\n\
      \1:2       | End\n"
    )
  , ( "a \n\n b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines LF 1\n\
      \+         | Newlines LF 1\n\
      \+         | Spaces 1\n\
      \2:1-2:2   | Identifier `b`\n\
      \2:2       | End\n"
    )
  , ( "a \n\n\n b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines LF 1\n\
      \+         | Newlines LF 2\n\
      \+         | Spaces 1\n\
      \3:1-3:2   | Identifier `b`\n\
      \3:2       | End\n"
    )
  , ( "a \r\r b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines CR 1\n\
      \+         | Newlines CR 1\n\
      \+         | Spaces 1\n\
      \2:1-2:2   | Identifier `b`\n\
      \2:2       | End\n"
    )
  , ( "a \r\r\r b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines CR 1\n\
      \+         | Newlines CR 2\n\
      \+         | Spaces 1\n\
      \3:1-3:2   | Identifier `b`\n\
      \3:2       | End\n"
    )
  , ( "a \r\n\r\n b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines CRLF 1\n\
      \+         | Newlines CRLF 1\n\
      \+         | Spaces 1\n\
      \2:1-2:2   | Identifier `b`\n\
      \2:2       | End\n"
    )
  , ( "a \r\n\r\n\r\n b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | Newlines CRLF 1\n\
      \+         | Newlines CRLF 2\n\
      \+         | Spaces 1\n\
      \3:1-3:2   | Identifier `b`\n\
      \3:2       | End\n"
    )
  , ( "a /* */ b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | BlockComment\n\
      \-         | Spaces 1\n\
      \0:8-0:9   | Identifier `b`\n\
      \0:9       | End\n"
    )
  , ( "a /* */ /* */ b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | BlockComment\n\
      \-         | Spaces 1\n\
      \-         | BlockComment\n\
      \-         | Spaces 1\n\
      \0:14-0:15 | Identifier `b`\n\
      \0:15      | End\n"
    )
  , ( "a /* \n */ b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | BlockComment\n\
      \+         | Spaces 1\n\
      \1:4-1:5   | Identifier `b`\n\
      \1:5       | End\n"
    )
  , ( "a /* \r */ b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | BlockComment\n\
      \+         | Spaces 1\n\
      \1:4-1:5   | Identifier `b`\n\
      \1:5       | End\n"
    )
  , ( "a /* \r\n */ b"
    , "0:0-0:1   | Identifier `a`\n\
      \-         | Spaces 1\n\
      \-         | BlockComment\n\
      \+         | Spaces 1\n\
      \1:4-1:5   | Identifier `b`\n\
      \1:5       | End\n"
    )
  ]
