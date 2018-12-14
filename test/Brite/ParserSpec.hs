{-# LANGUAGE OverloadedStrings #-}

module Brite.ParserSpec (spec) where

import Brite.AST
import Brite.Diagnostics
import qualified Brite.Parser as P
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
    (statement, diagnostics) = runDiagnosticWriter (runParser P.statement tokens)
    actual = L.toStrict $ B.toLazyText $
      (if null diagnostics then "" else
        mconcat (map debugDiagnostic diagnostics) <> B.singleton '\n')
      <> debugStatement (either (error "nope") id statement)
      <> B.singleton '\n'
  in
    actual `shouldBe` expected

spec = mapM_ (\(input, expected) -> it (T.unpack input) $ testParse input expected)
  [ ( "let x = y;"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let"
    , "(0:3-0:3) We wanted a variable name but the file ended.\n\
      \(0:3-0:3) We wanted `=` but the file ended.\n\
      \(0:3-0:3) We wanted an expression but the file ended.\n\
      \\n\
      \(err (bind (err 0:3-0:3) (err 0:3-0:3)))\n"
    )
  , ( "let x"
    , "(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (err 0:5-0:5)))\n"
    )
  , ( "let ="
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(bind (err 0:4-0:5) (err 0:5-0:5))\n"
    )
  , ( "let y"
    , "(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `y`) (err 0:5-0:5)))\n"
    )
  , ( "let ;"
    , "(0:4-0:5) We wanted a variable name but we found `;`.\n\
      \(0:4-0:5) We wanted `=` but we found `;`.\n\
      \(0:4-0:5) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind (err 0:4-0:5) (err 0:4-0:5)))\n"
    )
  , ( "let x ="
    , "(0:7-0:7) We wanted an expression but the file ended.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (err 0:7-0:7))\n"
    )
  , ( "let x y"
    , "(0:6-0:7) We wanted `=` but we found a variable name.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (var 0:6-0:7 `y`)))\n"
    )
  , ( "let x ;"
    , "(0:6-0:7) We wanted `=` but we found `;`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (err 0:6-0:7)))\n"
    )
  , ( "let = y"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \\n\
      \(bind (err 0:4-0:5) (var 0:6-0:7 `y`))\n"
    )
  , ( "let = ;"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (err 0:4-0:5) (err 0:6-0:7))\n"
    )
  , ( "let x = y;"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "😈 let x = y;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let 😈 x = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let x 😈 = y;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let x = 😈 y;"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let x = y 😈;"
    , "(0:10-0:12) We wanted `;` but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y; 😈"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y 😈"
    , "(0:10-0:12) We wanted `;` but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( ") let x = y;"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:6-0:7 `x`) (var 0:10-0:11 `y`))\n"
    )
  , ( "let ) x = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (var 0:6-0:7 `x`) (var 0:10-0:11 `y`))\n"
    )
  , ( "let x ) = y;"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:10-0:11 `y`))\n"
      )
  , ( "let x = ) y;"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:10-0:11 `y`))\n"
    )
  , ( "let x = y );"
    , "(0:10-0:11) We wanted `;` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y; )"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y )"
    , "(0:10-0:11) We wanted `;` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let 😈 = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (err 0:4-0:6) (var 0:9-0:10 `y`))\n"
    )
  , ( "let x 😈 y;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (var 0:9-0:10 `y`)))\n"
    )
  , ( "let x = 😈;"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (err 0:8-0:10))\n"
    )
  , ( "let 😈 y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:9) We wanted `=` but we found `;`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind (var 0:7-0:8 `y`) (err 0:8-0:9)))\n"
    )
  , ( "let 😈 =;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (err 0:4-0:6) (err 0:8-0:9))\n"
    )
  , ( "let x 😈;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (err 0:8-0:9)))\n"
    )
  , ( "let = 😈;"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (err 0:4-0:5) (err 0:6-0:8))\n"
    )
  , ( "let 😈;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted `=` but we found `;`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind (err 0:4-0:6) (err 0:6-0:7)))\n"
    )
  , ( "let ) = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (err 0:4-0:5) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x ) y;"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`)))\n"
    )
  , ( "let x = );"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (err 0:8-0:9))\n"
    )
  , ( "let ) y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:8) We wanted `=` but we found `;`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind (var 0:6-0:7 `y`) (err 0:7-0:8)))\n"
    )
  , ( "let ) =;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (err 0:4-0:5) (err 0:7-0:8))\n"
    )
  , ( "let x );"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (err 0:7-0:8)))\n"
    )
  , ( "let = );"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:7) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (err 0:4-0:5) (err 0:6-0:7))\n"
    )
  , ( "let );"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:5-0:6) We wanted `=` but we found `;`.\n\
      \(0:5-0:6) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind (err 0:4-0:5) (err 0:5-0:6)))\n"
    )
  , ( "x"
    , "(var 0:0-0:1 `x`)\n"
    )
  , ( "x;"
    , "(var 0:0-0:1 `x`)\n"
    )
  , ( "😈 x"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(var 0:3-0:4 `x`)\n"
    )
  , ( "x 😈"
    , "(0:2-0:4) We wanted `;` but we found `😈`.\n\
      \\n\
      \(var 0:0-0:1 `x`)\n"
    )
  , ( "😈 x;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(var 0:3-0:4 `x`)\n"
    )
  , ( "x 😈;"
    , "(0:2-0:4) We wanted `;` but we found `😈`.\n\
      \\n\
      \(var 0:0-0:1 `x`)\n"
    )
  , ( "x; 😈"
    , "(var 0:0-0:1 `x`)\n"
    )
  , ( "="
    , "(0:0-0:1) We wanted a statement but we found `=`.\n\
      \\n\
      \err\n"
    )
  , ( "😈"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \err\n"
    )
  , ( ")"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \err\n"
    )
  , ( ";"
    , "(0:0-0:1) We wanted a statement but we found `;`.\n\
      \\n\
      \err\n"
    )
  , ( "true"
    , "(bool 0:0-0:4 true)\n"
    )
  , ( "false"
    , "(bool 0:0-0:5 false)\n"
    )
  ]
