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

spec = mapM_ (\(input, expected) -> it (T.unpack input) $ testParse input expected)
  [ ( "let x = y"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let"
    , "(0:3-0:3) We wanted a variable name but the file ended.\n\
      \(0:3-0:3) We wanted `=` but the file ended.\n\
      \(0:3-0:3) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind err err))\n"
    )
  , ( "let x"
    , "(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) err))\n"
    )
  , ( "let ="
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:5-0:5) We wanted a variable name but the file ended.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let y"
    , "(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `y`) err))\n"
    )
  , ( "let x ="
    , "(0:7-0:7) We wanted a variable name but the file ended.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let = y"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \\n\
      \(bind err (var 0:6-0:7 `y`))\n"
    )
  , ( "let x y"
    , "(0:6-0:7) We wanted `=` but we found a variable name.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (var 0:6-0:7 `y`)))\n"
    )
  , ( "let x = y"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "😈 let x = y"
    , "(0:0-0:2) We wanted `let` but we found `😈`.\n\
      \\n\
      \(err (bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`)))\n"
    )
  , ( "let 😈 x = y"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (err (var 0:7-0:8 `x`)) (var 0:11-0:12 `y`))\n"
    )
  , ( "let x 😈 = y"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (var 0:11-0:12 `y`)))\n"
    )
  , ( "let x = 😈 y"
    , "(0:8-0:10) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (err (var 0:11-0:12 `y`)))\n"
    )
  , ( "let x = y 😈"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( ") let x = y"
    , "(0:0-0:1) We wanted `let` but we found `)`.\n\
      \\n\
      \(err (bind (var 0:6-0:7 `x`) (var 0:10-0:11 `y`)))\n"
    )
  , ( "let ) x = y"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (err (var 0:6-0:7 `x`)) (var 0:10-0:11 `y`))\n"
    )
  , ( "let x ) = y"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (var 0:10-0:11 `y`)))\n"
    )
  , ( "let x = ) y"
    , "(0:8-0:9) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (err (var 0:10-0:11 `y`)))\n"
    )
  , ( "let x = y )"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let 😈 = y"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind err (var 0:9-0:10 `y`))\n"
    )
  , ( "let x 😈 y"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (var 0:9-0:10 `y`)))\n"
    )
  , ( "let x = 😈"
    , "(0:8-0:10) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let 😈 y"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:8) We wanted `=` but the file ended.\n\
      \(0:8-0:8) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind (err (var 0:7-0:8 `y`)) err))\n"
    )
  , ( "let 😈 ="
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:8) We wanted a variable name but the file ended.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let x 😈"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \(0:8-0:8) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) err))\n"
    )
  , ( "let = 😈"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:8) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let 😈"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:6) We wanted `=` but the file ended.\n\
      \(0:6-0:6) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind err err))\n"
    )
  , ( "let ) = y"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind err (var 0:8-0:9 `y`))\n"
    )
  , ( "let x ) y"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`)))\n"
    )
  , ( "let x = )"
    , "(0:8-0:9) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let ) y"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:7) We wanted `=` but the file ended.\n\
      \(0:7-0:7) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind (err (var 0:6-0:7 `y`)) err))\n"
    )
  , ( "let ) ="
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:7) We wanted a variable name but the file ended.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let x )"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \(0:7-0:7) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind (var 0:4-0:5 `x`) err))\n"
    )
  , ( "let = )"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:7) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let )"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (bind err err))\n"
    )
  ]
