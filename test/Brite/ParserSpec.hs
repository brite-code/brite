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

runTest :: T.Text -> T.Text -> Spec
runTest input expected =
  it (T.unpack (T.replace "\n" "\\n" input)) $
    let
      tokens = tokenize initialPosition input
      (module_, diagnostics) = runDiagnosticWriter (parse tokens)
      actual = L.toStrict $ B.toLazyText $
        (if null diagnostics then "" else
          mconcat (map debugDiagnostic diagnostics) <> B.singleton '\n')
        <> debugModule module_
    in
      actual `shouldBe` expected

spec :: Spec
spec = mapM_ (uncurry runTest)
  [ ( "let x = y;"
    , "(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y"
    , "(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let"
    , "(0:3-0:3) We wanted a variable name but the file ended.\n\
      \(0:3-0:3) We wanted `=` but the file ended.\n\
      \(0:3-0:3) We wanted an expression but the file ended.\n\
      \\n\
      \(err (bind 0:0-0:3 (err 0:3-0:3) (err 0:3-0:3)))\n"
    )
  , ( "let x"
    , "(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(err (bind 0:0-0:5 (var 0:4-0:5 `x`) (err 0:5-0:5)))\n"
    )
  , ( "let ="
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(bind 0:0-0:5 (err 0:4-0:5) (err 0:5-0:5))\n"
    )
  , ( "let y"
    , "(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(err (bind 0:0-0:5 (var 0:4-0:5 `y`) (err 0:5-0:5)))\n"
    )
  , ( "let ;"
    , "(0:4-0:5) We wanted a variable name but we found `;`.\n\
      \(0:4-0:5) We wanted `=` but we found `;`.\n\
      \(0:4-0:5) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind 0:0-0:5 (err 0:4-0:5) (err 0:4-0:5)))\n"
    )
  , ( "let x ="
    , "(0:7-0:7) We wanted an expression but the file ended.\n\
      \\n\
      \(bind 0:0-0:7 (var 0:4-0:5 `x`) (err 0:7-0:7))\n"
    )
  , ( "let x y"
    , "(0:6-0:7) We wanted `=` but we found a variable name.\n\
      \\n\
      \(err (bind 0:0-0:7 (var 0:4-0:5 `x`) (var 0:6-0:7 `y`)))\n"
    )
  , ( "let x ;"
    , "(0:6-0:7) We wanted `=` but we found `;`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind 0:0-0:7 (var 0:4-0:5 `x`) (err 0:6-0:7)))\n"
    )
  , ( "let = y"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \\n\
      \(bind 0:0-0:7 (err 0:4-0:5) (var 0:6-0:7 `y`))\n"
    )
  , ( "let = ;"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind 0:0-0:7 (err 0:4-0:5) (err 0:6-0:7))\n"
    )
  , ( "😈 let x = y;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind 0:3-0:13 (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let 😈 x = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:13 (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let x 😈 = y;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:13 (var 0:4-0:5 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let x = 😈 y;"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:13 (var 0:4-0:5 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let x = y 😈;"
    , "(0:10-0:12) We wanted `;` but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:13 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y; 😈"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y 😈"
    , "(0:10-0:12) We wanted `;` but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( ") let x = y;"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind 0:2-0:12 (var 0:6-0:7 `x`) (var 0:10-0:11 `y`))\n"
    )
  , ( "let ) x = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind 0:0-0:12 (var 0:6-0:7 `x`) (var 0:10-0:11 `y`))\n"
    )
  , ( "let x ) = y;"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:12 (var 0:4-0:5 `x`) (var 0:10-0:11 `y`))\n"
      )
  , ( "let x = ) y;"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:12 (var 0:4-0:5 `x`) (var 0:10-0:11 `y`))\n"
    )
  , ( "let x = y );"
    , "(0:10-0:11) We wanted `;` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:12 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y; )"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y )"
    , "(0:10-0:11) We wanted `;` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let 😈 = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:11 (err 0:4-0:6) (var 0:9-0:10 `y`))\n"
    )
  , ( "let x 😈 y;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(err (bind 0:0-0:11 (var 0:4-0:5 `x`) (var 0:9-0:10 `y`)))\n"
    )
  , ( "let x = 😈;"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:11 (var 0:4-0:5 `x`) (err 0:8-0:10))\n"
    )
  , ( "let 😈 y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:9) We wanted `=` but we found `;`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind 0:0-0:9 (var 0:7-0:8 `y`) (err 0:8-0:9)))\n"
    )
  , ( "let 😈 =;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind 0:0-0:9 (err 0:4-0:6) (err 0:8-0:9))\n"
    )
  , ( "let x 😈;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind 0:0-0:9 (var 0:4-0:5 `x`) (err 0:8-0:9)))\n"
    )
  , ( "let = 😈;"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:9 (err 0:4-0:5) (err 0:6-0:8))\n"
    )
  , ( "let 😈;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted `=` but we found `;`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind 0:0-0:7 (err 0:4-0:6) (err 0:6-0:7)))\n"
    )
  , ( "let ) = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind 0:0-0:10 (err 0:4-0:5) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x ) y;"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(err (bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`)))\n"
    )
  , ( "let x = );"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (err 0:8-0:9))\n"
    )
  , ( "let ) y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:8) We wanted `=` but we found `;`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind 0:0-0:8 (var 0:6-0:7 `y`) (err 0:7-0:8)))\n"
    )
  , ( "let ) =;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind 0:0-0:8 (err 0:4-0:5) (err 0:7-0:8))\n"
    )
  , ( "let x );"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind 0:0-0:8 (var 0:4-0:5 `x`) (err 0:7-0:8)))\n"
    )
  , ( "let = );"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:7) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:8 (err 0:4-0:5) (err 0:6-0:7))\n"
    )
  , ( "let );"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:5-0:6) We wanted `=` but we found `;`.\n\
      \(0:5-0:6) We wanted an expression but we found `;`.\n\
      \\n\
      \(err (bind 0:0-0:6 (err 0:4-0:5) (err 0:5-0:6)))\n"
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
    , "(0:3-0:5) We wanted a statement but we found `😈`.\n\
      \\n\
      \(var 0:0-0:1 `x`)\n"
    )
  , ( "="
    , "(0:0-0:1) We wanted a statement but we found `=`.\n\
      \\n\
      \empty\n"
    )
  , ( "😈"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \empty\n"
    )
  , ( ")"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \empty\n"
    )
  , ( ";"
    , "(0:0-0:1) We wanted a statement but we found `;`.\n\
      \\n\
      \empty\n"
    )
  , ( "true"
    , "(bool 0:0-0:4 true)\n"
    )
  , ( "false"
    , "(bool 0:0-0:5 false)\n"
    )
  , ( "("
    , "(0:1-0:1) We wanted an expression but the file ended.\n\
      \(0:1-0:1) We wanted `)` but the file ended.\n\
      \\n\
      \(err (wrap 0:0-0:1 (err 0:1-0:1)))\n"
    )
  , ( "(x"
    , "(0:2-0:2) We wanted `)` but the file ended.\n\
      \\n\
      \(err (wrap 0:0-0:2 (var 0:1-0:2 `x`)))\n"
    )
  , ( "()"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(wrap 0:0-0:2 (err 0:1-0:2))\n"
    )
  , ( "(x)"
    , "(wrap 0:0-0:3 (var 0:1-0:2 `x`))\n"
    )
  , ( "x)"
    , "(0:1-0:2) We wanted `;` but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `x`)\n"
    )
  , ( "(x;"
    , "(0:2-0:3) We wanted `)` but we found `;`.\n\
      \\n\
      \(err (wrap 0:0-0:2 (var 0:1-0:2 `x`)))\n"
    )
  , ( "let x = (y);"
    , "(bind 0:0-0:12 (var 0:4-0:5 `x`) (wrap 0:8-0:11 (var 0:9-0:10 `y`)))\n"
    )
  , ( "let x = (y;"
    , "(0:10-0:11) We wanted `)` but we found `;`.\n\
      \\n\
      \(bind 0:0-0:11 (var 0:4-0:5 `x`) (err (wrap 0:8-0:10 (var 0:9-0:10 `y`))))\n"
    )
  , ( "let x = y; let x = y;"
    , "(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:11-0:21 (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y;"
    , "(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:11-0:21 (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind 0:22-0:32 (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; let x = y;"
    , "(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:11-0:21 (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind 0:22-0:32 (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n\
      \(bind 0:33-0:43 (var 0:37-0:38 `x`) (var 0:41-0:42 `y`))\n"
    )
  , ( "let x = y let x = y"
    , "(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n"
    )
  , ( "let x = y let x = y let x = y"
    , "(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind 0:20-0:29 (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
    )
  , ( "let x = y let x = y let x = y let x = y"
    , "(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind 0:20-0:29 (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n\
      \(bind 0:30-0:39 (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))\n"
    )
  , ( "let x = y\nlet x = y\n"
    , "(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 1:0-1:9 (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n"
    )
  , ( "let x = y\nlet x = y\nlet x = y\n"
    , "(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 1:0-1:9 (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
      \(bind 2:0-2:9 (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))\n"
    )
  , ( "let x = y\nlet x = y\nlet x = y\nlet x = y\n"
    , "(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 1:0-1:9 (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
      \(bind 2:0-2:9 (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))\n\
      \(bind 3:0-3:9 (var 3:4-3:5 `x`) (var 3:8-3:9 `y`))\n"
    )
  , ( "😈 let x = y; let x = y; let x = y;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind 0:3-0:13 (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n\
      \(bind 0:14-0:24 (var 0:18-0:19 `x`) (var 0:22-0:23 `y`))\n\
      \(bind 0:25-0:35 (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
    )
  , ( "let x = y; 😈 let x = y; let x = y;"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:14-0:24 (var 0:18-0:19 `x`) (var 0:22-0:23 `y`))\n\
      \(bind 0:25-0:35 (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
    )
  , ( "let x = y; let x = y; 😈 let x = y;"
    , "(0:22-0:24) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:11-0:21 (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind 0:25-0:35 (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; 😈"
    , "(0:33-0:35) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:11-0:21 (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind 0:22-0:32 (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "😈 let x = y let x = y let x = y"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind 0:3-0:12 (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n\
      \(bind 0:13-0:22 (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind 0:23-0:32 (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
    )
  , ( "let x = y 😈 let x = y let x = y"
    , "(0:10-0:12) We wanted `;` but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:13-0:22 (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind 0:23-0:32 (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
    )
  , ( "let x = y let x = y 😈 let x = y"
    , "(0:20-0:22) We wanted `;` but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind 0:23-0:32 (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
    )
  , ( "let x = y let x = y let x = y 😈"
    , "(0:30-0:32) We wanted `;` but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind 0:20-0:29 (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
    )
  -- , ( "do { let x = y; }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n"
  --   )
  -- , ( "do { let x = y }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n"
  --   )
  -- , ( "do { let x = y; let x = y; }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n"
  --   )
  -- , ( "do { let x = y; let x = y; let x = y; }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
  --     \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
  --   )
  -- , ( "do { let x = y; let x = y; let x = y; let x = y; }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
  --     \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n\
  --     \(bind (var 0:37-0:38 `x`) (var 0:41-0:42 `y`))\n"
  --   )
  -- , ( "do { let x = y let x = y }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n"
  --   )
  -- , ( "do { let x = y let x = y let x = y }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
  --     \(bind (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
  --   )
  -- , ( "do { let x = y let x = y let x = y let x = y }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
  --     \(bind (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n\
  --     \(bind (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))\n"
  --   )
  -- , ( "do { let x = y\nlet x = y\n }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n"
  --   )
  -- , ( "do { let x = y\nlet x = y\nlet x = y\n }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
  --     \(bind (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))\n"
  --   )
  -- , ( "do { let x = y\nlet x = y\nlet x = y\nlet x = y\n }"
  --   , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
  --     \(bind (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))\n\
  --     \(bind (var 3:4-3:5 `x`) (var 3:8-3:9 `y`))\n"
  --   )
  -- , ( "do { 😈 let x = y; let x = y; let x = y; }"
  --   , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
  --     \\n\
  --     \(bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n\
  --     \(bind (var 0:18-0:19 `x`) (var 0:22-0:23 `y`))\n\
  --     \(bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
  --   )
  -- , ( "do { let x = y; 😈 let x = y; let x = y; }"
  --   , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
  --     \\n\
  --     \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:18-0:19 `x`) (var 0:22-0:23 `y`))\n\
  --     \(bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
  --   )
  -- , ( "do { let x = y; let x = y; 😈 let x = y; }"
  --   , "(0:22-0:24) We wanted a statement but we found `😈`.\n\
  --     \\n\
  --     \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
  --     \(bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
  --   )
  -- , ( "do { let x = y; let x = y; let x = y; 😈 }"
  --   , "(0:33-0:35) We wanted a statement but we found `😈`.\n\
  --     \\n\
  --     \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
  --     \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
  --   )
  -- , ( "do { 😈 let x = y let x = y let x = y }"
  --   , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
  --     \\n\
  --     \(bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n\
  --     \(bind (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
  --     \(bind (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
  --   )
  -- , ( "do { let x = y 😈 let x = y let x = y }"
  --   , "(0:10-0:12) We wanted `;` but we found `😈`.\n\
  --     \\n\
  --     \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
  --     \(bind (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
  --   )
  -- , ( "do { let x = y let x = y 😈 let x = y }"
  --   , "(0:20-0:22) We wanted `;` but we found `😈`.\n\
  --     \\n\
  --     \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
  --     \(bind (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
  --   )
  -- , ( "do { let x = y let x = y let x = y 😈 }"
  --   , "(0:30-0:32) We wanted `;` but we found `😈`.\n\
  --     \\n\
  --     \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
  --     \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
  --     \(bind (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
  --   )
  ]
