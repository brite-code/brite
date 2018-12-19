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
  it (T.unpack (escape input)) $
    let
      tokens = tokenize initialPosition input
      (module_, diagnostics) = runDiagnosticWriter (parse tokens)
      actual = L.toStrict $ B.toLazyText $
        (if null diagnostics then "" else
          mconcat (map debugDiagnostic diagnostics) <> B.singleton '\n')
        <> debugModule module_
      rebuiltInput = L.toStrict (B.toLazyText (rebuildSource (tokenize' input)))
    in do
      actual `shouldBe` expected
      rebuiltInput `shouldBe` input

escape :: T.Text -> T.Text
escape = T.concatMap
  (\c ->
    case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      _ -> T.singleton c)

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
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:13 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y; 😈"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y 😈"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
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
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:12 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y; )"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y )"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
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
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(var 0:0-0:1 `x`)\n"
    )
  , ( "😈 x;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(var 0:3-0:4 `x`)\n"
    )
  , ( "x 😈;"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
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
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
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
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:13-0:22 (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind 0:23-0:32 (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
    )
  , ( "let x = y let x = y 😈 let x = y"
    , "(0:20-0:22) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind 0:23-0:32 (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
    )
  , ( "let x = y let x = y let x = y 😈"
    , "(0:30-0:32) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind 0:20-0:29 (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
    )
  , ( ") let x = y; let x = y; let x = y;"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind 0:2-0:12 (var 0:6-0:7 `x`) (var 0:10-0:11 `y`))\n\
      \(bind 0:13-0:23 (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind 0:24-0:34 (var 0:28-0:29 `x`) (var 0:32-0:33 `y`))\n"
    )
  , ( "let x = y; ) let x = y; let x = y;"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:13-0:23 (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind 0:24-0:34 (var 0:28-0:29 `x`) (var 0:32-0:33 `y`))\n"
    )
  , ( "let x = y; let x = y; ) let x = y;"
    , "(0:22-0:23) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:11-0:21 (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind 0:24-0:34 (var 0:28-0:29 `x`) (var 0:32-0:33 `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; )"
    , "(0:33-0:34) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind 0:0-0:10 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:11-0:21 (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind 0:22-0:32 (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( ") let x = y let x = y let x = y"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind 0:2-0:11 (var 0:6-0:7 `x`) (var 0:10-0:11 `y`))\n\
      \(bind 0:12-0:21 (var 0:16-0:17 `x`) (var 0:20-0:21 `y`))\n\
      \(bind 0:22-0:31 (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "let x = y ) let x = y let x = y"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:12-0:21 (var 0:16-0:17 `x`) (var 0:20-0:21 `y`))\n\
      \(bind 0:22-0:31 (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "let x = y let x = y ) let x = y"
    , "(0:20-0:21) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind 0:22-0:31 (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "let x = y let x = y let x = y )"
    , "(0:30-0:31) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind 0:20-0:29 (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
    )
  , ( "do {}"
    , "(do 0:0-0:5 (block 0:3-0:5))\n"
    )
  , ( "do { "
    , "(0:5-0:5) We wanted `}` but the file ended.\n\
      \\n\
      \(err (do 0:0-0:4 (block 0:3-0:4)))\n"
    )
  , ( "do }"
    , "(0:3-0:4) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (do 0:0-0:4 (block 0:3-0:4)))\n"
    )
  , ( "do } do }"
    , "(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:8-0:9) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (do 0:0-0:4 (block 0:3-0:4)))\n\
      \(err (do 0:5-0:9 (block 0:8-0:9)))\n"
    )
  , ( "do"
    , "(0:2-0:2) We wanted `{` but the file ended.\n\
      \(0:2-0:2) We wanted `}` but the file ended.\n\
      \\n\
      \(err (do 0:0-0:2 (block 0:2-0:2)))\n"
    )
  , ( "do do"
    , "(0:3-0:5) We wanted `{` but we found `do`.\n\
      \(0:5-0:5) We wanted `{` but the file ended.\n\
      \(0:5-0:5) We wanted `}` but the file ended.\n\
      \(0:5-0:5) We wanted `}` but the file ended.\n\
      \\n\
      \(err (do 0:0-0:5 (block 0:3-0:5\n\
      \  (err (do 0:3-0:5 (block 0:5-0:5))))))\n"
    )
  , ( "do { let x = y; }"
    , "(do 0:0-0:17 (block 0:3-0:17\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))))\n"
    )
  , ( "do { let x = y; "
    , "(0:16-0:16) We wanted `}` but the file ended.\n\
      \\n\
      \(err (do 0:0-0:15 (block 0:3-0:15\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`)))))\n"
    )
  , ( "do let x = y; }"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \\n\
      \(err (do 0:0-0:15 (block 0:3-0:15\n\
      \  (bind 0:3-0:13 (var 0:7-0:8 `x`) (var 0:11-0:12 `y`)))))\n"
    )
  , ( "do let x = y }"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \\n\
      \(err (do 0:0-0:14 (block 0:3-0:14\n\
      \  (bind 0:3-0:12 (var 0:7-0:8 `x`) (var 0:11-0:12 `y`)))))\n"
    )
  , ( "do let x = y;"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \(0:13-0:13) We wanted `}` but the file ended.\n\
      \\n\
      \(err (do 0:0-0:13 (block 0:3-0:13\n\
      \  (bind 0:3-0:13 (var 0:7-0:8 `x`) (var 0:11-0:12 `y`)))))\n"
    )
  , ( "do let x = y"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \(0:12-0:12) We wanted `}` but the file ended.\n\
      \\n\
      \(err (do 0:0-0:12 (block 0:3-0:12\n\
      \  (bind 0:3-0:12 (var 0:7-0:8 `x`) (var 0:11-0:12 `y`)))))\n"
    )
  , ( "let x = (do {);"
    , "(0:13-0:14) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:15 (var 0:4-0:5 `x`) (wrap 0:8-0:14 (err (do 0:9-0:13 (block 0:12-0:13)))))\n"
    )
  , ( "let x = (do { let y = z; );"
    , "(0:25-0:26) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:27 (var 0:4-0:5 `x`) (wrap 0:8-0:26 (err (do 0:9-0:24 (block 0:12-0:24\n\
      \  (bind 0:14-0:24 (var 0:18-0:19 `y`) (var 0:22-0:23 `z`)))))))\n"
    )
  , ( "let x = (do);"
    , "(0:11-0:12) We wanted `{` but we found `)`.\n\
      \(0:11-0:12) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:13 (var 0:4-0:5 `x`) (wrap 0:8-0:12 (err (do 0:9-0:12 (block 0:11-0:12)))))\n"
    )
  , ( "let x = (do let y = z; );"
    , "(0:12-0:15) We wanted `{` but we found `let`.\n\
      \(0:23-0:24) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:25 (var 0:4-0:5 `x`) (wrap 0:8-0:24 (err (do 0:9-0:22 (block 0:12-0:22\n\
      \  (bind 0:12-0:22 (var 0:16-0:17 `y`) (var 0:20-0:21 `z`)))))))\n"
    )
  , ( "let x = (do { let y = z );"
    , "(0:24-0:25) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:26 (var 0:4-0:5 `x`) (wrap 0:8-0:25 (err (do 0:9-0:23 (block 0:12-0:23\n\
      \  (bind 0:14-0:23 (var 0:18-0:19 `y`) (var 0:22-0:23 `z`)))))))\n"
    )
  , ( "let x = (do { let y = );"
    , "(0:22-0:23) We wanted an expression but we found `)`.\n\
      \(0:22-0:23) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind 0:0-0:24 (var 0:4-0:5 `x`) (wrap 0:8-0:23 (err (do 0:9-0:23 (block 0:12-0:23\n\
      \  (bind 0:14-0:23 (var 0:18-0:19 `y`) (err 0:22-0:23)))))))\n"
    )
  , ( "do { let x = y; }"
    , "(do 0:0-0:17 (block 0:3-0:17\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))))\n"
    )
  , ( "do { let x = y }"
    , "(do 0:0-0:16 (block 0:3-0:16\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; }"
    , "(do 0:0-0:28 (block 0:3-0:28\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:16-0:26 (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; }"
    , "(do 0:0-0:39 (block 0:3-0:39\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:16-0:26 (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind 0:27-0:37 (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; let x = y; }"
    , "(do 0:0-0:50 (block 0:3-0:50\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:16-0:26 (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind 0:27-0:37 (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))\n\
      \  (bind 0:38-0:48 (var 0:42-0:43 `x`) (var 0:46-0:47 `y`))))\n"
    )
  , ( "do { let x = y let x = y }"
    , "(do 0:0-0:26 (block 0:3-0:26\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:15-0:24 (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y }"
    , "(do 0:0-0:36 (block 0:3-0:36\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:15-0:24 (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind 0:25-0:34 (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y let x = y }"
    , "(do 0:0-0:46 (block 0:3-0:46\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:15-0:24 (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind 0:25-0:34 (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n\
      \  (bind 0:35-0:44 (var 0:39-0:40 `x`) (var 0:43-0:44 `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\n }"
    , "(do 0:0-2:2 (block 0:3-2:2\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 1:0-1:9 (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\nlet x = y\n }"
    , "(do 0:0-3:2 (block 0:3-3:2\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 1:0-1:9 (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
      \  (bind 2:0-2:9 (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\nlet x = y\nlet x = y\n }"
    , "(do 0:0-4:2 (block 0:3-4:2\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 1:0-1:9 (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
      \  (bind 2:0-2:9 (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))\n\
      \  (bind 3:0-3:9 (var 3:4-3:5 `x`) (var 3:8-3:9 `y`))))\n"
    )
  , ( "do { 😈 let x = y; let x = y; let x = y; }"
    , "(0:5-0:7) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do 0:0-0:42 (block 0:3-0:42\n\
      \  (bind 0:8-0:18 (var 0:12-0:13 `x`) (var 0:16-0:17 `y`))\n\
      \  (bind 0:19-0:29 (var 0:23-0:24 `x`) (var 0:27-0:28 `y`))\n\
      \  (bind 0:30-0:40 (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))))\n"
    )
  , ( "do { let x = y; 😈 let x = y; let x = y; }"
    , "(0:16-0:18) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do 0:0-0:42 (block 0:3-0:42\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:19-0:29 (var 0:23-0:24 `x`) (var 0:27-0:28 `y`))\n\
      \  (bind 0:30-0:40 (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; 😈 let x = y; }"
    , "(0:27-0:29) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do 0:0-0:42 (block 0:3-0:42\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:16-0:26 (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind 0:30-0:40 (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; 😈 }"
    , "(0:38-0:40) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do 0:0-0:42 (block 0:3-0:42\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:16-0:26 (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind 0:27-0:37 (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { 😈 let x = y let x = y let x = y }"
    , "(0:5-0:7) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do 0:0-0:39 (block 0:3-0:39\n\
      \  (bind 0:8-0:17 (var 0:12-0:13 `x`) (var 0:16-0:17 `y`))\n\
      \  (bind 0:18-0:27 (var 0:22-0:23 `x`) (var 0:26-0:27 `y`))\n\
      \  (bind 0:28-0:37 (var 0:32-0:33 `x`) (var 0:36-0:37 `y`))))\n"
    )
  , ( "do { let x = y 😈 let x = y let x = y }"
    , "(0:15-0:17) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do 0:0-0:39 (block 0:3-0:39\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:18-0:27 (var 0:22-0:23 `x`) (var 0:26-0:27 `y`))\n\
      \  (bind 0:28-0:37 (var 0:32-0:33 `x`) (var 0:36-0:37 `y`))))\n"
    )
  , ( "do { let x = y let x = y 😈 let x = y }"
    , "(0:25-0:27) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do 0:0-0:39 (block 0:3-0:39\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:15-0:24 (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind 0:28-0:37 (var 0:32-0:33 `x`) (var 0:36-0:37 `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y 😈 }"
    , "(0:35-0:37) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do 0:0-0:39 (block 0:3-0:39\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:15-0:24 (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind 0:25-0:34 (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))))\n"
    )
  , ( "do { ) let x = y; let x = y; let x = y; }"
    , "(0:5-0:6) We wanted a statement but we found `)`.\n\
      \\n\
      \(do 0:0-0:41 (block 0:3-0:41\n\
      \  (bind 0:7-0:17 (var 0:11-0:12 `x`) (var 0:15-0:16 `y`))\n\
      \  (bind 0:18-0:28 (var 0:22-0:23 `x`) (var 0:26-0:27 `y`))\n\
      \  (bind 0:29-0:39 (var 0:33-0:34 `x`) (var 0:37-0:38 `y`))))\n"
    )
  , ( "do { let x = y; ) let x = y; let x = y; }"
    , "(0:16-0:17) We wanted a statement but we found `)`.\n\
      \\n\
      \(do 0:0-0:41 (block 0:3-0:41\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:18-0:28 (var 0:22-0:23 `x`) (var 0:26-0:27 `y`))\n\
      \  (bind 0:29-0:39 (var 0:33-0:34 `x`) (var 0:37-0:38 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; ) let x = y; }"
    , "(0:27-0:28) We wanted a statement but we found `)`.\n\
      \\n\
      \(do 0:0-0:41 (block 0:3-0:41\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:16-0:26 (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind 0:29-0:39 (var 0:33-0:34 `x`) (var 0:37-0:38 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; ) }"
    , "(0:38-0:39) We wanted a statement but we found `)`.\n\
      \\n\
      \(do 0:0-0:41 (block 0:3-0:41\n\
      \  (bind 0:5-0:15 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:16-0:26 (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind 0:27-0:37 (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { ) let x = y let x = y let x = y }"
    , "(0:5-0:6) We wanted a statement but we found `)`.\n\
      \\n\
      \(do 0:0-0:38 (block 0:3-0:38\n\
      \  (bind 0:7-0:16 (var 0:11-0:12 `x`) (var 0:15-0:16 `y`))\n\
      \  (bind 0:17-0:26 (var 0:21-0:22 `x`) (var 0:25-0:26 `y`))\n\
      \  (bind 0:27-0:36 (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { let x = y ) let x = y let x = y }"
    , "(0:15-0:16) We wanted an expression but we found `)`.\n\
      \\n\
      \(do 0:0-0:38 (block 0:3-0:38\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:17-0:26 (var 0:21-0:22 `x`) (var 0:25-0:26 `y`))\n\
      \  (bind 0:27-0:36 (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { let x = y let x = y ) let x = y }"
    , "(0:25-0:26) We wanted an expression but we found `)`.\n\
      \\n\
      \(do 0:0-0:38 (block 0:3-0:38\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:15-0:24 (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind 0:27-0:36 (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y ) }"
    , "(0:35-0:36) We wanted an expression but we found `)`.\n\
      \\n\
      \(do 0:0-0:38 (block 0:3-0:38\n\
      \  (bind 0:5-0:14 (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind 0:15-0:24 (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind 0:25-0:34 (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))))\n"
    )
  , ( "let x = ) let x = )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:18-0:19) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (err 0:8-0:9))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `x`) (err 0:18-0:19))\n"
    )
  , ( "let x = ) )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (err 0:8-0:9))\n"
    )
  , ( ") let x = )"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:2-0:11 (var 0:6-0:7 `x`) (err 0:10-0:11))\n"
    )
  , ( "let x = ) ) let x = )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \(0:20-0:21) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind 0:0-0:9 (var 0:4-0:5 `x`) (err 0:8-0:9))\n\
      \(bind 0:12-0:21 (var 0:16-0:17 `x`) (err 0:20-0:21))\n"
    )
  , ( "do do 😈"
    , "(0:3-0:5) We wanted `{` but we found `do`.\n\
      \(0:6-0:8) We wanted `{` but we found `😈`.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \\n\
      \(err (do 0:0-0:8 (block 0:3-0:8\n\
      \  (err (do 0:3-0:8 (block 0:6-0:8))))))\n"
    )
  , ( "do do )"
    , "(0:3-0:5) We wanted `{` but we found `do`.\n\
      \(0:6-0:7) We wanted `{` but we found `)`.\n\
      \(0:7-0:7) We wanted `}` but the file ended.\n\
      \(0:7-0:7) We wanted `}` but the file ended.\n\
      \\n\
      \(err (do 0:0-0:7 (block 0:3-0:7\n\
      \  (err (do 0:3-0:7 (block 0:6-0:7))))))\n"
    )
  , ( "if x {}"
    , "(if 0:0-0:7\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:7))\n"
    )
  , ( "if x { y }"
    , "(if 0:0-0:10\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:10\n\
      \    (var 0:7-0:8 `y`)))\n"
    )
  , ( "if x {} else {}"
    , "(if 0:0-0:15\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:7)\n\
      \  (block 0:13-0:15))\n"
    )
  , ( "if x { y } else {}"
    , "(if 0:0-0:18\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:10\n\
      \    (var 0:7-0:8 `y`))\n\
      \  (block 0:16-0:18))\n"
    )
  , ( "if x {} else { y }"
    , "(if 0:0-0:18\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:7)\n\
      \  (block 0:13-0:18\n\
      \    (var 0:15-0:16 `y`)))\n"
    )
  , ( "if x { y } else { z }"
    , "(if 0:0-0:21\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:10\n\
      \    (var 0:7-0:8 `y`))\n\
      \  (block 0:16-0:21\n\
      \    (var 0:18-0:19 `z`)))\n"
    )
  , ( "if {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \\n\
      \(if 0:0-0:5\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:5))\n"
    )
  , ( "if x }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:6\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)))\n"
    )
  , ( "if x {"
    , "(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:6\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)))\n"
    )
  , ( "if x"
    , "(0:4-0:4) We wanted `{` but the file ended.\n\
      \(0:4-0:4) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:4\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:4-0:4)))\n"
    )
  , ( "if {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:4-0:4) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:4\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)))\n"
    )
  , ( "if }"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:4\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)))\n"
    )
  , ( "if {} else {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \\n\
      \(if 0:0-0:13\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:5)\n\
      \  (block 0:11-0:13))\n"
    )
  , ( "if x } else {}"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:14\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)\n\
      \  (block 0:12-0:14)))\n"
    )
  , ( "if x { else {}"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \\n\
      \(err (if 0:0-0:14\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)\n\
      \  (block 0:12-0:14)))\n"
    )
  , ( "if x else {}"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \\n\
      \(err (if 0:0-0:12\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:9)\n\
      \  (block 0:10-0:12)))\n"
    )
  , ( "if { else {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \\n\
      \(err (if 0:0-0:12\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)\n\
      \  (block 0:10-0:12)))\n"
    )
  , ( "if } else {}"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:12\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)\n\
      \  (block 0:10-0:12)))\n"
    )
  , ( "if {} {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:6-0:7) We wanted `else` but we found `{`.\n\
      \(0:7-0:8) We wanted `else` but we found `}`.\n\
      \\n\
      \(if 0:0-0:5\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:5))\n"
    )
  , ( "if x } {}"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:7-0:8) We wanted `else` but we found `{`.\n\
      \(0:8-0:9) We wanted `else` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:6\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)))\n"
    )
  , ( "if x { {}"
    , "(0:7-0:8) We wanted a statement but we found `{`.\n\
      \\n\
      \(if 0:0-0:9\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:9))\n"
    )
  , ( "if { {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:6) We wanted a statement but we found `{`.\n\
      \\n\
      \(if 0:0-0:7\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:7))\n"
    )
  , ( "if } {}"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:5-0:6) We wanted `else` but we found `{`.\n\
      \(0:6-0:7) We wanted `else` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:4\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)))\n"
    )
  , ( "if {} else }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:11-0:12) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:12\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:5)\n\
      \  (block 0:11-0:12)))\n"
    )
  , ( "if x } else }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:12-0:13) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:13\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)\n\
      \  (block 0:12-0:13)))\n"
    )
  , ( "if x { else }"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:12-0:13) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:13\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)\n\
      \  (block 0:12-0:13)))\n"
    )
  , ( "if x else }"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:10-0:11) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:11\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:9)\n\
      \  (block 0:10-0:11)))\n"
    )
  , ( "if { else }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:10-0:11) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:11\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)\n\
      \  (block 0:10-0:11)))\n"
    )
  , ( "if } else }"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:10-0:11) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:11\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)\n\
      \  (block 0:10-0:11)))\n"
    )
  , ( "if {} else {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:12-0:12) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:12\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:5)\n\
      \  (block 0:11-0:12)))\n"
    )
  , ( "if x } else {"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:13-0:13) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:13\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)\n\
      \  (block 0:12-0:13)))\n"
    )
  , ( "if x { else {"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:13-0:13) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:13\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)\n\
      \  (block 0:12-0:13)))\n"
    )
  , ( "if x else {"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:11\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:9)\n\
      \  (block 0:10-0:11)))\n"
    )
  , ( "if { else {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:11\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)\n\
      \  (block 0:10-0:11)))\n"
    )
  , ( "if } else {"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:11\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)\n\
      \  (block 0:10-0:11)))\n"
    )
  , ( "if {} else"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:10-0:10) We wanted `{` but the file ended.\n\
      \(0:10-0:10) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:10\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:5)\n\
      \  (block 0:10-0:10)))\n"
    )
  , ( "if x } else"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:11-0:11) We wanted `{` but the file ended.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:11\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)\n\
      \  (block 0:11-0:11)))\n"
    )
  , ( "if x { else"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:11-0:11) We wanted `{` but the file ended.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:11\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)\n\
      \  (block 0:11-0:11)))\n"
    )
  , ( "if x else"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:9-0:9) We wanted `{` but the file ended.\n\
      \(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:9\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:9)\n\
      \  (block 0:9-0:9)))\n"
    )
  , ( "if { else"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:9-0:9) We wanted `{` but the file ended.\n\
      \(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:9\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)\n\
      \  (block 0:9-0:9)))\n"
    )
  , ( "if } else"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:9-0:9) We wanted `{` but the file ended.\n\
      \(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:9\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)\n\
      \  (block 0:9-0:9)))\n"
    )
  , ( "if {} {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:6-0:7) We wanted `else` but we found `{`.\n\
      \\n\
      \(if 0:0-0:5\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:5))\n"
    )
  , ( "if x } {"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:7-0:8) We wanted `else` but we found `{`.\n\
      \\n\
      \(err (if 0:0-0:6\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)))\n"
    )
  , ( "if x { {"
    , "(0:7-0:8) We wanted a statement but we found `{`.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:6\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)))\n"
    )
  , ( "if x {"
    , "(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:6\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)))\n"
    )
  , ( "if { {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:6) We wanted a statement but we found `{`.\n\
      \(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(err (if 0:0-0:4\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)))\n"
    )
  , ( "if } {"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:5-0:6) We wanted `else` but we found `{`.\n\
      \\n\
      \(err (if 0:0-0:4\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)))\n"
    )
  , ( "if {} }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:6-0:7) We wanted `else` but we found `}`.\n\
      \\n\
      \(if 0:0-0:5\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:5))\n"
    )
  , ( "if x } }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:7-0:8) We wanted `else` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:6\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)))\n"
    )
  , ( "if x { }"
    , "(if 0:0-0:8\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:8))\n"
    )
  , ( "if x }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:6\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:6)))\n"
    )
  , ( "if { }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \\n\
      \(if 0:0-0:6\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:6))\n"
    )
  , ( "if } }"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:5-0:6) We wanted `else` but we found `}`.\n\
      \\n\
      \(err (if 0:0-0:4\n\
      \  (err 0:3-0:4)\n\
      \  (block 0:3-0:4)))\n"
    )
  , ( "x 😈 😈 ;"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(var 0:0-0:1 `x`)\n"
    )
  , ( "x ) ) ;"
    , "(0:2-0:3) We wanted an expression but we found `)`.\n\
      \(0:4-0:5) We wanted an expression but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `x`)\n"
    )
  , ( "let 😈 😈 x = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:7-0:9) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind 0:0-0:16 (var 0:10-0:11 `x`) (var 0:14-0:15 `y`))\n"
    )
  , ( "let ) ) x = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:6-0:7) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind 0:0-0:14 (var 0:8-0:9 `x`) (var 0:12-0:13 `y`))\n"
    )
  , ( "😈 😈"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \(0:3-0:5) We wanted a statement but we found `😈`.\n\
      \\n\
      \empty\n"
    )
  , ( ") )"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \(0:2-0:3) We wanted a statement but we found `)`.\n\
      \\n\
      \empty\n"
    )
  , ( "if x {} { let y = z }"
    , "(0:8-0:9) We wanted `else` but we found `{`.\n\
      \(0:20-0:21) We wanted an expression but we found `}`.\n\
      \\n\
      \(if 0:0-0:7\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block 0:5-0:7))\n\
      \(bind 0:10-0:19 (var 0:14-0:15 `y`) (var 0:18-0:19 `z`))\n"
    )
  , ( "o.p"
    , "(prop 0:0-0:3 (var 0:0-0:1 `o`) (name 0:2-0:3 `p`))\n"
    )
  , ( "o.p.q"
    , "(prop 0:0-0:5 (prop 0:0-0:3 (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:4-0:5 `q`))\n"
    )
  , ( "o."
    , "(0:2-0:2) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (var 0:0-0:1 `o`))\n"
    )
  , ( "o.p."
    , "(0:4-0:4) We wanted a variable name but the file ended.\n\
      \\n\
      \(err (prop 0:0-0:3 (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)))\n"
    )
  , ( "o p"
    , "(var 0:0-0:1 `o`)\n\
      \(var 0:2-0:3 `p`)\n"
    )
  , ( "o😈.p"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \\n\
      \(prop 0:0-0:5 (var 0:0-0:1 `o`) (name 0:4-0:5 `p`))\n"
    )
  , ( "o.😈p"
    , "(0:2-0:4) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop 0:0-0:5 (var 0:0-0:1 `o`) (name 0:4-0:5 `p`))\n"
    )
  , ( "o.😈p.q"
    , "(0:2-0:4) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop 0:0-0:7 (prop 0:0-0:5 (var 0:0-0:1 `o`) (name 0:4-0:5 `p`)) (name 0:6-0:7 `q`))\n"
    )
  , ( "o.p😈.q"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(prop 0:0-0:7 (prop 0:0-0:3 (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:6-0:7 `q`))\n"
    )
  , ( "o.p.😈q"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop 0:0-0:7 (prop 0:0-0:3 (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:6-0:7 `q`))\n"
    )
  , ( "o).p"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(prop 0:0-0:4 (var 0:0-0:1 `o`) (name 0:3-0:4 `p`))\n"
    )
  , ( "o.)p"
    , "(0:2-0:3) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop 0:0-0:4 (var 0:0-0:1 `o`) (name 0:3-0:4 `p`))\n"
    )
  , ( "o.)p.q"
    , "(0:2-0:3) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop 0:0-0:6 (prop 0:0-0:4 (var 0:0-0:1 `o`) (name 0:3-0:4 `p`)) (name 0:5-0:6 `q`))\n"
    )
  , ( "o.p).q"
    , "(0:3-0:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(prop 0:0-0:6 (prop 0:0-0:3 (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:5-0:6 `q`))\n"
    )
  , ( "o.p.)q"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop 0:0-0:6 (prop 0:0-0:3 (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:5-0:6 `q`))\n"
    )
  ]
