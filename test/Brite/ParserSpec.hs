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
      (module_, diagnostics) = runDiagnosticWriter (parse (tokenize input))
      actual = L.toStrict $ B.toLazyText $
        (if null diagnostics then "" else
          mconcat (map debugDiagnostic diagnostics) <> B.singleton '\n')
        <> debugModule module_
      input2 = L.toStrict (B.toLazyText (uncurry printSource (moduleTokens module_)))
    in do
      actual `shouldBe` expected
      input2 `shouldBe` input

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
      \(bind err err)\n"
    )
  , ( "let x"
    , "(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let ="
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let y"
    , "(0:5-0:5) We wanted `=` but the file ended.\n\
      \(0:5-0:5) We wanted an expression but the file ended.\n\
      \\n\
      \(bind (var 0:4-0:5 `y`) err)\n"
    )
  , ( "let ;"
    , "(0:4-0:5) We wanted a variable name but we found `;`.\n\
      \(0:4-0:5) We wanted `=` but we found `;`.\n\
      \(0:4-0:5) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let x ="
    , "(0:7-0:7) We wanted an expression but the file ended.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let x y"
    , "(0:6-0:7) We wanted `=` but we found a variable name.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:6-0:7 `y`))\n"
    )
  , ( "let x ;"
    , "(0:6-0:7) We wanted `=` but we found `;`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let = y"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \\n\
      \(bind err (var 0:6-0:7 `y`))\n"
    )
  , ( "let = ;"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind err err)\n"
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
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y; 😈"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \err\n"
    )
  , ( "let x = y 😈"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
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
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y; )"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \err\n"
    )
  , ( "let x = y )"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let 😈 = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind err (var 0:9-0:10 `y`))\n"
    )
  , ( "let x 😈 y;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:9-0:10 `y`))\n"
    )
  , ( "let x = 😈;"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let 😈 y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:9) We wanted `=` but we found `;`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var 0:7-0:8 `y`) err)\n"
    )
  , ( "let 😈 =;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let x 😈;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let = 😈;"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let 😈;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted `=` but we found `;`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let ) = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind err (var 0:8-0:9 `y`))\n"
    )
  , ( "let x ) y;"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = );"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let ) y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:8) We wanted `=` but we found `;`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var 0:6-0:7 `y`) err)\n"
    )
  , ( "let ) =;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let x );"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( "let = );"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \(0:6-0:7) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind err err)\n"
    )
  , ( "let );"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:5-0:6) We wanted `=` but we found `;`.\n\
      \(0:5-0:6) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind err err)\n"
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
      \(var 0:0-0:1 `x`)\n\
      \err\n"
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
  , ( "("
    , "(0:1-0:1) We wanted an expression but the file ended.\n\
      \(0:1-0:1) We wanted `)` but the file ended.\n\
      \\n\
      \(wrap err)\n"
    )
  , ( "(x"
    , "(0:2-0:2) We wanted `)` but the file ended.\n\
      \\n\
      \(wrap (var 0:1-0:2 `x`))\n"
    )
  , ( "()"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(wrap err)\n"
    )
  , ( "(x)"
    , "(wrap (var 0:1-0:2 `x`))\n"
    )
  , ( "x)"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `x`)\n"
    )
  , ( "(x;"
    , "(0:2-0:3) We wanted `)` but we found `;`.\n\
      \\n\
      \(wrap (var 0:1-0:2 `x`))\n"
    )
  , ( "let x = (y);"
    , "(bind (var 0:4-0:5 `x`) (wrap (var 0:9-0:10 `y`)))\n"
    )
  , ( "let x = (y;"
    , "(0:10-0:11) We wanted `)` but we found `;`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (wrap (var 0:9-0:10 `y`)))\n"
    )
  , ( "let x = y; let x = y;"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y;"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; let x = y;"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n\
      \(bind (var 0:37-0:38 `x`) (var 0:41-0:42 `y`))\n"
    )
  , ( "let x = y let x = y"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n"
    )
  , ( "let x = y let x = y let x = y"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
    )
  , ( "let x = y let x = y let x = y let x = y"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n\
      \(bind (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))\n"
    )
  , ( "let x = y\nlet x = y\n"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n"
    )
  , ( "let x = y\nlet x = y\nlet x = y\n"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
      \(bind (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))\n"
    )
  , ( "let x = y\nlet x = y\nlet x = y\nlet x = y\n"
    , "(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
      \(bind (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))\n\
      \(bind (var 3:4-3:5 `x`) (var 3:8-3:9 `y`))\n"
      )
  , ( "😈 let x = y; let x = y; let x = y;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n\
      \(bind (var 0:18-0:19 `x`) (var 0:22-0:23 `y`))\n\
      \(bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
    )
  , ( "let x = y; 😈 let x = y; let x = y;"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:18-0:19 `x`) (var 0:22-0:23 `y`))\n\
      \(bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
    )
  , ( "let x = y; let x = y; 😈 let x = y;"
    , "(0:22-0:24) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; 😈"
    , "(0:33-0:35) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n\
      \err\n"
    )
  , ( "😈 let x = y let x = y let x = y"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n\
      \(bind (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
    )
  , ( "let x = y 😈 let x = y let x = y"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
    )
  , ( "let x = y let x = y 😈 let x = y"
    , "(0:20-0:22) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind (var 0:27-0:28 `x`) (var 0:31-0:32 `y`))\n"
    )
  , ( "let x = y let x = y let x = y 😈"
    , "(0:30-0:32) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
    )
  , ( ") let x = y; let x = y; let x = y;"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:6-0:7 `x`) (var 0:10-0:11 `y`))\n\
      \(bind (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind (var 0:28-0:29 `x`) (var 0:32-0:33 `y`))\n"
    )
  , ( "let x = y; ) let x = y; let x = y;"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:17-0:18 `x`) (var 0:21-0:22 `y`))\n\
      \(bind (var 0:28-0:29 `x`) (var 0:32-0:33 `y`))\n"
    )
  , ( "let x = y; let x = y; ) let x = y;"
    , "(0:22-0:23) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind (var 0:28-0:29 `x`) (var 0:32-0:33 `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; )"
    , "(0:33-0:34) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n\
      \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n\
      \err\n"
    )
  , ( ") let x = y let x = y let x = y"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:6-0:7 `x`) (var 0:10-0:11 `y`))\n\
      \(bind (var 0:16-0:17 `x`) (var 0:20-0:21 `y`))\n\
      \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "let x = y ) let x = y let x = y"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:16-0:17 `x`) (var 0:20-0:21 `y`))\n\
      \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "let x = y let x = y ) let x = y"
    , "(0:20-0:21) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind (var 0:26-0:27 `x`) (var 0:30-0:31 `y`))\n"
    )
  , ( "let x = y let x = y let x = y )"
    , "(0:30-0:31) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(bind (var 0:14-0:15 `x`) (var 0:18-0:19 `y`))\n\
      \(bind (var 0:24-0:25 `x`) (var 0:28-0:29 `y`))\n"
    )
  , ( "do {}"
    , "(do block)\n"
    )
  , ( "do { "
    , "(0:5-0:5) We wanted `}` but the file ended.\n\
      \\n\
      \(do block)\n"
    )
  , ( "do }"
    , "(0:3-0:4) We wanted `{` but we found `}`.\n\
      \\n\
      \(do block)\n"
    )
  , ( "do } do }"
    , "(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:8-0:9) We wanted `{` but we found `}`.\n\
      \\n\
      \(do block)\n\
      \(do block)\n"
    )
  , ( "do"
    , "(0:2-0:2) We wanted `{` but the file ended.\n\
      \(0:2-0:2) We wanted `}` but the file ended.\n\
      \\n\
      \(do block)\n"
    )
  , ( "do do"
    , "(0:3-0:5) We wanted `{` but we found `do`.\n\
      \(0:5-0:5) We wanted `{` but the file ended.\n\
      \(0:5-0:5) We wanted `}` but the file ended.\n\
      \(0:5-0:5) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (do block)))\n"
    )
  , ( "do { let x = y; }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))))\n"
    )
  , ( "do { let x = y; "
    , "(0:16-0:16) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))))\n"
    )
  , ( "do let x = y; }"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))))\n"
    )
  , ( "do let x = y }"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))))\n"
    )
  , ( "do let x = y;"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \(0:13-0:13) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))))\n"
    )
  , ( "do let x = y"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \(0:12-0:12) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))))\n"
    )
  , ( "let x = (do {);"
    , "(0:13-0:14) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (wrap (do block)))\n"
    )
  , ( "let x = (do { let y = z; );"
    , "(0:25-0:26) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (wrap (do (block\n\
      \  (bind (var 0:18-0:19 `y`) (var 0:22-0:23 `z`))))))\n"
    )
  , ( "let x = (do);"
    , "(0:11-0:12) We wanted `{` but we found `)`.\n\
      \(0:11-0:12) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (wrap (do block)))\n"
    )
  , ( "let x = (do let y = z; );"
    , "(0:12-0:15) We wanted `{` but we found `let`.\n\
      \(0:23-0:24) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (wrap (do (block\n\
      \  (bind (var 0:16-0:17 `y`) (var 0:20-0:21 `z`))))))\n"
    )
  , ( "let x = (do { let y = z );"
    , "(0:24-0:25) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (wrap (do (block\n\
      \  (bind (var 0:18-0:19 `y`) (var 0:22-0:23 `z`))))))\n"
    )
  , ( "let x = (do { let y = );"
    , "(0:22-0:23) We wanted an expression but we found `)`.\n\
      \(0:22-0:23) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (wrap (do (block\n\
      \  (bind (var 0:18-0:19 `y`) err)))))\n"
    )
  , ( "do { let x = y; }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))))\n"
    )
  , ( "do { let x = y }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; let x = y; }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))\n\
      \  (bind (var 0:42-0:43 `x`) (var 0:46-0:47 `y`))))\n"
    )
  , ( "do { let x = y let x = y }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y let x = y }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))\n\
      \  (bind (var 0:39-0:40 `x`) (var 0:43-0:44 `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\n }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\nlet x = y\n }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
      \  (bind (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\nlet x = y\nlet x = y\n }"
    , "(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 1:4-1:5 `x`) (var 1:8-1:9 `y`))\n\
      \  (bind (var 2:4-2:5 `x`) (var 2:8-2:9 `y`))\n\
      \  (bind (var 3:4-3:5 `x`) (var 3:8-3:9 `y`))))\n"
    )
  , ( "do { 😈 let x = y; let x = y; let x = y; }"
    , "(0:5-0:7) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:12-0:13 `x`) (var 0:16-0:17 `y`))\n\
      \  (bind (var 0:23-0:24 `x`) (var 0:27-0:28 `y`))\n\
      \  (bind (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))))\n"
    )
  , ( "do { let x = y; 😈 let x = y; let x = y; }"
    , "(0:16-0:18) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:23-0:24 `x`) (var 0:27-0:28 `y`))\n\
      \  (bind (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; 😈 let x = y; }"
    , "(0:27-0:29) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; 😈 }"
    , "(0:38-0:40) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))\n\
      \  err))\n"
    )
  , ( "do { 😈 let x = y let x = y let x = y }"
    , "(0:5-0:7) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:12-0:13 `x`) (var 0:16-0:17 `y`))\n\
      \  (bind (var 0:22-0:23 `x`) (var 0:26-0:27 `y`))\n\
      \  (bind (var 0:32-0:33 `x`) (var 0:36-0:37 `y`))))\n"
    )
  , ( "do { let x = y 😈 let x = y let x = y }"
    , "(0:15-0:17) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:22-0:23 `x`) (var 0:26-0:27 `y`))\n\
      \  (bind (var 0:32-0:33 `x`) (var 0:36-0:37 `y`))))\n"
    )
  , ( "do { let x = y let x = y 😈 let x = y }"
    , "(0:25-0:27) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind (var 0:32-0:33 `x`) (var 0:36-0:37 `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y 😈 }"
    , "(0:35-0:37) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))))\n"
    )
  , ( "do { ) let x = y; let x = y; let x = y; }"
    , "(0:5-0:6) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:11-0:12 `x`) (var 0:15-0:16 `y`))\n\
      \  (bind (var 0:22-0:23 `x`) (var 0:26-0:27 `y`))\n\
      \  (bind (var 0:33-0:34 `x`) (var 0:37-0:38 `y`))))\n"
    )
  , ( "do { let x = y; ) let x = y; let x = y; }"
    , "(0:16-0:17) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:22-0:23 `x`) (var 0:26-0:27 `y`))\n\
      \  (bind (var 0:33-0:34 `x`) (var 0:37-0:38 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; ) let x = y; }"
    , "(0:27-0:28) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind (var 0:33-0:34 `x`) (var 0:37-0:38 `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; ) }"
    , "(0:38-0:39) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))\n\
      \  err))\n"
    )
  , ( "do { ) let x = y let x = y let x = y }"
    , "(0:5-0:6) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:11-0:12 `x`) (var 0:15-0:16 `y`))\n\
      \  (bind (var 0:21-0:22 `x`) (var 0:25-0:26 `y`))\n\
      \  (bind (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { let x = y ) let x = y let x = y }"
    , "(0:15-0:16) We wanted an expression but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:21-0:22 `x`) (var 0:25-0:26 `y`))\n\
      \  (bind (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { let x = y let x = y ) let x = y }"
    , "(0:25-0:26) We wanted an expression but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind (var 0:31-0:32 `x`) (var 0:35-0:36 `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y ) }"
    , "(0:35-0:36) We wanted an expression but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var 0:9-0:10 `x`) (var 0:13-0:14 `y`))\n\
      \  (bind (var 0:19-0:20 `x`) (var 0:23-0:24 `y`))\n\
      \  (bind (var 0:29-0:30 `x`) (var 0:33-0:34 `y`))))\n"
    )
  , ( "let x = ) let x = )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:18-0:19) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n\
      \(bind (var 0:14-0:15 `x`) err)\n"
    )
  , ( "let x = ) )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n"
    )
  , ( ") let x = )"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:6-0:7 `x`) err)\n"
    )
  , ( "let x = ) ) let x = )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \(0:20-0:21) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n\
      \(bind (var 0:16-0:17 `x`) err)\n"
    )
  , ( "do do 😈"
    , "(0:3-0:5) We wanted `{` but we found `do`.\n\
      \(0:6-0:8) We wanted `{` but we found `😈`.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (do block)))\n"
    )
  , ( "do do )"
    , "(0:3-0:5) We wanted `{` but we found `do`.\n\
      \(0:6-0:7) We wanted `{` but we found `)`.\n\
      \(0:7-0:7) We wanted `}` but the file ended.\n\
      \(0:7-0:7) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (do block)))\n"
    )
  , ( "if x {}"
    , "(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "if x { y }"
    , "(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block\n\
      \    (var 0:7-0:8 `y`)))\n"
    )
  , ( "if x {} else {}"
    , "(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { y } else {}"
    , "(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block\n\
      \    (var 0:7-0:8 `y`))\n\
      \  block)\n"
    )
  , ( "if x {} else { y }"
    , "(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  (block\n\
      \    (var 0:15-0:16 `y`)))\n"
    )
  , ( "if x { y } else { z }"
    , "(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block\n\
      \    (var 0:7-0:8 `y`))\n\
      \  (block\n\
      \    (var 0:18-0:19 `z`)))\n"
    )
  , ( "if {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n"
    )
  , ( "if x }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "if x {"
    , "(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "if x"
    , "(0:4-0:4) We wanted `{` but the file ended.\n\
      \(0:4-0:4) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "if {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:4-0:4) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n"
    )
  , ( "if }"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n"
    )
  , ( "if {} else {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x } else {}"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { else {}"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x else {}"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if { else {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if } else {}"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if {} {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:6-0:7) We wanted `else` but we found `{`.\n\
      \(0:7-0:8) We wanted `else` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x } {}"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:7-0:8) We wanted `else` but we found `{`.\n\
      \(0:8-0:9) We wanted `else` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x { {}"
    , "(0:7-0:8) We wanted a statement but we found `{`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block\n\
      \    err))\n"
    )
  , ( "if { {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:6) We wanted a statement but we found `{`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  (block\n\
      \    err))\n"
    )
  , ( "if } {}"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:5-0:6) We wanted `else` but we found `{`.\n\
      \(0:6-0:7) We wanted `else` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if {} else }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:11-0:12) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x } else }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:12-0:13) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { else }"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:12-0:13) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x else }"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:10-0:11) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if { else }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:10-0:11) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if } else }"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:10-0:11) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if {} else {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:12-0:12) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x } else {"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:13-0:13) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { else {"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:13-0:13) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x else {"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if { else {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if } else {"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if {} else"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:10-0:10) We wanted `{` but the file ended.\n\
      \(0:10-0:10) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x } else"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:11-0:11) We wanted `{` but the file ended.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { else"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:11-0:11) We wanted `{` but the file ended.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x else"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:9-0:9) We wanted `{` but the file ended.\n\
      \(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if { else"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:9-0:9) We wanted `{` but the file ended.\n\
      \(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if } else"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:9-0:9) We wanted `{` but the file ended.\n\
      \(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if {} {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:6-0:7) We wanted `else` but we found `{`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x } {"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:7-0:8) We wanted `else` but we found `{`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x { {"
    , "(0:7-0:8) We wanted a statement but we found `{`.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  (block\n\
      \    err))\n"
    )
  , ( "if x {"
    , "(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "if { {"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:6) We wanted a statement but we found `{`.\n\
      \(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  (block\n\
      \    err))\n"
    )
  , ( "if } {"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:5-0:6) We wanted `else` but we found `{`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if {} }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:6-0:7) We wanted `else` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x } }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:7-0:8) We wanted `else` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x { }"
    , "(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "if x }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "if { }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n"
    )
  , ( "if } }"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:5-0:6) We wanted `else` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block\n\
      \  err)\n"
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
      \(bind (var 0:10-0:11 `x`) (var 0:14-0:15 `y`))\n"
    )
  , ( "let ) ) x = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:6-0:7) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (var 0:8-0:9 `x`) (var 0:12-0:13 `y`))\n"
    )
  , ( "😈 😈"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \(0:3-0:5) We wanted a statement but we found `😈`.\n\
      \\n\
      \err\n"
    )
  , ( ") )"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \(0:2-0:3) We wanted a statement but we found `)`.\n\
      \\n\
      \err\n"
    )
  , ( "if x {} { let y = z }"
    , "(0:8-0:9) We wanted `else` but we found `{`.\n\
      \(0:20-0:21) We wanted an expression but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  err)\n\
      \(bind (var 0:14-0:15 `y`) (var 0:18-0:19 `z`))\n"
    )
  , ( "o.p"
    , "(prop (var 0:0-0:1 `o`) (name 0:2-0:3 `p`))\n"
    )
  , ( "o.p.q"
    , "(prop (prop (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:4-0:5 `q`))\n"
    )
  , ( "o."
    , "(0:2-0:2) We wanted a variable name but the file ended.\n\
      \\n\
      \(prop (var 0:0-0:1 `o`) err)\n"
    )
  , ( "o.p."
    , "(0:4-0:4) We wanted a variable name but the file ended.\n\
      \\n\
      \(prop (prop (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) err)\n"
    )
  , ( "o..p"
    , "(0:2-0:3) We wanted a variable name but we found `.`.\n\
      \\n\
      \(prop (prop (var 0:0-0:1 `o`) err) (name 0:3-0:4 `p`))\n"
    )
  , ( "o p"
    , "(var 0:0-0:1 `o`)\n\
      \(var 0:2-0:3 `p`)\n"
    )
  , ( "o😈.p"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \\n\
      \(prop (var 0:0-0:1 `o`) (name 0:4-0:5 `p`))\n"
    )
  , ( "o.😈p"
    , "(0:2-0:4) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop (var 0:0-0:1 `o`) (name 0:4-0:5 `p`))\n"
    )
  , ( "o.😈p.q"
    , "(0:2-0:4) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop (prop (var 0:0-0:1 `o`) (name 0:4-0:5 `p`)) (name 0:6-0:7 `q`))\n"
    )
  , ( "o.p😈.q"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(prop (prop (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:6-0:7 `q`))\n"
    )
  , ( "o.p.😈q"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop (prop (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:6-0:7 `q`))\n"
    )
  , ( "o).p"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(prop (var 0:0-0:1 `o`) (name 0:3-0:4 `p`))\n"
    )
  , ( "o.)p"
    , "(0:2-0:3) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop (var 0:0-0:1 `o`) (name 0:3-0:4 `p`))\n"
    )
  , ( "o.)p.q"
    , "(0:2-0:3) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop (prop (var 0:0-0:1 `o`) (name 0:3-0:4 `p`)) (name 0:5-0:6 `q`))\n"
    )
  , ( "o.p).q"
    , "(0:3-0:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(prop (prop (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:5-0:6 `q`))\n"
    )
  , ( "o.p.)q"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop (prop (var 0:0-0:1 `o`) (name 0:2-0:3 `p`)) (name 0:5-0:6 `q`))\n"
    )
  , ( "if x {} 😈"
    , "(0:8-0:10) We wanted `else` but we found `😈`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x {} 😈 else {}"
    , "(0:8-0:10) We wanted `else` but we found `😈`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x {} )"
    , "(0:8-0:9) We wanted `else` but we found `)`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x {} ) else {}"
    , "(0:8-0:9) We wanted `else` but we found `)`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if 😈 x {}"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(if\n\
      \  (var 0:6-0:7 `x`)\n\
      \  block)\n"
    )
  , ( "if x 😈 {}"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "if ) x {}"
    , "(0:3-0:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(if\n\
      \  (var 0:5-0:6 `x`)\n\
      \  block)\n"
    )
  , ( "if x ) {}"
    , "(0:5-0:6) We wanted an expression but we found `)`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block)\n"
    )
  , ( "do {}.p"
    , "(prop (do block) (name 0:6-0:7 `p`))\n"
    )
  , ( "if x {}.p"
    , "(prop (if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block) (name 0:8-0:9 `p`))\n"
    )
  , ( "if x {} else {}.p"
    , "(prop (if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block) (name 0:16-0:17 `p`))\n"
    )
  , ( "f()"
    , "(call\n\
      \  (var 0:0-0:1 `f`))\n"
    )
  , ( "f ()"
    , "(call\n\
      \  (var 0:0-0:1 `f`))\n"
    )
  , ( "f😈()"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`))\n"
    )
  , ( "f)()"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`))\n"
    )
  , ( "f\n()"
    , "(1:1-1:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `f`)\n\
      \(wrap err)\n"
    )
  , ( "f;()"
    , "(0:3-0:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `f`)\n\
      \(wrap err)\n"
    )
  , ( "f\n😈()"
    , "(1:0-1:2) We wanted an expression but we found `😈`.\n\
      \(1:3-1:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `f`)\n\
      \(wrap err)\n"
    )
  , ( "f😈\n()"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \(1:1-1:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `f`)\n\
      \(wrap err)\n"
    )
  , ( "f\n)()"
    , "(1:0-1:1) We wanted an expression but we found `)`.\n\
      \(1:2-1:3) We wanted an expression but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `f`)\n\
      \(wrap err)\n"
    )
  , ( "f)\n()"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \(1:1-1:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(var 0:0-0:1 `f`)\n\
      \(wrap err)\n"
    )
  , ( "f(😈)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err)\n"
    )
  , ( "f("
    , "(0:2-0:2) We wanted `)` but the file ended.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`))\n"
    )
  , ( "😈.p"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \(0:2-0:3) We wanted a statement but we found `.`.\n\
      \\n\
      \(var 0:3-0:4 `p`)\n"
    )
  , ( "(😈.p)"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \(0:3-0:4) We wanted `)` but we found `.`.\n\
      \(0:5-0:6) We wanted an expression but we found `)`.\n\
      \\n\
      \(prop (wrap err) (name 0:4-0:5 `p`))\n"
    )
  , ( "let 😈) = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind err (var 0:10-0:11 `y`))\n"
    )
  , ( "let )😈 = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:5-0:7) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind err (var 0:10-0:11 `y`))\n"
    )
  , ( "let 😈)x = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let )😈x = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:5-0:7) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (var 0:7-0:8 `x`) (var 0:11-0:12 `y`))\n"
    )
  , ( "let x = y 😈);"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \(0:12-0:13) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( "let x = y )😈;"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \(0:11-0:13) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n"
    )
  , ( ")😈 let x = y;"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \(0:1-0:3) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:8-0:9 `x`) (var 0:12-0:13 `y`))\n"
    )
  , ( "😈) let x = y;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \(0:2-0:3) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:8-0:9 `x`) (var 0:12-0:13 `y`))\n"
    )
  , ( "let x = y; )😈"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \(0:12-0:14) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \err\n"
    )
  , ( "let x = y; 😈)"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \(0:13-0:14) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \err\n"
    )
  , ( ")😈"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \(0:1-0:3) We wanted a statement but we found `😈`.\n\
      \\n\
      \err\n"
    )
  , ( "😈)"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \(0:2-0:3) We wanted a statement but we found `)`.\n\
      \\n\
      \err\n"
    )
  , ( "let x = 🐶🐱 y;"
    , "(0:8-0:10) We wanted an expression but we found `🐶`.\n\
      \(0:10-0:12) We wanted an expression but we found `🐱`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:13-0:14 `y`))\n"
    )
  , ( "if x {} 🐶🐱 else {}"
    , "(0:8-0:10) We wanted `else` but we found `🐶`.\n\
      \(0:10-0:12) We wanted `else` but we found `🐱`.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x {} 🐶🐱 else {"
    , "(0:8-0:10) We wanted `else` but we found `🐶`.\n\
      \(0:10-0:12) We wanted `else` but we found `🐱`.\n\
      \(0:19-0:19) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var 0:3-0:4 `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "let x = y; do { let x = y; 😈 let x = y; } let x = y;"
    , "(0:27-0:29) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) (var 0:8-0:9 `y`))\n\
      \(do (block\n\
      \  (bind (var 0:20-0:21 `x`) (var 0:24-0:25 `y`))\n\
      \  (bind (var 0:34-0:35 `x`) (var 0:38-0:39 `y`))))\n\
      \(bind (var 0:47-0:48 `x`) (var 0:51-0:52 `y`))\n"
    )
  , ( "let x = 😈 let x = y"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var 0:4-0:5 `x`) err)\n\
      \(bind (var 0:15-0:16 `x`) (var 0:19-0:20 `y`))\n"
    )
  , ( "f(x)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `x`))\n"
    )
  , ( "f\n(x)"
    , "(var 0:0-0:1 `f`)\n\
      \(wrap (var 1:1-1:2 `x`))\n"
    )
  , ( "f;(x)"
    , "(var 0:0-0:1 `f`)\n\
      \(wrap (var 0:3-0:4 `x`))\n"
    )
  , ( "f(a)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a😈)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a})"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a😈})"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a}😈)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a😈,)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a},)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a😈},)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a}😈,)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a😈, b)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:7-0:8 `b`))\n"
    )
  , ( "f(a}, b)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:6-0:7 `b`))\n"
    )
  , ( "f(a😈}, b)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:8-0:9 `b`))\n"
    )
  , ( "f(a}😈, b)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:8-0:9 `b`))\n"
    )
  , ( "f(a😈, b.)" -- NOTE: `b.` is used here and below to enter “yield” mode for the expression.
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:9-0:10) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:7-0:8 `b`) err))\n"
    )
  , ( "f(a}, b.)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:8-0:9) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:6-0:7 `b`) err))\n"
    )
  , ( "f(a😈}, b.)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:10-0:11) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:8-0:9 `b`) err))\n"
    )
  , ( "f(a}😈, b.)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:8-0:9 `b`) err))\n"
    )
  , ( "f(a😈 b)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:6-0:7 `b`))\n"
    )
  , ( "f(a} b)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:5-0:6) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a😈} b)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:7-0:8 `b`))\n"
    )
  , ( "f(a}😈 b)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:7-0:8 `b`))\n"
    )
  , ( "f(a😈 b.)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \(0:8-0:9) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:6-0:7 `b`) err))\n"
    )
  , ( "f(a} b.)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:5-0:6) We wanted `,` but we found a variable name.\n\
      \(0:7-0:8) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:5-0:6 `b`) err))\n"
    )
  , ( "f(a😈} b.)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \(0:9-0:10) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:7-0:8 `b`) err))\n"
    )
  , ( "f(a}😈 b.)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \(0:9-0:10) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:7-0:8 `b`) err))\n"
    )
  , ( "f(a, b)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b😈)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b})"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b}😈)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b😈})"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b😈,)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b},)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b}😈,)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b😈},)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b😈, c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, b}, c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a, b}😈, c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, b😈}, c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, b😈, c.)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:12-0:13) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (prop (var 0:10-0:11 `c`) err))\n"
    )
  , ( "f(a, b}, c.)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:11-0:12) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (prop (var 0:9-0:10 `c`) err))\n"
    )
  , ( "f(a, b}😈, c.)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \(0:13-0:14) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (prop (var 0:11-0:12 `c`) err))\n"
    )
  , ( "f(a, b😈}, c.)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:13-0:14) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (prop (var 0:11-0:12 `c`) err))\n"
    )
  , ( "f(a, b😈 c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:9-0:10) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a, b} c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:8-0:9) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  , ( "f(a, b}😈 c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, b😈} c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, b😈 c.)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:9-0:10) We wanted `,` but we found a variable name.\n\
      \(0:11-0:12) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (prop (var 0:9-0:10 `c`) err))\n"
    )
  , ( "f(a, b} c.)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:8-0:9) We wanted `,` but we found a variable name.\n\
      \(0:10-0:11) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (prop (var 0:8-0:9 `c`) err))\n"
    )
    , ( "f(a, b}😈 c.)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \(0:12-0:13) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (prop (var 0:10-0:11 `c`) err))\n"
    )
  , ( "f(a, b😈} c.)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \(0:12-0:13) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (prop (var 0:10-0:11 `c`) err))\n"
    )
  , ( "f(a.)"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (prop (var 0:2-0:3 `a`) err))\n"
    )
  , ( "f(a.,)"
    , "(0:4-0:5) We wanted a variable name but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (prop (var 0:2-0:3 `a`) err))\n"
    )
  , ( "f(a., b)"
    , "(0:4-0:5) We wanted a variable name but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (prop (var 0:2-0:3 `a`) err)\n\
      \  (var 0:6-0:7 `b`))\n"
    )
  , ( "f(a. (b))"
    , "(0:5-0:6) We wanted a variable name but we found `(`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (call\n\
      \    (prop (var 0:2-0:3 `a`) err)\n\
      \    (var 0:6-0:7 `b`)))\n"
    )
  , ( "f(a. (b).)"
    , "(0:5-0:6) We wanted a variable name but we found `(`.\n\
      \(0:9-0:10) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (prop (call\n\
      \    (prop (var 0:2-0:3 `a`) err)\n\
      \    (var 0:6-0:7 `b`)) err))\n"
    )
  , ( "f(a.😈)"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (prop (var 0:2-0:3 `a`) err))\n"
    )
  , ( "f(a.})"
    , "(0:4-0:5) We wanted a variable name but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (prop (var 0:2-0:3 `a`) err))\n"
    )
  , ( "f(a.😈})"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted a variable name but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (prop (var 0:2-0:3 `a`) err))\n"
    )
  , ( "f(a.}😈)"
    , "(0:4-0:5) We wanted a variable name but we found `}`.\n\
      \(0:5-0:7) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (prop (var 0:2-0:3 `a`) err))\n"
    )
  , ( "f(a, b.)"
    , "(0:7-0:8) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:5-0:6 `b`) err))\n"
    )
  , ( "f(a, b.,)"
    , "(0:7-0:8) We wanted a variable name but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:5-0:6 `b`) err))\n"
    )
  , ( "f(a, b., c)"
    , "(0:7-0:8) We wanted a variable name but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:5-0:6 `b`) err)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a, b. (c))"
    , "(0:8-0:9) We wanted a variable name but we found `(`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (call\n\
      \    (prop (var 0:5-0:6 `b`) err)\n\
      \    (var 0:9-0:10 `c`)))\n"
    )
  , ( "f(a, b. (c).)"
    , "(0:8-0:9) We wanted a variable name but we found `(`.\n\
      \(0:12-0:13) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (call\n\
      \    (prop (var 0:5-0:6 `b`) err)\n\
      \    (var 0:9-0:10 `c`)) err))\n"
    )
  , ( "f(a, b.😈)"
    , "(0:7-0:9) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:5-0:6 `b`) err))\n"
    )
  , ( "f(a, b.})"
    , "(0:7-0:8) We wanted a variable name but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:5-0:6 `b`) err))\n"
    )
  , ( "f(a, b.😈})"
    , "(0:7-0:9) We wanted a variable name but we found `😈`.\n\
      \(0:9-0:10) We wanted a variable name but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:5-0:6 `b`) err))\n"
    )
  , ( "f(a, b.}😈)"
    , "(0:7-0:8) We wanted a variable name but we found `}`.\n\
      \(0:8-0:10) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (prop (var 0:5-0:6 `b`) err))\n"
    )
  , ( "f(😈)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err)\n"
    )
  , ( "f(})"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err)\n"
    )
  , ( "f(😈})"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:4-0:5) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err)\n"
    )
  , ( "f(}😈)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err)\n"
    )
  , ( "f(a, 😈)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err)\n"
    )
  , ( "f(a, })"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err)\n"
    )
  , ( "f(a, 😈})"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err)\n"
    )
  , ( "f(a, }😈)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err)\n"
    )
  , ( "f(a b)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:4-0:5 `b`))\n"
    )
  , ( "f(a, b c)"
    , "(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:7-0:8 `c`))\n"
    )
  , ( "f(a b, c)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:4-0:5 `b`)\n\
      \  (var 0:7-0:8 `c`))\n"
    )
  , ( "f(a b c)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:4-0:5 `b`)\n\
      \  (var 0:6-0:7 `c`))\n"
    )
  , ( "f(a b, c, d)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:4-0:5 `b`)\n\
      \  (var 0:7-0:8 `c`)\n\
      \  (var 0:10-0:11 `d`))\n"
    )
  , ( "f(a, b c, d)"
    , "(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:7-0:8 `c`)\n\
      \  (var 0:10-0:11 `d`))\n"
    )
  , ( "f(a, b, c d)"
    , "(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`)\n\
      \  (var 0:10-0:11 `d`))\n"
    )
  , ( "f(a b c, d)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:4-0:5 `b`)\n\
      \  (var 0:6-0:7 `c`)\n\
      \  (var 0:9-0:10 `d`))\n"
    )
  , ( "f(a, b c d)"
    , "(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \(0:9-0:10) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:7-0:8 `c`)\n\
      \  (var 0:9-0:10 `d`))\n"
    )
  , ( "f(a b, c d)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \(0:9-0:10) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:4-0:5 `b`)\n\
      \  (var 0:7-0:8 `c`)\n\
      \  (var 0:9-0:10 `d`))\n"
    )
  , ( "f(a b c d)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \(0:8-0:9) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:4-0:5 `b`)\n\
      \  (var 0:6-0:7 `c`)\n\
      \  (var 0:8-0:9 `d`))\n"
    )
  , ( "f(a,, b)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:6-0:7 `b`))\n"
    )
  , ( "f(a,, b, c)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:6-0:7 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a, b,, c)"
    , "(0:7-0:8) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  err\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a,, b,, c)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \(0:8-0:9) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:6-0:7 `b`)\n\
      \  err\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a,, b, c, d)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:6-0:7 `b`)\n\
      \  (var 0:9-0:10 `c`)\n\
      \  (var 0:12-0:13 `d`))\n"
    )
  , ( "f(a, b,, c, d)"
    , "(0:7-0:8) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  err\n\
      \  (var 0:9-0:10 `c`)\n\
      \  (var 0:12-0:13 `d`))\n"
    )
  , ( "f(a, b, c,, d)"
    , "(0:10-0:11) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`)\n\
      \  err\n\
      \  (var 0:12-0:13 `d`))\n"
    )
  , ( "f(a, b,, c,, d)"
    , "(0:7-0:8) We wanted an expression but we found `,`.\n\
      \(0:11-0:12) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  err\n\
      \  (var 0:9-0:10 `c`)\n\
      \  err\n\
      \  (var 0:13-0:14 `d`))\n"
    )
  , ( "f(a,, b, c,, d)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \(0:11-0:12) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:6-0:7 `b`)\n\
      \  (var 0:9-0:10 `c`)\n\
      \  err\n\
      \  (var 0:13-0:14 `d`))\n"
    )
  , ( "f(a,, b,, c, d)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \(0:8-0:9) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:6-0:7 `b`)\n\
      \  err\n\
      \  (var 0:10-0:11 `c`)\n\
      \  (var 0:13-0:14 `d`))\n"
    )
  , ( "f(a,, b,, c,, d)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \(0:8-0:9) We wanted an expression but we found `,`.\n\
      \(0:12-0:13) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:6-0:7 `b`)\n\
      \  err\n\
      \  (var 0:10-0:11 `c`)\n\
      \  err\n\
      \  (var 0:14-0:15 `d`))\n"
    )
  , ( "f(a, b, c, d,)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`)\n\
      \  (var 0:11-0:12 `d`))\n"
    )
  , ( "f(a, b, c, d,,)"
    , "(0:13-0:14) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`)\n\
      \  (var 0:11-0:12 `d`)\n\
      \  err)\n"
    )
  , ( "f(a, b, c,,)"
    , "(0:10-0:11) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`)\n\
      \  err)\n"
    )
  , ( "f(a, b,,)"
    , "(0:7-0:8) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  err)\n"
    )
  , ( "f(a,,)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err)\n"
    )
  , ( "f(a, 😈, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(😈, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err\n\
      \  (var 0:6-0:7 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a, b, 😈)"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  err)\n"
    )
  , ( "f(a, }, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  , ( "f(}, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  , ( "f(a, b, })"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  err)\n"
    )
  , ( "f(a, 😈}, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(😈}, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:4-0:5) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err\n\
      \  (var 0:7-0:8 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, b, 😈})"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  err)\n"
    )
  , ( "f(a, }😈, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  err\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(}😈, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err\n\
      \  (var 0:7-0:8 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, b, }😈)"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:9-0:11) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  err)\n"
    )
  , ( "f(a, b😈, c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a😈, b, c)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:7-0:8 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, b, c😈)"
    , "(0:9-0:11) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  , ( "f(a, b}, c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a}, b, c)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:6-0:7 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a, b, c})"
    , "(0:9-0:10) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  , ( "f(a, b😈}, c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a😈}, b, c)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:8-0:9 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, b, c😈})"
    , "(0:9-0:11) We wanted an expression but we found `😈`.\n\
      \(0:11-0:12) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  , ( "f(a, b}😈, c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a}😈, b, c)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:8-0:9 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, b, c}😈)"
    , "(0:9-0:10) We wanted an expression but we found `}`.\n\
      \(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  , ( "f(a, 😈b, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:7-0:8 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(😈a, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:4-0:5 `a`)\n\
      \  (var 0:7-0:8 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, b, 😈c)"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, }b, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:6-0:7 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(}a, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:3-0:4 `a`)\n\
      \  (var 0:6-0:7 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a, b, }c)"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(a, 😈}b, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:8-0:9 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(😈}a, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:4-0:5) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:5-0:6 `a`)\n\
      \  (var 0:8-0:9 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, b, 😈}c)"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, }😈b, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:8-0:9 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(}😈a, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:5-0:6 `a`)\n\
      \  (var 0:8-0:9 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, b, }😈c)"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:9-0:11) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, 😈b}, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:7-0:8 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(😈a}, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:4-0:5 `a`)\n\
      \  (var 0:8-0:9 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, b, 😈c})"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \(0:11-0:12) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a, }b😈, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:6-0:7 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(}a😈, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:3-0:4 `a`)\n\
      \  (var 0:8-0:9 `b`)\n\
      \  (var 0:11-0:12 `c`))\n"
    )
  , ( "f(a, b, }c😈)"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:9-0:10 `c`))\n"
    )
  , ( "f(, a)"
    , "(0:2-0:3) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err\n\
      \  (var 0:4-0:5 `a`))\n"
    )
  , ( "f(, a, b)"
    , "(0:2-0:3) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err\n\
      \  (var 0:4-0:5 `a`)\n\
      \  (var 0:7-0:8 `b`))\n"
    )
  , ( "f(, a, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err\n\
      \  (var 0:4-0:5 `a`)\n\
      \  (var 0:7-0:8 `b`)\n\
      \  (var 0:10-0:11 `c`))\n"
    )
  , ( "f(a,)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(,)"
    , "(0:2-0:3) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  err)\n"
    )
  , ( "f()"
    , "(call\n\
      \  (var 0:0-0:1 `f`))\n"
    )
  , ( "f(a)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a,)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`))\n"
    )
  , ( "f(a, b)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b,)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`))\n"
    )
  , ( "f(a, b, c)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  , ( "f(a, b, c,)"
    , "(call\n\
      \  (var 0:0-0:1 `f`)\n\
      \  (var 0:2-0:3 `a`)\n\
      \  (var 0:5-0:6 `b`)\n\
      \  (var 0:8-0:9 `c`))\n"
    )
  ]
