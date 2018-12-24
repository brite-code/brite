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
    , "(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y"
    , "(bind (var `x`) (var `y`))\n"
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
      \(bind (var `x`) err)\n"
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
      \(bind (var `y`) err)\n"
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
      \(bind (var `x`) err)\n"
    )
  , ( "let x y"
    , "(0:6-0:7) We wanted `=` but we found a variable name.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x ;"
    , "(0:6-0:7) We wanted `=` but we found `;`.\n\
      \(0:6-0:7) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var `x`) err)\n"
    )
  , ( "let = y"
    , "(0:4-0:5) We wanted a variable name but we found `=`.\n\
      \\n\
      \(bind err (var `y`))\n"
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
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let 😈 x = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x 😈 = y;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = 😈 y;"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y 😈;"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; 😈"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \err\n"
    )
  , ( "let x = y 😈"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( ") let x = y;"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let ) x = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x ) = y;"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
      )
  , ( "let x = ) y;"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y );"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; )"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \err\n"
    )
  , ( "let x = y )"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let 😈 = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind err (var `y`))\n"
    )
  , ( "let x 😈 y;"
    , "(0:6-0:8) We wanted `=` but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = 😈;"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) err)\n"
    )
  , ( "let 😈 y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:8-0:9) We wanted `=` but we found `;`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var `y`) err)\n"
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
      \(bind (var `x`) err)\n"
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
      \(bind err (var `y`))\n"
    )
  , ( "let x ) y;"
    , "(0:6-0:7) We wanted `=` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = );"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) err)\n"
    )
  , ( "let ) y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:7-0:8) We wanted `=` but we found `;`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var `y`) err)\n"
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
      \(bind (var `x`) err)\n"
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
    , "(var `x`)\n"
    )
  , ( "x;"
    , "(var `x`)\n"
    )
  , ( "😈 x"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(var `x`)\n"
    )
  , ( "x 😈"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(var `x`)\n"
    )
  , ( "😈 x;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(var `x`)\n"
    )
  , ( "x 😈;"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(var `x`)\n"
    )
  , ( "x; 😈"
    , "(0:3-0:5) We wanted a statement but we found `😈`.\n\
      \\n\
      \(var `x`)\n\
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
    , "(bool true)\n"
    )
  , ( "false"
    , "(bool false)\n"
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
      \(wrap (var `x`))\n"
    )
  , ( "()"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(wrap err)\n"
    )
  , ( "(x)"
    , "(wrap (var `x`))\n"
    )
  , ( "x)"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(var `x`)\n"
    )
  , ( "(x;"
    , "(0:2-0:3) We wanted `)` but we found `;`.\n\
      \\n\
      \(wrap (var `x`))\n"
    )
  , ( "let x = (y);"
    , "(bind (var `x`) (wrap (var `y`)))\n"
    )
  , ( "let x = (y;"
    , "(0:10-0:11) We wanted `)` but we found `;`.\n\
      \\n\
      \(bind (var `x`) (wrap (var `y`)))\n"
    )
  , ( "let x = y; let x = y;"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y;"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; let x = y;"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y let x = y"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y let x = y let x = y"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y let x = y let x = y let x = y"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y\nlet x = y\n"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y\nlet x = y\nlet x = y\n"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y\nlet x = y\nlet x = y\nlet x = y\n"
    , "(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
      )
  , ( "😈 let x = y; let x = y; let x = y;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; 😈 let x = y; let x = y;"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; let x = y; 😈 let x = y;"
    , "(0:22-0:24) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; 😈"
    , "(0:33-0:35) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \err\n"
    )
  , ( "😈 let x = y let x = y let x = y"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y 😈 let x = y let x = y"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y let x = y 😈 let x = y"
    , "(0:20-0:22) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y let x = y let x = y 😈"
    , "(0:30-0:32) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( ") let x = y; let x = y; let x = y;"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; ) let x = y; let x = y;"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; let x = y; ) let x = y;"
    , "(0:22-0:23) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; let x = y; let x = y; )"
    , "(0:33-0:34) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \err\n"
    )
  , ( ") let x = y let x = y let x = y"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y ) let x = y let x = y"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y let x = y ) let x = y"
    , "(0:20-0:21) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y let x = y let x = y )"
    , "(0:30-0:31) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n\
      \(bind (var `x`) (var `y`))\n"
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
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; "
    , "(0:16-0:16) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do let x = y; }"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do let x = y }"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do let x = y;"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \(0:13-0:13) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do let x = y"
    , "(0:3-0:6) We wanted `{` but we found `let`.\n\
      \(0:12-0:12) We wanted `}` but the file ended.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "let x = (do {);"
    , "(0:13-0:14) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (wrap (do block)))\n"
    )
  , ( "let x = (do { let y = z; );"
    , "(0:25-0:26) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (wrap (do (block\n\
      \  (bind (var `y`) (var `z`))))))\n"
    )
  , ( "let x = (do);"
    , "(0:11-0:12) We wanted `{` but we found `)`.\n\
      \(0:11-0:12) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (wrap (do block)))\n"
    )
  , ( "let x = (do let y = z; );"
    , "(0:12-0:15) We wanted `{` but we found `let`.\n\
      \(0:23-0:24) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (wrap (do (block\n\
      \  (bind (var `y`) (var `z`))))))\n"
    )
  , ( "let x = (do { let y = z );"
    , "(0:24-0:25) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (wrap (do (block\n\
      \  (bind (var `y`) (var `z`))))))\n"
    )
  , ( "let x = (do { let y = );"
    , "(0:22-0:23) We wanted an expression but we found `)`.\n\
      \(0:22-0:23) We wanted `}` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (wrap (do (block\n\
      \  (bind (var `y`) err)))))\n"
    )
  , ( "do { let x = y; }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; let x = y; }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; let x = y; }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y let x = y }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y let x = y }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\n }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\nlet x = y\n }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y\nlet x = y\nlet x = y\nlet x = y\n }"
    , "(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { 😈 let x = y; let x = y; let x = y; }"
    , "(0:5-0:7) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; 😈 let x = y; let x = y; }"
    , "(0:16-0:18) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; let x = y; 😈 let x = y; }"
    , "(0:27-0:29) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; 😈 }"
    , "(0:38-0:40) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  err))\n"
    )
  , ( "do { 😈 let x = y let x = y let x = y }"
    , "(0:5-0:7) We wanted a statement but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y 😈 let x = y let x = y }"
    , "(0:15-0:17) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y let x = y 😈 let x = y }"
    , "(0:25-0:27) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y 😈 }"
    , "(0:35-0:37) We wanted an expression but we found `😈`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { ) let x = y; let x = y; let x = y; }"
    , "(0:5-0:6) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; ) let x = y; let x = y; }"
    , "(0:16-0:17) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; let x = y; ) let x = y; }"
    , "(0:27-0:28) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y; let x = y; let x = y; ) }"
    , "(0:38-0:39) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  err))\n"
    )
  , ( "do { ) let x = y let x = y let x = y }"
    , "(0:5-0:6) We wanted a statement but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y ) let x = y let x = y }"
    , "(0:15-0:16) We wanted an expression but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y let x = y ) let x = y }"
    , "(0:25-0:26) We wanted an expression but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "do { let x = y let x = y let x = y ) }"
    , "(0:35-0:36) We wanted an expression but we found `)`.\n\
      \\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "let x = ) let x = )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:18-0:19) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) err)\n\
      \(bind (var `x`) err)\n"
    )
  , ( "let x = ) )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) err)\n"
    )
  , ( ") let x = )"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) err)\n"
    )
  , ( "let x = ) ) let x = )"
    , "(0:8-0:9) We wanted an expression but we found `)`.\n\
      \(0:10-0:11) We wanted an expression but we found `)`.\n\
      \(0:20-0:21) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) err)\n\
      \(bind (var `x`) err)\n"
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
      \  (var `x`)\n\
      \  block)\n"
    )
  , ( "if x { y }"
    , "(if\n\
      \  (var `x`)\n\
      \  (block\n\
      \    (var `y`)))\n"
    )
  , ( "if x {} else {}"
    , "(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { y } else {}"
    , "(if\n\
      \  (var `x`)\n\
      \  (block\n\
      \    (var `y`))\n\
      \  block)\n"
    )
  , ( "if x {} else { y }"
    , "(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (block\n\
      \    (var `y`)))\n"
    )
  , ( "if x { y } else { z }"
    , "(if\n\
      \  (var `x`)\n\
      \  (block\n\
      \    (var `y`))\n\
      \  (block\n\
      \    (var `z`)))\n"
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
      \  (var `x`)\n\
      \  block)\n"
    )
  , ( "if x {"
    , "(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block)\n"
    )
  , ( "if x"
    , "(0:4-0:4) We wanted `{` but the file ended.\n\
      \(0:4-0:4) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
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
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { else {}"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x else {}"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
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
      \  (var `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x { {}"
    , "(0:7-0:8) We wanted a statement but we found `{`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
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
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { else }"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:12-0:13) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x else }"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:10-0:11) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
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
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { else {"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:13-0:13) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x else {"
    , "(0:5-0:9) We wanted `{` but we found `else`.\n\
      \(0:5-0:9) We wanted `}` but we found `else`.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
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
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x { else"
    , "(0:7-0:11) We wanted `}` but we found `else`.\n\
      \(0:11-0:11) We wanted `{` but the file ended.\n\
      \(0:11-0:11) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
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
      \  (var `x`)\n\
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
      \  (var `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x { {"
    , "(0:7-0:8) We wanted a statement but we found `{`.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  (block\n\
      \    err))\n"
    )
  , ( "if x {"
    , "(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
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
      \  (var `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x { }"
    , "(if\n\
      \  (var `x`)\n\
      \  block)\n"
    )
  , ( "if x }"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
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
      \(var `x`)\n"
    )
  , ( "x ) ) ;"
    , "(0:2-0:3) We wanted an expression but we found `)`.\n\
      \(0:4-0:5) We wanted an expression but we found `)`.\n\
      \\n\
      \(var `x`)\n"
    )
  , ( "let 😈 😈 x = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:7-0:9) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let ) ) x = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:6-0:7) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
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
      \  (var `x`)\n\
      \  block\n\
      \  err)\n\
      \(bind (var `y`) (var `z`))\n"
    )
  , ( "o.p"
    , "(prop (var `o`) (name `p`))\n"
    )
  , ( "o.p.q"
    , "(prop (prop (var `o`) (name `p`)) (name `q`))\n"
    )
  , ( "o."
    , "(0:2-0:2) We wanted a variable name but the file ended.\n\
      \\n\
      \(prop (var `o`) err)\n"
    )
  , ( "o.p."
    , "(0:4-0:4) We wanted a variable name but the file ended.\n\
      \\n\
      \(prop (prop (var `o`) (name `p`)) err)\n"
    )
  , ( "o..p"
    , "(0:2-0:3) We wanted a variable name but we found `.`.\n\
      \\n\
      \(prop (prop (var `o`) err) (name `p`))\n"
    )
  , ( "o p"
    , "(var `o`)\n\
      \(var `p`)\n"
    )
  , ( "o😈.p"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \\n\
      \(prop (var `o`) (name `p`))\n"
    )
  , ( "o.😈p"
    , "(0:2-0:4) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop (var `o`) (name `p`))\n"
    )
  , ( "o.😈p.q"
    , "(0:2-0:4) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop (prop (var `o`) (name `p`)) (name `q`))\n"
    )
  , ( "o.p😈.q"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(prop (prop (var `o`) (name `p`)) (name `q`))\n"
    )
  , ( "o.p.😈q"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(prop (prop (var `o`) (name `p`)) (name `q`))\n"
    )
  , ( "o).p"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(prop (var `o`) (name `p`))\n"
    )
  , ( "o.)p"
    , "(0:2-0:3) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop (var `o`) (name `p`))\n"
    )
  , ( "o.)p.q"
    , "(0:2-0:3) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop (prop (var `o`) (name `p`)) (name `q`))\n"
    )
  , ( "o.p).q"
    , "(0:3-0:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(prop (prop (var `o`) (name `p`)) (name `q`))\n"
    )
  , ( "o.p.)q"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(prop (prop (var `o`) (name `p`)) (name `q`))\n"
    )
  , ( "if x {} 😈"
    , "(0:8-0:10) We wanted `else` but we found `😈`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x {} 😈 else {}"
    , "(0:8-0:10) We wanted `else` but we found `😈`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x {} )"
    , "(0:8-0:9) We wanted `else` but we found `)`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  err)\n"
    )
  , ( "if x {} ) else {}"
    , "(0:8-0:9) We wanted `else` but we found `)`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if 😈 x {}"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block)\n"
    )
  , ( "if x 😈 {}"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block)\n"
    )
  , ( "if ) x {}"
    , "(0:3-0:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block)\n"
    )
  , ( "if x ) {}"
    , "(0:5-0:6) We wanted an expression but we found `)`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block)\n"
    )
  , ( "do {}.p"
    , "(prop (do block) (name `p`))\n"
    )
  , ( "if x {}.p"
    , "(prop (if\n\
      \  (var `x`)\n\
      \  block) (name `p`))\n"
    )
  , ( "if x {} else {}.p"
    , "(prop (if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block) (name `p`))\n"
    )
  , ( "f()"
    , "(call\n\
      \  (var `f`))\n"
    )
  , ( "f ()"
    , "(call\n\
      \  (var `f`))\n"
    )
  , ( "f😈()"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`))\n"
    )
  , ( "f)()"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`))\n"
    )
  , ( "f\n()"
    , "(1:1-1:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(var `f`)\n\
      \(wrap err)\n"
    )
  , ( "f;()"
    , "(0:3-0:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(var `f`)\n\
      \(wrap err)\n"
    )
  , ( "f\n😈()"
    , "(1:0-1:2) We wanted an expression but we found `😈`.\n\
      \(1:3-1:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(var `f`)\n\
      \(wrap err)\n"
    )
  , ( "f😈\n()"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \(1:1-1:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(var `f`)\n\
      \(wrap err)\n"
    )
  , ( "f\n)()"
    , "(1:0-1:1) We wanted an expression but we found `)`.\n\
      \(1:2-1:3) We wanted an expression but we found `)`.\n\
      \\n\
      \(var `f`)\n\
      \(wrap err)\n"
    )
  , ( "f)\n()"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \(1:1-1:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(var `f`)\n\
      \(wrap err)\n"
    )
  , ( "f(😈)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err)\n"
    )
  , ( "f("
    , "(0:2-0:2) We wanted `)` but the file ended.\n\
      \\n\
      \(call\n\
      \  (var `f`))\n"
    )
  , ( "😈.p"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \(0:2-0:3) We wanted a statement but we found `.`.\n\
      \\n\
      \(var `p`)\n"
    )
  , ( "(😈.p)"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \(0:3-0:4) We wanted `)` but we found `.`.\n\
      \(0:5-0:6) We wanted an expression but we found `)`.\n\
      \\n\
      \(prop (wrap err) (name `p`))\n"
    )
  , ( "let 😈) = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind err (var `y`))\n"
    )
  , ( "let )😈 = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:5-0:7) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind err (var `y`))\n"
    )
  , ( "let 😈)x = y;"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted a variable name but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let )😈x = y;"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \(0:5-0:7) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y 😈);"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \(0:12-0:13) We wanted an expression but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y )😈;"
    , "(0:10-0:11) We wanted an expression but we found `)`.\n\
      \(0:11-0:13) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( ")😈 let x = y;"
    , "(0:0-0:1) We wanted a statement but we found `)`.\n\
      \(0:1-0:3) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "😈) let x = y;"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \(0:2-0:3) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = y; )😈"
    , "(0:11-0:12) We wanted a statement but we found `)`.\n\
      \(0:12-0:14) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \err\n"
    )
  , ( "let x = y; 😈)"
    , "(0:11-0:13) We wanted a statement but we found `😈`.\n\
      \(0:13-0:14) We wanted a statement but we found `)`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
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
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "if x {} 🐶🐱 else {}"
    , "(0:8-0:10) We wanted `else` but we found `🐶`.\n\
      \(0:10-0:12) We wanted `else` but we found `🐱`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "if x {} 🐶🐱 else {"
    , "(0:8-0:10) We wanted `else` but we found `🐶`.\n\
      \(0:10-0:12) We wanted `else` but we found `🐱`.\n\
      \(0:19-0:19) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  block)\n"
    )
  , ( "let x = y; do { let x = y; 😈 let x = y; } let x = y;"
    , "(0:27-0:29) We wanted a statement but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (var `y`))\n\
      \(do (block\n\
      \  (bind (var `x`) (var `y`))\n\
      \  (bind (var `x`) (var `y`))))\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "let x = 😈 let x = y"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(bind (var `x`) err)\n\
      \(bind (var `x`) (var `y`))\n"
    )
  , ( "f(x)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `x`))\n"
    )
  , ( "f\n(x)"
    , "(var `f`)\n\
      \(wrap (var `x`))\n"
    )
  , ( "f;(x)"
    , "(var `f`)\n\
      \(wrap (var `x`))\n"
    )
  , ( "f(a)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a😈)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a})"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a😈})"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a}😈)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a😈,)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a},)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a😈},)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a}😈,)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a😈, b)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a}, b)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a😈}, b)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a}😈, b)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a😈, b.)" -- NOTE: `b.` is used here and below to enter “yield” mode for the expression.
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:9-0:10) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a}, b.)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:8-0:9) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a😈}, b.)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:10-0:11) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a}😈, b.)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a😈 b)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a} b)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:5-0:6) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a😈} b)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a}😈 b)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a😈 b.)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \(0:8-0:9) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a} b.)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:5-0:6) We wanted `,` but we found a variable name.\n\
      \(0:7-0:8) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a😈} b.)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \(0:9-0:10) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a}😈 b.)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \(0:9-0:10) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a, b)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b😈)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b})"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b}😈)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b😈})"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b😈,)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b},)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b}😈,)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b😈},)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b😈, c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b}, c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b}😈, c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b😈}, c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b😈, c.)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:12-0:13) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (prop (var `c`) err))\n"
    )
  , ( "f(a, b}, c.)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:11-0:12) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (prop (var `c`) err))\n"
    )
  , ( "f(a, b}😈, c.)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \(0:13-0:14) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (prop (var `c`) err))\n"
    )
  , ( "f(a, b😈}, c.)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:13-0:14) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (prop (var `c`) err))\n"
    )
  , ( "f(a, b😈 c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:9-0:10) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b} c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:8-0:9) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b}😈 c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b😈} c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b😈 c.)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:9-0:10) We wanted `,` but we found a variable name.\n\
      \(0:11-0:12) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (prop (var `c`) err))\n"
    )
  , ( "f(a, b} c.)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:8-0:9) We wanted `,` but we found a variable name.\n\
      \(0:10-0:11) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (prop (var `c`) err))\n"
    )
    , ( "f(a, b}😈 c.)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \(0:12-0:13) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (prop (var `c`) err))\n"
    )
  , ( "f(a, b😈} c.)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \(0:12-0:13) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (prop (var `c`) err))\n"
    )
  , ( "f(a.)"
    , "(0:4-0:5) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (prop (var `a`) err))\n"
    )
  , ( "f(a.,)"
    , "(0:4-0:5) We wanted a variable name but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (prop (var `a`) err))\n"
    )
  , ( "f(a., b)"
    , "(0:4-0:5) We wanted a variable name but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (prop (var `a`) err)\n\
      \  (var `b`))\n"
    )
  , ( "f(a. (b))"
    , "(0:5-0:6) We wanted a variable name but we found `(`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (call\n\
      \    (prop (var `a`) err)\n\
      \    (var `b`)))\n"
    )
  , ( "f(a. (b).)"
    , "(0:5-0:6) We wanted a variable name but we found `(`.\n\
      \(0:9-0:10) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (prop (call\n\
      \    (prop (var `a`) err)\n\
      \    (var `b`)) err))\n"
    )
  , ( "f(a.😈)"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (prop (var `a`) err))\n"
    )
  , ( "f(a.})"
    , "(0:4-0:5) We wanted a variable name but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (prop (var `a`) err))\n"
    )
  , ( "f(a.😈})"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \(0:6-0:7) We wanted a variable name but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (prop (var `a`) err))\n"
    )
  , ( "f(a.}😈)"
    , "(0:4-0:5) We wanted a variable name but we found `}`.\n\
      \(0:5-0:7) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (prop (var `a`) err))\n"
    )
  , ( "f(a, b.)"
    , "(0:7-0:8) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a, b.,)"
    , "(0:7-0:8) We wanted a variable name but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a, b., c)"
    , "(0:7-0:8) We wanted a variable name but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b. (c))"
    , "(0:8-0:9) We wanted a variable name but we found `(`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (call\n\
      \    (prop (var `b`) err)\n\
      \    (var `c`)))\n"
    )
  , ( "f(a, b. (c).)"
    , "(0:8-0:9) We wanted a variable name but we found `(`.\n\
      \(0:12-0:13) We wanted a variable name but we found `)`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (call\n\
      \    (prop (var `b`) err)\n\
      \    (var `c`)) err))\n"
    )
  , ( "f(a, b.😈)"
    , "(0:7-0:9) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a, b.})"
    , "(0:7-0:8) We wanted a variable name but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a, b.😈})"
    , "(0:7-0:9) We wanted a variable name but we found `😈`.\n\
      \(0:9-0:10) We wanted a variable name but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(a, b.}😈)"
    , "(0:7-0:8) We wanted a variable name but we found `}`.\n\
      \(0:8-0:10) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (prop (var `b`) err))\n"
    )
  , ( "f(😈)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err)\n"
    )
  , ( "f(})"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err)\n"
    )
  , ( "f(😈})"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:4-0:5) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err)\n"
    )
  , ( "f(}😈)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err)\n"
    )
  , ( "f(a, 😈)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err)\n"
    )
  , ( "f(a, })"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err)\n"
    )
  , ( "f(a, 😈})"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err)\n"
    )
  , ( "f(a, }😈)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err)\n"
    )
  , ( "f(a b)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b c)"
    , "(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a b, c)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a b c)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a b, c, d)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a, b c, d)"
    , "(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a, b, c d)"
    , "(0:10-0:11) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a b c, d)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a, b c d)"
    , "(0:7-0:8) We wanted `,` but we found a variable name.\n\
      \(0:9-0:10) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a b, c d)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \(0:9-0:10) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a b c d)"
    , "(0:4-0:5) We wanted `,` but we found a variable name.\n\
      \(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \(0:8-0:9) We wanted `,` but we found a variable name.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a,, b)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `b`))\n"
    )
  , ( "f(a,, b, c)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b,, c)"
    , "(0:7-0:8) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  err\n\
      \  (var `c`))\n"
    )
  , ( "f(a,, b,, c)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \(0:8-0:9) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  err\n\
      \  (var `c`))\n"
    )
  , ( "f(a,, b, c, d)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a, b,, c, d)"
    , "(0:7-0:8) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  err\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a, b, c,, d)"
    , "(0:10-0:11) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  err\n\
      \  (var `d`))\n"
    )
  , ( "f(a, b,, c,, d)"
    , "(0:7-0:8) We wanted an expression but we found `,`.\n\
      \(0:11-0:12) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  err\n\
      \  (var `c`)\n\
      \  err\n\
      \  (var `d`))\n"
    )
  , ( "f(a,, b, c,, d)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \(0:11-0:12) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  err\n\
      \  (var `d`))\n"
    )
  , ( "f(a,, b,, c, d)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \(0:8-0:9) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  err\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a,, b,, c,, d)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \(0:8-0:9) We wanted an expression but we found `,`.\n\
      \(0:12-0:13) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  err\n\
      \  (var `c`)\n\
      \  err\n\
      \  (var `d`))\n"
    )
  , ( "f(a, b, c, d,)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`))\n"
    )
  , ( "f(a, b, c, d,,)"
    , "(0:13-0:14) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  (var `d`)\n\
      \  err)\n"
    )
  , ( "f(a, b, c,,)"
    , "(0:10-0:11) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  err)\n"
    )
  , ( "f(a, b,,)"
    , "(0:7-0:8) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  err)\n"
    )
  , ( "f(a,,)"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err)\n"
    )
  , ( "f(a, 😈, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `c`))\n"
    )
  , ( "f(😈, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, 😈)"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  err)\n"
    )
  , ( "f(a, }, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `c`))\n"
    )
  , ( "f(}, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, })"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  err)\n"
    )
  , ( "f(a, 😈}, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `c`))\n"
    )
  , ( "f(😈}, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:4-0:5) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, 😈})"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  err)\n"
    )
  , ( "f(a, }😈, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  err\n\
      \  (var `c`))\n"
    )
  , ( "f(}😈, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, }😈)"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:9-0:11) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  err)\n"
    )
  , ( "f(a, b😈, c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a😈, b, c)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, c😈)"
    , "(0:9-0:11) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b}, c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a}, b, c)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, c})"
    , "(0:9-0:10) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b😈}, c)"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a😈}, b, c)"
    , "(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, c😈})"
    , "(0:9-0:11) We wanted an expression but we found `😈`.\n\
      \(0:11-0:12) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b}😈, c)"
    , "(0:6-0:7) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a}😈, b, c)"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, c}😈)"
    , "(0:9-0:10) We wanted an expression but we found `}`.\n\
      \(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, 😈b, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(😈a, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, 😈c)"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, }b, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(}a, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, }c)"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, 😈}b, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(😈}a, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:4-0:5) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, 😈}c)"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \(0:10-0:11) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, }😈b, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(}😈a, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \(0:3-0:5) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, }😈c)"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:9-0:11) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, 😈b}, c)"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \(0:8-0:9) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(😈a}, b, c)"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, 😈c})"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \(0:11-0:12) We wanted an expression but we found `}`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, }b😈, c)"
    , "(0:5-0:6) We wanted an expression but we found `}`.\n\
      \(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(}a😈, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `}`.\n\
      \(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, }c😈)"
    , "(0:8-0:9) We wanted an expression but we found `}`.\n\
      \(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(, a)"
    , "(0:2-0:3) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err\n\
      \  (var `a`))\n"
    )
  , ( "f(, a, b)"
    , "(0:2-0:3) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(, a, b, c)"
    , "(0:2-0:3) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a,)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(,)"
    , "(0:2-0:3) We wanted an expression but we found `,`.\n\
      \\n\
      \(call\n\
      \  (var `f`)\n\
      \  err)\n"
    )
  , ( "f()"
    , "(call\n\
      \  (var `f`))\n"
    )
  , ( "f(a)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a,)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`))\n"
    )
  , ( "f(a, b)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b,)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( "f(a, b, c)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "f(a, b, c,)"
    , "(call\n\
      \  (var `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "let _ = x;"
    , "(bind hole (var `x`))\n"
    )
  , ( "let x = _;"
    , "(0:8-0:9) We wanted an expression but we found `_`.\n\
      \\n\
      \(bind (var `x`) err)\n"
    )
  , ( "let _ = _;"
    , "(0:8-0:9) We wanted an expression but we found `_`.\n\
      \\n\
      \(bind hole err)\n"
    )
  , ( "fun() {}"
    , "(fun\n\
      \  block)\n"
    )
  , ( "fun f() {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun(a) {}"
    , "(fun\n\
      \  (var `a`)\n\
      \  block)\n"
    )
  , ( "fun(a, b) {}"
    , "(fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block)\n"
    )
  , ( "fun(a, b, c) {}"
    , "(fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block)\n"
    )
  , ( "fun f(a) {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  block)\n"
    )
  , ( "fun f(a, b) {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block)\n"
    )
  , ( "fun f(a, b, c) {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block)\n"
    )
  , ( "fun(a,) {}"
    , "(fun\n\
      \  (var `a`)\n\
      \  block)\n"
    )
  , ( "fun(a, b,) {}"
    , "(fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block)\n"
    )
  , ( "fun(a, b, c,) {}"
    , "(fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block)\n"
    )
  , ( "fun f(a,) {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  block)\n"
    )
  , ( "fun f(a, b,) {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block)\n"
    )
  , ( "fun f(a, b, c,) {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block)\n"
    )
  , ( "fun() { let x = y; }"
    , "(fun\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
    )
  , ( "fun f() { let x = y; }"
    , "(fun\n\
      \  (name `f`)\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
    )
  , ( "fun() {}()"
    , "(call\n\
      \  (fun\n\
      \    block))\n"
    )
  , ( "(fun() {})()"
    , "(call\n\
      \  (wrap (fun\n\
      \    block)))\n"
    )
  , ( "let f = fun() {};"
    , "(bind (var `f`) (fun\n\
      \  block))\n"
    )
  , ( "let f = fun f() {};"
    , "(bind (var `f`) (fun\n\
      \  (name `f`)\n\
      \  block))\n"
    )
  , ( "let f = fun(a) {};"
    , "(bind (var `f`) (fun\n\
      \  (var `a`)\n\
      \  block))\n"
    )
  , ( "let f = fun(a, b) {};"
    , "(bind (var `f`) (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block))\n"
    )
  , ( "let f = fun(a, b, c) {};"
    , "(bind (var `f`) (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block))\n"
    )
  , ( "let f = fun f(a) {};"
    , "(bind (var `f`) (fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  block))\n"
    )
  , ( "let f = fun f(a, b) {};"
    , "(bind (var `f`) (fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block))\n"
    )
  , ( "let f = fun f(a, b, c) {};"
    , "(bind (var `f`) (fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block))\n"
    )
  , ( "let f = fun(a,) {};"
    , "(bind (var `f`) (fun\n\
      \  (var `a`)\n\
      \  block))\n"
    )
  , ( "let f = fun(a, b,) {};"
    , "(bind (var `f`) (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block))\n"
    )
  , ( "let f = fun(a, b, c,) {};"
    , "(bind (var `f`) (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block))\n"
    )
  , ( "let f = fun f(a,) {};"
    , "(bind (var `f`) (fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  block))\n"
    )
  , ( "let f = fun f(a, b,) {};"
    , "(bind (var `f`) (fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block))\n"
    )
  , ( "let f = fun f(a, b, c,) {};"
    , "(bind (var `f`) (fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block))\n"
    )
  , ( "let f = fun() { let x = y; };"
    , "(bind (var `f`) (fun\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`)))))\n"
    )
  , ( "let f = fun f() { let x = y; };"
    , "(bind (var `f`) (fun\n\
      \  (name `f`)\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`)))))\n"
    )
  , ( "let f = fun() {}();"
    , "(bind (var `f`) (call\n\
      \  (fun\n\
      \    block)))\n"
    )
  , ( "let f = (fun() {})();"
    , "(bind (var `f`) (call\n\
      \  (wrap (fun\n\
      \    block))))\n"
    )
  , ( "fun f() {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f) {}"
    , "(0:5-0:6) We wanted `(` but we found `)`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f( {}"
    , "(0:7-0:8) We wanted `)` but we found `{`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f() }"
    , "(0:8-0:9) We wanted `{` but we found `}`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f() {"
    , "(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun) {}"
    , "(0:3-0:4) We wanted `(` but we found `)`.\n\
      \\n\
      \(fun\n\
      \  block)\n"
    )
  , ( "fun( {}"
    , "(0:5-0:6) We wanted `)` but we found `{`.\n\
      \\n\
      \(fun\n\
      \  block)\n"
    )
  , ( "fun() }"
    , "(0:6-0:7) We wanted `{` but we found `}`.\n\
      \\n\
      \(fun\n\
      \  block)\n"
    )
  , ( "fun() {"
    , "(0:7-0:7) We wanted `}` but the file ended.\n\
      \\n\
      \(fun\n\
      \  block)\n"
    )
  , ( "fun(a, b let x = y; }"
    , "(0:9-0:12) We wanted `)` but we found `let`.\n\
      \(0:9-0:12) We wanted `{` but we found `let`.\n\
      \\n\
      \(fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
    )
  , ( "fun f(a, b let x = y; }"
    , "(0:11-0:14) We wanted `)` but we found `let`.\n\
      \(0:11-0:14) We wanted `{` but we found `let`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
    )
  , ( "fun 😈 f() {}"
    , "(0:4-0:6) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f😈() {}"
    , "(0:5-0:7) We wanted `(` but we found `😈`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f(😈) {}"
    , "(0:6-0:8) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  err\n\
      \  block)\n"
    )
  , ( "fun f() 😈 {}"
    , "(0:8-0:10) We wanted `{` but we found `😈`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f() {😈}"
    , "(0:9-0:11) We wanted a statement but we found `😈`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  (block\n\
      \    err))\n"
    )
  , ( "fun ] f() {}"
    , "(0:4-0:5) We wanted a variable name but we found `]`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f]() {}"
    , "(0:5-0:6) We wanted `(` but we found `]`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f(]) {}"
    , "(0:6-0:7) We wanted a variable name but we found `]`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  err\n\
      \  block)\n"
    )
  , ( "fun f() ] {}"
    , "(0:8-0:9) We wanted `{` but we found `]`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun f() {]}"
    , "(0:9-0:10) We wanted a statement but we found `]`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  (block\n\
      \    err))\n"
    )
  , ( "fun f(,) {}"
    , "(0:6-0:7) We wanted a variable name but we found `,`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  err\n\
      \  block)\n"
    )
  , ( "return"
    , "return\n"
    )
  , ( "return x"
    , "(return (var `x`))\n"
    )
  , ( "return\nx"
    , "return\n\
      \(var `x`)\n"
    )
  , ( "return;"
    , "return\n"
    )
  , ( "return;x"
    , "return\n\
      \(var `x`)\n"
    )
  , ( "return x;"
    , "(return (var `x`))\n"
    )
  , ( "return 😈 x;"
    , "(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(return (var `x`))\n"
    )
  , ( "return\n😈 x;"
    , "(1:0-1:2) We wanted `;` but we found `😈`.\n\
      \\n\
      \return\n\
      \(var `x`)\n"
    )
  , ( "return 😈\nx;"
    , "(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(return err)\n\
      \(var `x`)\n"
    )
  , ( "return ) x;"
    , "(0:7-0:8) We wanted an expression but we found `)`.\n\
      \\n\
      \(return (var `x`))\n"
    )
  , ( "return\n) x;"
    , "(1:0-1:1) We wanted `;` but we found `)`.\n\
      \\n\
      \return\n\
      \(var `x`)\n"
    )
  , ( "return )\nx;"
    , "(0:7-0:8) We wanted an expression but we found `)`.\n\
      \\n\
      \(return err)\n\
      \(var `x`)\n"
    )
  , ( "return 😈) x;"
    , "(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \(0:9-0:10) We wanted an expression but we found `)`.\n\
      \\n\
      \(return (var `x`))\n"
    )
  , ( "return\n😈) x;"
    , "(1:0-1:2) We wanted `;` but we found `😈`.\n\
      \(1:2-1:3) We wanted `;` but we found `)`.\n\
      \\n\
      \return\n\
      \(var `x`)\n"
    )
  , ( "return 😈)\nx;"
    , "(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \(0:9-0:10) We wanted an expression but we found `)`.\n\
      \\n\
      \(return err)\n\
      \(var `x`)\n"
    )
  , ( "return )😈 x;"
    , "(0:7-0:8) We wanted an expression but we found `)`.\n\
      \(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(return (var `x`))\n"
    )
  , ( "return\n)😈 x;"
    , "(1:0-1:1) We wanted `;` but we found `)`.\n\
      \(1:1-1:3) We wanted `;` but we found `😈`.\n\
      \\n\
      \return\n\
      \(var `x`)\n"
    )
  , ( "return )😈\nx;"
    , "(0:7-0:8) We wanted an expression but we found `)`.\n\
      \(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(return err)\n\
      \(var `x`)\n"
    )
  , ( "break"
    , "break\n"
    )
  , ( "break x"
    , "(break (var `x`))\n"
    )
  , ( "break\nx"
    , "break\n\
      \(var `x`)\n"
    )
  , ( "break;"
    , "break\n"
    )
  , ( "break;x"
    , "break\n\
      \(var `x`)\n"
    )
  , ( "break x;"
    , "(break (var `x`))\n"
    )
  , ( "loop {}"
    , "(loop block)\n"
    )
  , ( "loop { let x = y; }"
    , "(loop (block\n\
      \  (bind (var `x`) (var `y`))))\n"
    )
  , ( "!x"
    , "(not (var `x`))\n"
    )
  , ( "+x"
    , "(pos (var `x`))\n"
    )
  , ( "-x"
    , "(neg (var `x`))\n"
    )
  , ( "!x.p"
    , "(not (prop (var `x`) (name `p`)))\n"
    )
  , ( "!x()"
    , "(not (call\n  (var `x`)))\n"
    )
  , ( "!"
    , "(0:1-0:1) We wanted an expression but the file ended.\n\
      \\n\
      \(not err)\n"
    )
  , ( "!😈x"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \\n\
      \(not (var `x`))\n"
    )
  , ( "!)x"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(not (var `x`))\n"
    )
  , ( "!😈)x"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \(0:3-0:4) We wanted an expression but we found `)`.\n\
      \\n\
      \(not (var `x`))\n"
    )
  , ( "!)😈x"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(not (var `x`))\n"
    )
  , ( "!!x"
    , "(not (not (var `x`)))\n"
    )
  , ( "++x"
    , "(pos (pos (var `x`)))\n"
    )
  , ( "--x"
    , "(neg (neg (var `x`)))\n"
    )
  , ( "+-x"
    , "(pos (neg (var `x`)))\n"
    )
  , ( "-+x"
    , "(neg (pos (var `x`)))\n"
    )
  ]
