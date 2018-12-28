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
    , "(0:6-0:8) We wanted `:` but we found `😈`.\n\
      \\n\
      \(bind (var `x`) (type err) (var `y`))\n"
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
    , "(0:6-0:7) We wanted `:` but we found `)`.\n\
      \\n\
      \(bind (var `x`) (type err) (var `y`))\n"
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
    , "(0:6-0:8) We wanted `:` but we found `😈`.\n\
      \(0:9-0:10) We wanted `=` but we found a variable name.\n\
      \\n\
      \(bind (var `x`) (type err) (var `y`))\n"
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
    , "(0:6-0:8) We wanted `:` but we found `😈`.\n\
      \(0:8-0:9) We wanted `=` but we found `;`.\n\
      \(0:8-0:9) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var `x`) (type err) err)\n"
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
    , "(0:6-0:7) We wanted `:` but we found `)`.\n\
      \(0:8-0:9) We wanted `=` but we found a variable name.\n\
      \\n\
      \(bind (var `x`) (type err) (var `y`))\n"
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
    , "(0:6-0:7) We wanted `:` but we found `)`.\n\
      \(0:7-0:8) We wanted `=` but we found `;`.\n\
      \(0:7-0:8) We wanted an expression but we found `;`.\n\
      \\n\
      \(bind (var `x`) (type err) err)\n"
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
  , ( "if { let x = y }"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
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
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n\
      \object\n"
    )
  , ( "if x } {}"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block)\n\
      \object\n"
    )
  , ( "if x { {}"
    , "(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  (block\n\
      \    object))\n"
    )
  , ( "if { {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:7-0:7) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  (block\n\
      \    object))\n"
    )
  , ( "if } {}"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n\
      \object\n"
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
      \(0:7-0:7) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n\
      \object\n"
    )
  , ( "if x } {"
    , "(0:5-0:6) We wanted `{` but we found `}`.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block)\n\
      \object\n"
    )
  , ( "if x { {"
    , "(0:8-0:8) We wanted `}` but the file ended.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  (block\n\
      \    object))\n"
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
      \(0:6-0:6) We wanted `}` but the file ended.\n\
      \(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  (block\n\
      \    object))\n"
    )
  , ( "if } {"
    , "(0:3-0:4) We wanted an expression but we found `}`.\n\
      \(0:3-0:4) We wanted `{` but we found `}`.\n\
      \(0:6-0:6) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n\
      \object\n"
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
    , "(0:10-0:13) We wanted `}` but we found `let`.\n\
      \(0:20-0:21) We wanted an expression but we found `}`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block)\n\
      \object\n\
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
    , "(call (var `f`))\n"
    )
  , ( "f ()"
    , "(call (var `f`))\n"
    )
  , ( "f😈()"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \\n\
      \(call (var `f`))\n"
    )
  , ( "f)()"
    , "(0:1-0:2) We wanted an expression but we found `)`.\n\
      \\n\
      \(call (var `f`))\n"
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
      \(call (var `f`))\n"
    )
  , ( "😈.p"
    , "(0:0-0:2) We wanted a statement but we found `😈`.\n\
      \\n\
      \(variant (name `p`))\n"
    )
  , ( "(😈.p)"
    , "(0:1-0:3) We wanted an expression but we found `😈`.\n\
      \\n\
      \(wrap (variant (name `p`)))\n"
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
    , "(call (var `f`))\n"
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
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  block)\n"
    )
  , ( "(fun() {})"
    , "(wrap (fun\n\
      \  block))\n"
    )
  , ( "fun f() {}"
    , "(fun\n\
      \  (name `f`)\n\
      \  block)\n"
    )
  , ( "fun(a) {}"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  (var `a`)\n\
      \  block)\n"
    )
  , ( "(fun(a) {})"
    , "(wrap (fun\n\
      \  (var `a`)\n\
      \  block))\n"
    )
  , ( "fun(a, b) {}"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block)\n"
    )
  , ( "fun(a, b, c) {}"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block)\n"
    )
  , ( "(fun(a, b) {})"
    , "(wrap (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block))\n"
    )
  , ( "(fun(a, b, c) {})"
    , "(wrap (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block))\n"
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
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  (var `a`)\n\
      \  block)\n"
    )
  , ( "fun(a, b,) {}"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block)\n"
    )
  , ( "fun(a, b, c,) {}"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block)\n"
    )
  , ( "(fun(a,) {})"
    , "(wrap (fun\n\
      \  (var `a`)\n\
      \  block))\n"
    )
  , ( "(fun(a, b,) {})"
    , "(wrap (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  block))\n"
    )
  , ( "(fun(a, b, c,) {})"
    , "(wrap (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)\n\
      \  block))\n"
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
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
    )
  , ( "(fun() { let x = y; })"
    , "(wrap (fun\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`)))))\n"
    )
  , ( "fun f() { let x = y; }"
    , "(fun\n\
      \  (name `f`)\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
    )
  , ( "fun() {}()"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \(0:9-0:10) We wanted an expression but we found `)`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  block)\n\
      \(wrap err)\n"
    )
  , ( "(fun() {}())"
    , "(wrap (call (fun\n\
      \  block)))\n"
    )
  , ( "(fun() {})()"
    , "(call (wrap (fun\n\
      \  block)))\n"
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
    , "(bind (var `f`) (call (fun\n\
      \  block)))\n"
    )
  , ( "let f = (fun() {})();"
    , "(bind (var `f`) (call (wrap (fun\n\
      \  block))))\n"
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
    , "(0:9-0:9) We wanted `)` but the file ended.\n\
      \(0:9-0:9) We wanted `{` but the file ended.\n\
      \(0:9-0:9) We wanted `}` but the file ended.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  object\n\
      \  block)\n"
    )
  , ( "fun f( { let x = y }"
    , "(0:9-0:12) We wanted `}` but we found `let`.\n\
      \(0:9-0:12) We wanted `)` but we found `let`.\n\
      \(0:9-0:12) We wanted `{` but we found `let`.\n\
      \\n\
      \(fun\n\
      \  (name `f`)\n\
      \  object\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
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
    , "(0:3-0:4) We wanted a variable name but we found `)`.\n\
      \(0:3-0:4) We wanted `(` but we found `)`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  block)\n"
    )
  , ( "fun( {}"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \(0:7-0:7) We wanted `)` but the file ended.\n\
      \(0:7-0:7) We wanted `{` but the file ended.\n\
      \(0:7-0:7) We wanted `}` but the file ended.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  object\n\
      \  block)\n"
    )
  , ( "fun() }"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \(0:6-0:7) We wanted `{` but we found `}`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  block)\n"
    )
  , ( "fun() {"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \(0:7-0:7) We wanted `}` but the file ended.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  block)\n"
    )
  , ( "fun(a, b let x = y; }"
    , "(0:3-0:4) We wanted a variable name but we found `(`.\n\
      \(0:9-0:12) We wanted `)` but we found `let`.\n\
      \(0:9-0:12) We wanted `{` but we found `let`.\n\
      \\n\
      \(fun\n\
      \  err\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`))))\n"
    )
  , ( "(fun) {})"
    , "(0:4-0:5) We wanted `(` but we found `)`.\n\
      \\n\
      \(wrap (fun\n\
      \  block))\n"
    )
  , ( "(fun( {})"
    , "(0:9-0:9) We wanted `{` but the file ended.\n\
      \(0:9-0:9) We wanted `}` but the file ended.\n\
      \(0:9-0:9) We wanted `)` but the file ended.\n\
      \\n\
      \(wrap (fun\n\
      \  object\n\
      \  block))\n"
    )
  , ( "(fun() })"
    , "(0:7-0:8) We wanted `{` but we found `}`.\n\
      \\n\
      \(wrap (fun\n\
      \  block))\n"
    )
  , ( "(fun() {)"
    , "(0:8-0:9) We wanted `}` but we found `)`.\n\
      \\n\
      \(wrap (fun\n\
      \  block))\n"
    )
  , ( "(fun(a, b let x = y; })"
    , "(0:10-0:13) We wanted `)` but we found `let`.\n\
      \(0:10-0:13) We wanted `{` but we found `let`.\n\
      \\n\
      \(wrap (fun\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (block\n\
      \    (bind (var `x`) (var `y`)))))\n"
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
    , "(not (call (var `x`)))\n"
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
  , ( "a + b"
    , "(add (var `a`) (var `b`))\n"
    )
  , ( "a + b + c"
    , "(add (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a - b"
    , "(sub (var `a`) (var `b`))\n"
    )
  , ( "a - b - c"
    , "(sub (sub (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a * b"
    , "(mul (var `a`) (var `b`))\n"
    )
  , ( "a * b * c"
    , "(mul (mul (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a / b"
    , "(div (var `a`) (var `b`))\n"
    )
  , ( "a / b / c"
    , "(div (div (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a % b"
    , "(rem (var `a`) (var `b`))\n"
    )
  , ( "a % b % c"
    , "(rem (rem (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a == b"
    , "(eq (var `a`) (var `b`))\n"
    )
  , ( "a == b == c"
    , "(eq (eq (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a != b"
    , "(neq (var `a`) (var `b`))\n"
    )
  , ( "a != b != c"
    , "(neq (neq (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a < b"
    , "(lt (var `a`) (var `b`))\n"
    )
  , ( "a < b < c"
    , "(lt (lt (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a <= b"
    , "(lte (var `a`) (var `b`))\n"
    )
  , ( "a <= b <= c"
    , "(lte (lte (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a > b"
    , "(gt (var `a`) (var `b`))\n"
    )
  , ( "a > b > c"
    , "(gt (gt (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a >= b"
    , "(gte (var `a`) (var `b`))\n"
    )
  , ( "a >= b >= c"
    , "(gte (gte (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b - c"
    , "(sub (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a - b + c"
    , "(add (sub (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b * c"
    , "(add (var `a`) (mul (var `b`) (var `c`)))\n"
    )
  , ( "a * b + c"
    , "(add (mul (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b / c"
    , "(add (var `a`) (div (var `b`) (var `c`)))\n"
    )
  , ( "a / b + c"
    , "(add (div (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a * b / c"
    , "(div (mul (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a / b * c"
    , "(mul (div (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b * c + d"
    , "(add (add (var `a`) (mul (var `b`) (var `c`))) (var `d`))\n"
    )
  , ( "a * b + c * d"
    , "(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))\n"
    )
  , ( "a ^ b + c"
    , "(add (pow (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b ^ c"
    , "(add (var `a`) (pow (var `b`) (var `c`)))\n"
    )
  , ( "a ^ b * c"
    , "(mul (pow (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a * b ^ c"
    , "(mul (var `a`) (pow (var `b`) (var `c`)))\n"
    )
  , ( "a > b + c"
    , "(gt (var `a`) (add (var `b`) (var `c`)))\n"
    )
  , ( "a + b > c"
    , "(gt (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a < b + c"
    , "(lt (var `a`) (add (var `b`) (var `c`)))\n"
    )
  , ( "a + b < c"
    , "(lt (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a >= b + c"
    , "(gte (var `a`) (add (var `b`) (var `c`)))\n"
    )
  , ( "a + b >= c"
    , "(gte (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a <= b + c"
    , "(lte (var `a`) (add (var `b`) (var `c`)))\n"
    )
  , ( "a + b <= c"
    , "(lte (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b == c"
    , "(eq (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a == b + c"
    , "(eq (var `a`) (add (var `b`) (var `c`)))\n"
    )
  , ( "a + b != c"
    , "(neq (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a != b + c"
    , "(neq (var `a`) (add (var `b`) (var `c`)))\n"
    )
  , ( "a =="
    , "(0:4-0:4) We wanted an expression but the file ended.\n\
      \\n\
      \(eq (var `a`) err)\n"
    )
  , ( "== b"
    , "(0:0-0:2) We wanted a statement but we found `==`.\n\
      \\n\
      \(var `b`)\n"
    )
  , ( "a 😈 == b"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(eq (var `a`) (var `b`))\n"
    )
  , ( "a == 😈 b"
    , "(0:5-0:7) We wanted an expression but we found `😈`.\n\
      \\n\
      \(eq (var `a`) (var `b`))\n"
    )
  , ( "a == b 😈"
    , "(0:7-0:9) We wanted an expression but we found `😈`.\n\
      \\n\
      \(eq (var `a`) (var `b`))\n"
    )
  , ( "a ) == b"
    , "(0:2-0:3) We wanted an expression but we found `)`.\n\
      \\n\
      \(eq (var `a`) (var `b`))\n"
    )
  , ( "a == ) b"
    , "(0:5-0:6) We wanted an expression but we found `)`.\n\
      \\n\
      \(eq (var `a`) (var `b`))\n"
    )
  , ( "a.p + b.q"
    , "(add (prop (var `a`) (name `p`)) (prop (var `b`) (name `q`)))\n"
    )
  , ( "!a + !b"
    , "(add (not (var `a`)) (not (var `b`)))\n"
    )
  , ( "a() + b()"
    , "(add (call (var `a`)) (call (var `b`)))\n"
    )
  , ( "a + b +"
    , "(0:7-0:7) We wanted an expression but the file ended.\n\
      \\n\
      \(add (add (var `a`) (var `b`)) err)\n"
    )
  , ( "a + b + c +"
    , "(0:11-0:11) We wanted an expression but the file ended.\n\
      \\n\
      \(add (add (add (var `a`) (var `b`)) (var `c`)) err)\n"
    )
  , ( "a 😈 + b + c"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + 😈 b + c"
    , "(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b 😈 + c"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b + 😈 c"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + b + c 😈"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (add (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "^ b * c ^ d"
    , "(0:0-0:1) We wanted a statement but we found `^`.\n\
      \\n\
      \(mul (var `b`) (pow (var `c`) (var `d`)))\n"
    )
  , ( "a ^ * c ^ d"
    , "(0:4-0:5) We wanted an expression but we found `*`.\n\
      \\n\
      \(mul (pow (var `a`) err) (pow (var `c`) (var `d`)))\n"
    )
  , ( "a ^ b * ^ d"
    , "(0:8-0:9) We wanted an expression but we found `^`.\n\
      \\n\
      \(mul (pow (var `a`) (var `b`)) (pow err (var `d`)))\n"
    )
  , ( "a * ^ c * d"
    , "(0:4-0:5) We wanted an expression but we found `^`.\n\
      \\n\
      \(mul (mul (var `a`) (pow err (var `c`))) (var `d`))\n"
    )
  , ( "a * b ^ * d"
    , "(0:8-0:9) We wanted an expression but we found `*`.\n\
      \\n\
      \(mul (mul (var `a`) (pow (var `b`) err)) (var `d`))\n"
    )
  , ( "a ^ b * c ^"
    , "(0:11-0:11) We wanted an expression but the file ended.\n\n(mul (pow (var `a`) (var `b`)) (pow (var `c`) err))\n"
    )
  , ( "a 😈 * b + c * d"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))\n"
    )
  , ( "a * 😈 b + c * d"
    , "(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))\n"
    )
  , ( "a * b 😈 + c * d"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))\n"
    )
  , ( "a * b + 😈 c * d"
    , "(0:8-0:10) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))\n"
    )
  , ( "a * b + c 😈 * d"
    , "(0:10-0:12) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))\n"
    )
  , ( "a * b + c * 😈 d"
    , "(0:12-0:14) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))\n"
    )
  , ( "a * b + c * d 😈"
    , "(0:14-0:16) We wanted an expression but we found `😈`.\n\
      \\n\
      \(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))\n"
    )
  , ( "a - b + c"
    , "(add (sub (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a + -b + c"
    , "(add (add (var `a`) (neg (var `b`))) (var `c`))\n"
    )
  , ( "if x {} else if y {}"
    , "(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (if\n\
      \    (var `y`)\n\
      \    block))\n"
    )
  , ( "if x {} else if y {} else {}"
    , "(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (if\n\
      \    (var `y`)\n\
      \    block\n\
      \    block))\n"
    )
  , ( "if x {} else if y {} else if z {}"
    , "(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (if\n\
      \    (var `y`)\n\
      \    block\n\
      \    (if\n\
      \      (var `z`)\n\
      \      block)))\n"
    )
  , ( "if x {} else if y {} else if z {} else {}"
    , "(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (if\n\
      \    (var `y`)\n\
      \    block\n\
      \    (if\n\
      \      (var `z`)\n\
      \      block\n\
      \      block)))\n"
    )
  , ( "if x {} else if {}"
    , "(0:16-0:17) We wanted an expression but we found `{`.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (if\n\
      \    err\n\
      \    block))\n"
    )
  , ( "if x {} else 😈 if y {}"
    , "(0:13-0:15) We wanted `{` but we found `😈`.\n\
      \(0:23-0:23) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (block\n\
      \    (if\n\
      \      (var `y`)\n\
      \      block)))\n"
    )
  , ( "if x {} else 😈 if y + z {}"
    , "(0:13-0:15) We wanted `{` but we found `😈`.\n\
      \(0:27-0:27) We wanted `}` but the file ended.\n\
      \\n\
      \(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (block\n\
      \    (if\n\
      \      (add (var `y`) (var `z`))\n\
      \      block)))\n"
    )
  , ( "if x {} else { if y {} }"
    , "(if\n\
      \  (var `x`)\n\
      \  block\n\
      \  (block\n\
      \    (if\n\
      \      (var `y`)\n\
      \      block)))\n"
    )
  , ( "a + b * c ^ d"
    , "(add (var `a`) (mul (var `b`) (pow (var `c`) (var `d`))))\n"
    )
  , ( "a 😈 + * b"
    , "(0:2-0:4) We wanted an expression but we found `😈`.\n\
      \(0:7-0:8) We wanted an expression but we found `*`.\n\
      \\n\
      \(add (var `a`) (mul err (var `b`)))\n"
    )
  , ( "{}"
    , "object\n"
    )
  , ( "{p: a}"
    , "(object\n\
      \  (prop (name `p`) (var `a`)))\n"
    )
  , ( "{p: a, q: b}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "{,}"
    , "(0:1-0:2) We wanted a variable name but we found `,`.\n\
      \\n\
      \(object\n\
      \  (prop err))\n"
    )
  , ( "{p: a,}"
    , "(object\n\
      \  (prop (name `p`) (var `a`)))\n"
    )
  , ( "{p: a, q: b,}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "{p: a q: b}"
    , "(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "{p: a,, q: b}"
    , "(0:6-0:7) We wanted a variable name but we found `,`.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (prop err)\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "{p: a, q: b,,}"
    , "(0:12-0:13) We wanted a variable name but we found `,`.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (prop (name `q`) (var `b`))\n\
      \  (prop err))\n"
    )
  , ( "{p: a q: b,}"
    , "(0:6-0:7) We wanted `,` but we found a variable name.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "{| o}"
    , "(object\n\
      \  (var `o`))\n"
    )
  , ( "{p: a | o}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (var `o`))\n"
    )
  , ( "{p: a, | o}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (var `o`))\n"
    )
  , ( "{p: a, q: b | o}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (prop (name `q`) (var `b`))\n\
      \  (var `o`))\n"
    )
  , ( "{p: a | {q: b}}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (object\n\
      \    (prop (name `q`) (var `b`))))\n"
    )
  , ( "{p: a | {q: b | {}}}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (object\n\
      \    (prop (name `q`) (var `b`))\n\
      \    object))\n"
    )
  , ( "{p: a | {q: b | o}}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (object\n\
      \    (prop (name `q`) (var `b`))\n\
      \    (var `o`)))\n"
    )
  , ( "{: a}"
    , "(0:1-0:2) We wanted a variable name but we found `:`.\n\
      \\n\
      \(object\n\
      \  (prop (name `a`)))\n"
    )
  , ( "{p a}"
    , "(0:3-0:4) We wanted `,` but we found a variable name.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`))\n\
      \  (prop (name `a`)))\n"
    )
  , ( "{p: }"
    , "(0:4-0:5) We wanted an expression but we found `}`.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) err))\n"
    )
  , ( "{: a, q: b}"
    , "(0:1-0:2) We wanted a variable name but we found `:`.\n\
      \\n\
      \(object\n\
      \  (prop (name `a`))\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "{p a, q: b}"
    , "(0:3-0:4) We wanted `,` but we found a variable name.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`))\n\
      \  (prop (name `a`))\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "{p: , q: b}"
    , "(0:4-0:5) We wanted an expression but we found `,`.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) err)\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "{p}"
    , "(object\n\
      \  (prop (name `p`)))\n"
    )
  , ( "{p, q}"
    , "(object\n\
      \  (prop (name `p`))\n\
      \  (prop (name `q`)))\n"
    )
  , ( "{p: a, q}"
    , "(object\n\
      \  (prop (name `p`) (var `a`))\n\
      \  (prop (name `q`)))\n"
    )
  , ( "{p, q: b}"
    , "(object\n\
      \  (prop (name `p`))\n\
      \  (prop (name `q`) (var `b`)))\n"
    )
  , ( "if {} {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \\n\
      \(if\n\
      \  err\n\
      \  block)\n\
      \object\n"
    )
  , ( "if {p: a}.p {}"
    , "(0:3-0:4) We wanted an expression but we found `{`.\n\
      \(0:5-0:6) We wanted an expression but we found `:`.\n\
      \\n\
      \(prop (if\n\
      \  err\n\
      \  (block\n\
      \    (var `p`)\n\
      \    (var `a`))) (name `p`))\n\
      \object\n"
    )
  , ( "{p: a}.p"
    , "(prop (object\n\
      \  (prop (name `p`) (var `a`))) (name `p`))\n"
    )
  , ( "{😈 p: a}"
    , "(0:1-0:3) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) (var `a`)))\n"
    )
  , ( "{p 😈 : a}"
    , "(0:3-0:5) We wanted `:` but we found `😈`.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) (var `a`)))\n"
    )
  , ( "{p: 😈 a}"
    , "(0:4-0:6) We wanted an expression but we found `😈`.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) (var `a`)))\n"
    )
  , ( "{p: a 😈}"
    , "(0:6-0:8) We wanted an expression but we found `😈`.\n\
      \\n\
      \(object\n\
      \  (prop (name `p`) (var `a`)))\n"
    )
  , ( "{😈}"
    , "(0:1-0:3) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(object\n\
      \  (prop err))\n"
    )
  , ( "a && b"
    , "(and (var `a`) (var `b`))\n"
    )
  , ( "a && b && c"
    , "(and (and (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a || b || c"
    , "(or (or (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a && b || c"
    , "(or (and (var `a`) (var `b`)) (var `c`))\n"
    )
  , ( "a || b && c"
    , "(or (var `a`) (and (var `b`) (var `c`)))\n"
    )
  , ( "a && b && c && d"
    , "(and (and (and (var `a`) (var `b`)) (var `c`)) (var `d`))\n"
    )
  , ( "a || b && c && d"
    , "(or (var `a`) (and (and (var `b`) (var `c`)) (var `d`)))\n"
    )
  , ( "a && b || c && d"
    , "(or (and (var `a`) (var `b`)) (and (var `c`) (var `d`)))\n"
    )
  , ( "a && b && c || d"
    , "(or (and (and (var `a`) (var `b`)) (var `c`)) (var `d`))\n"
    )
  , ( "a && b || c || d"
    , "(or (or (and (var `a`) (var `b`)) (var `c`)) (var `d`))\n"
    )
  , ( "a || b && c || d"
    , "(or (or (var `a`) (and (var `b`) (var `c`))) (var `d`))\n"
    )
  , ( "a || b || c && d"
    , "(or (or (var `a`) (var `b`)) (and (var `c`) (var `d`)))\n"
    )
  , ( "a || b || c || d"
    , "(or (or (or (var `a`) (var `b`)) (var `c`)) (var `d`))\n"
    )
  , ( ".V"
    , "(variant (name `V`))\n"
    )
  , ( ".V()"
    , "(variant (name `V`))\n"
    )
  , ( ".V(a)"
    , "(variant (name `V`)\n\
      \  (var `a`))\n"
    )
  , ( ".V(a, b)"
    , "(variant (name `V`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( ".V(a, b, c)"
    , "(variant (name `V`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( ".V(,)"
    , "(0:3-0:4) We wanted an expression but we found `,`.\n\
      \\n\
      \(variant (name `V`)\n\
      \  err)\n"
    )
  , ( ".V(a,)"
    , "(variant (name `V`)\n\
      \  (var `a`))\n"
    )
  , ( ".V(a, b,)"
    , "(variant (name `V`)\n\
      \  (var `a`)\n\
      \  (var `b`))\n"
    )
  , ( ".V(a, b, c,)"
    , "(variant (name `V`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`))\n"
    )
  , ( "."
    , "(0:1-0:1) We wanted a variable name but the file ended.\n\
      \\n\
      \(variant err)\n"
    )
  , ( ".V"
    , "(variant (name `V`))\n"
    )
  , ( ".V("
    , "(0:3-0:3) We wanted `)` but the file ended.\n\
      \\n\
      \(variant (name `V`))\n"
    )
  , ( ".V)"
    , "(0:2-0:3) We wanted `(` but we found `)`.\n\
      \\n\
      \(variant (name `V`)err)\n"
    )
  , ( ".()"
    , "(0:1-0:2) We wanted a variable name but we found `(`.\n\
      \\n\
      \(variant err)\n"
    )
  , ( ".(a)"
    , "(0:1-0:2) We wanted a variable name but we found `(`.\n\
      \\n\
      \(variant err\n\
      \  (var `a`))\n"
    )
  , ( ".😈(a)"
    , "(0:1-0:3) We wanted a variable name but we found `😈`.\n\
      \\n\
      \(variant err\n\
      \  (var `a`))\n"
    )
  , ( "let true = x"
    , "(bind (bool true) (var `x`))\n"
    )
  , ( "let {} = o"
    , "(bind object (var `o`))\n"
    )
  , ( "let {a} = o"
    , "(bind (object\n\
      \  (prop (name `a`))) (var `o`))\n"
    )
  , ( "let {a, b} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (prop (name `b`))) (var `o`))\n"
    )
  , ( "let {a, b, c} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (prop (name `b`))\n\
      \  (prop (name `c`))) (var `o`))\n"
    )
  , ( "let {,} = o"
    , "(0:5-0:6) We wanted a variable name but we found `,`.\n\
      \\n\
      \(bind (object\n\
      \  (prop err)) (var `o`))\n"
    )
  , ( "let {a,} = o"
    , "(bind (object\n\
      \  (prop (name `a`))) (var `o`))\n"
    )
  , ( "let {a, b,} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (prop (name `b`))) (var `o`))\n"
    )
  , ( "let {a, b, c,} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (prop (name `b`))\n\
      \  (prop (name `c`))) (var `o`))\n"
    )
  , ( "let {a: a2} = o"
    , "(bind (object\n\
      \  (prop (name `a`) (var `a2`))) (var `o`))\n"
    )
  , ( "let {a: a2, b: b2} = o"
    , "(bind (object\n\
      \  (prop (name `a`) (var `a2`))\n\
      \  (prop (name `b`) (var `b2`))) (var `o`))\n"
    )
  , ( "let {a: a2, b: b2, c: c2} = o"
    , "(bind (object\n\
      \  (prop (name `a`) (var `a2`))\n\
      \  (prop (name `b`) (var `b2`))\n\
      \  (prop (name `c`) (var `c2`))) (var `o`))\n"
    )
  , ( "let {a: a2, b} = o"
    , "(bind (object\n\
      \  (prop (name `a`) (var `a2`))\n\
      \  (prop (name `b`))) (var `o`))\n"
    )
  , ( "let {a, b: b2} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (prop (name `b`) (var `b2`))) (var `o`))\n"
    )
  , ( "let {| o} = o"
    , "(bind (object\n\
      \  (var `o`)) (var `o`))\n"
    )
  , ( "let {a | o} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (var `o`)) (var `o`))\n"
    )
  , ( "let {a, | o} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (var `o`)) (var `o`))\n"
    )
  , ( "let {a, b | o} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (prop (name `b`))\n\
      \  (var `o`)) (var `o`))\n"
    )
  , ( "let {a, b, c | o} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (prop (name `b`))\n\
      \  (prop (name `c`))\n\
      \  (var `o`)) (var `o`))\n"
    )
  , ( "let {a | {b | o}} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (object\n\
      \    (prop (name `b`))\n\
      \    (var `o`))) (var `o`))\n"
    )
  , ( "let {a | {b | {c | o}}} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (object\n\
      \    (prop (name `b`))\n\
      \    (object\n\
      \      (prop (name `c`))\n\
      \      (var `o`)))) (var `o`))\n"
    )
  , ( "let {a | {b | {c | {}}}} = o"
    , "(bind (object\n\
      \  (prop (name `a`))\n\
      \  (object\n\
      \    (prop (name `b`))\n\
      \    (object\n\
      \      (prop (name `c`))\n\
      \      object))) (var `o`))\n"
    )
  , ( "{a: {b: c}}"
    , "(object\n\
      \  (prop (name `a`) (object\n\
      \    (prop (name `b`) (var `c`)))))\n"
    )
  , ( "{a {}}"
    , "(0:3-0:4) We wanted `}` but we found `{`.\n\
      \(0:5-0:6) We wanted an expression but we found `}`.\n\
      \\n\
      \(object\n\
      \  (prop (name `a`)))\n\
      \object\n"
    )
  , ( "{a true}"
    , "(0:3-0:7) We wanted `}` but we found `true`.\n\
      \(0:7-0:8) We wanted an expression but we found `}`.\n\
      \\n\
      \(object\n\
      \  (prop (name `a`)))\n\
      \(bool true)\n"
    )
  , ( "let .V = x"
    , "(bind (variant (name `V`)) (var `x`))\n"
    )
  , ( "let .V() = x"
    , "(bind (variant (name `V`)) (var `x`))\n"
    )
  , ( "let .V(a) = x"
    , "(bind (variant (name `V`)\n\
      \  (var `a`)) (var `x`))\n"
    )
  , ( "let .V(a, b) = x"
    , "(bind (variant (name `V`)\n\
      \  (var `a`)\n\
      \  (var `b`)) (var `x`))\n"
    )
  , ( "let .V(a, b, c) = x"
    , "(bind (variant (name `V`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)) (var `x`))\n"
    )
  , ( "let .V(,) = x"
    , "(0:7-0:8) We wanted a variable name but we found `,`.\n\
      \\n\
      \(bind (variant (name `V`)\n\
      \  err) (var `x`))\n"
    )
  , ( "let .V(a,) = x"
    , "(bind (variant (name `V`)\n\
      \  (var `a`)) (var `x`))\n"
    )
  , ( "let .V(a, b,) = x"
    , "(bind (variant (name `V`)\n\
      \  (var `a`)\n\
      \  (var `b`)) (var `x`))\n"
    )
  , ( "let .V(a, b, c,) = x"
    , "(bind (variant (name `V`)\n\
      \  (var `a`)\n\
      \  (var `b`)\n\
      \  (var `c`)) (var `x`))\n"
    )
  , ( "match x { y -> {} }"
    , "(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block))\n"
    )
  , ( "match x { y -> {} z -> {} }"
    , "(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block)\n\
      \  (case (var `z`) block))\n"
    )
  , ( "match x { .Red -> {} .Green -> {} .Blue -> {} }"
    , "(match\n\
      \  (var `x`)\n\
      \  (case (variant (name `Red`)) block)\n\
      \  (case (variant (name `Green`)) block)\n\
      \  (case (variant (name `Blue`)) block))\n"
    )
  , ( "match x {\n\
      \  y -> {}\n\
      \  z -> {}\n\
      \}"
    , "(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block)\n\
      \  (case (var `z`) block))\n"
    )
  , ( "match x {\n\
      \  .Red -> {}\n\
      \  .Green -> {}\n\
      \  .Blue -> {}\n\
      \}"
    , "(match\n\
      \  (var `x`)\n\
      \  (case (variant (name `Red`)) block)\n\
      \  (case (variant (name `Green`)) block)\n\
      \  (case (variant (name `Blue`)) block))\n"
    )
  , ( "match {} {}"
    , "(match\n\
      \  object)\n"
    )
  , ( "match {}"
    , "(0:8-0:8) We wanted `{` but the file ended.\n\
      \(0:8-0:8) We wanted `}` but the file ended.\n\
      \\n\
      \(match\n\
      \  object)\n"
    )
  , ( "match { y -> {} }"
    , "(0:10-0:12) We wanted `:` but we found `->`.\n\
      \(0:13-0:14) We wanted `}` but we found `{`.\n\
      \(0:16-0:17) We wanted an expression but we found `}`.\n\
      \\n\
      \(match\n\
      \  (object\n\
      \    (prop (name `y`) err)))\n"
    )
  , ( "match x y -> {} }"
    , "(0:8-0:9) We wanted `{` but we found a variable name.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block))\n"
    )
  , ( "match x { y -> {}"
    , "(0:17-0:17) We wanted `}` but the file ended.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block))\n"
    )
  , ( "match x { -> {} }"
    , "(0:10-0:12) We wanted a variable name but we found `->`.\n\
      \(0:16-0:17) We wanted `->` but we found `}`.\n\
      \(0:16-0:17) We wanted `{` but we found `}`.\n\
      \(0:17-0:17) We wanted `}` but the file ended.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case object block))\n"
    )
  , ( "match x { -> { let a = b } }"
    , "(0:10-0:12) We wanted a variable name but we found `->`.\n\
      \(0:15-0:18) We wanted `}` but we found `let`.\n\
      \(0:15-0:18) We wanted `->` but we found `let`.\n\
      \(0:15-0:18) We wanted `{` but we found `let`.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case object (block\n\
      \    (bind (var `a`) (var `b`)))))\n"
    )
  , ( "match x { y {} }"
    , "(0:12-0:13) We wanted `->` but we found `{`.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block))\n"
    )
  , ( "match x { y -> } }"
    , "(0:15-0:16) We wanted `{` but we found `}`.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block))\n"
    )
  , ( "match x { y -> { }"
    , "(0:18-0:18) We wanted `}` but the file ended.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block))\n"
    )
  , ( "match x { y -> {} let a = b }"
    , "(0:18-0:21) We wanted `}` but we found `let`.\n\
      \(0:28-0:29) We wanted an expression but we found `}`.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block))\n\
      \(bind (var `a`) (var `b`))\n"
    )
  , ( "match x { y -> {} let a = b z -> {} }"
    , "(0:18-0:21) We wanted `}` but we found `let`.\n\
      \(0:30-0:32) We wanted an expression but we found `->`.\n\
      \(0:36-0:37) We wanted an expression but we found `}`.\n\
      \\n\
      \(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) block))\n\
      \(bind (var `a`) (var `b`))\n\
      \(var `z`)\n\
      \object\n"
    )
  , ( "match x { y -> { let a = b } }"
    , "(match\n\
      \  (var `x`)\n\
      \  (case (var `y`) (block\n\
      \    (bind (var `a`) (var `b`)))))\n"
    )
  , ( "let x: T = y;"
    , "(bind (var `x`) (type (var `T`)) (var `y`))\n"
    )
  , ( "let x: = y;"
    , "(0:7-0:8) We wanted a type but we found `=`.\n\
      \\n\
      \(bind (var `x`) (type err) (var `y`))\n"
    )
  , ( "let x T = y;"
    , "(0:6-0:7) We wanted `=` but we found a variable name.\n\
      \(0:8-0:9) We wanted an expression but we found `=`.\n\
      \\n\
      \(bind (var `x`) (var `T`))\n\
      \(var `y`)\n"
    )
  , ( "(x: T)"
    , "(wrap (var `x`) (type (var `T`)))\n"
    )
  , ( "(x:)"
    , "(0:3-0:4) We wanted a type but we found `)`.\n\
      \\n\
      \(wrap (var `x`) (type err))\n"
    )
  , ( "(x T)"
    , "(0:3-0:4) We wanted `)` but we found a variable name.\n\
      \(0:4-0:5) We wanted an expression but we found `)`.\n\
      \\n\
      \(wrap (var `x`))\n\
      \(var `T`)\n"
    )
  ]
