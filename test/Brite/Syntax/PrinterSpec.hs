{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.PrinterSpec (spec) where

import Brite.Diagnostics
import Brite.Syntax.Parser
import Brite.Syntax.Printer
import qualified Brite.Syntax.PrinterAST as PrinterAST
import Brite.Syntax.Tokens
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import Test.Hspec
import System.IO

testData :: [Text]
testData =
  [ "true"
  , "false"
  , "true false"
  , "true false true"
  , "true"
  , "  true"
  , "true  "
  , "  true  "
  , "\ntrue"
  , "\n\ntrue"
  , "\rtrue"
  , "\r\rtrue"
  , "\r\ntrue"
  , "\r\n\r\ntrue"
  , "\n\rtrue"
  , "/**/true"
  , "/**/ true"
  , "/**/  true"
  , "/**/   true"
  , "/**/\ttrue"
  , "/**/\t\ttrue"
  , "/**/\t\t\ttrue"
  , "/**/\ntrue"
  , "/**/\n\ntrue"
  , "/**/\n\n\ntrue"
  , "/**/\rtrue"
  , "/**/\r\rtrue"
  , "/**/\r\r\rtrue"
  , "/**/\r\ntrue"
  , "/**/\r\n\r\ntrue"
  , "/**/\r\n\r\n\r\ntrue"
  , "/**/\n\rtrue"
  , "/**/\n\r\r\ntrue"
  , "/**/ \ntrue"
  , "/**/\n true"
  , "/**/  \ntrue"
  , "/**/\n  true"
  , "/**/   \ntrue"
  , "/**/\n   true"
  , "/**/   \n   true"
  , "/**/ \n \n true"
  , "/**/ \n \n \n true"
  , "//\n/**/ true"
  , "//\n\n/**/ true"
  , "//\n\n\n/**/ true"
  , "/**/\n/**/ true"
  , "/**/\n\n/**/ true"
  , "/**/\n\n\n/**/ true"
  , "/**/ /**/ true"
  , "//\ntrue"
  , "//\n\ntrue"
  , "//\n\n\ntrue"
  , "//\rtrue"
  , "//\r\rtrue"
  , "//\r\r\rtrue"
  , "//\r\ntrue"
  , "//\r\n\r\ntrue"
  , "//\r\n\r\n\r\ntrue"
  , "// \ntrue"
  , "//  \ntrue"
  , "//   \ntrue"
  , "//\n true"
  , "//\n  true"
  , "//\n   true"
  , "// \n \n true"
  , "// \n \n \n true"
  , "/**//**/true"
  , "/**/ /**/ true"
  , "/**/  /**/  true"
  , "/**/   /**/   true"
  , "/**/\n/**/\ntrue"
  , "/**/\n\n/**/\n\ntrue"
  , "/**/\n\n\n/**/\n\n\ntrue"
  , "/**/\n\n/**/\ntrue"
  , "/**/\n\n\n/**/\ntrue"
  , "/**/\n/**/\n\ntrue"
  , "/**/\n/**/\n\n\ntrue"
  , "//\n//\ntrue"
  , "//\n\n//\n\ntrue"
  , "//\n\n\n//\n\n\ntrue"
  , "//\n\n//\ntrue"
  , "//\n\n\n//\ntrue"
  , "//\n//\n\ntrue"
  , "//\n//\n\n\ntrue"
  , "true/**/"
  , "true /**/"
  , "true  /**/"
  , "true   /**/"
  , "true/**//**/"
  , "true /**/ /**/"
  , "true  /**/  /**/"
  , "true   /**/   /**/"
  , "true//"
  , "true //"
  , "true  //"
  , "true   //"
  , "true //\n"
  , "true //\n\n"
  , "true //\n\n\n"
  , "true //\nfalse"
  , "x"
  , "x y"
  , "x y z"
  , "(x)"
  , "(true)"
  , "( x)"
  , "(x )"
  , "( x )"
  , "(  x)"
  , "(x  )"
  , "(  x  )"
  , "!x"
  , "+x"
  , "-x"
  , "!  x"
  , "!\nx"
  , "a + b + c + d"
  , "a + (b + c + d)"
  , "a + (b + (c + d))"
  , "((a + b) + c) + d"
  , "a + (b + c) + d"
  , "a + b - c + d"
  , "a + (b - c + d)"
  , "a + (b - (c + d))"
  , "((a + b) - c) + d"
  , "a + (b - c) + d"
  , "a * b + c * d"
  , "a * (b + c) * d"
  , "(a * b) + (c * d)"
  , "((a * b) + c) * d"
  , "a * (b + (c * d))"
  , "a + -b + c"
  , "-a + b + -c"
  , "a + (-b) + c"
  , "(-a) + b + (-c)"
  , "a + -(b) + c"
  , "-(a) + b + -(c)"
  , "a + -(b + c)"
  , "-(a + b) + -c"
  , "(a * b) ^ (c * d)"
  , "(a + b) * (c + d)"
  , "(a + b) / (c + d)"
  , "(a + b) % (c + d)"
  , "(a < b) + (c < d)"
  , "(a < b) - (c < d)"
  , "(a == b) < (c == d)"
  , "(a == b) <= (c == d)"
  , "(a == b) > (c == d)"
  , "(a == b) >= (c == d)"
  , "(a && b) == (c && d)"
  , "(a && b) != (c && d)"
  , "(a || b) && (c || d)"
  , "(a && b) || (c && d)"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + a + b"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + a * b"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + (a + b)"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + (a * b)"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + a + b"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + a * b"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + (a + b)"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong + (a * b)"
  , "reallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyLong * reallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyLong * reallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyLong * reallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyLong"
  , "(reallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyLong) * (reallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyLong)"
  , "reallyReallyReallyReallyReallyReallyReallyLong * reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong * reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong * (reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong) * reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong && (reallyReallyReallyReallyReallyReallyReallyLong || reallyReallyReallyReallyReallyReallyReallyLong) && reallyReallyReallyReallyReallyReallyReallyLong"
  , "f(reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong)"
  , "(reallyReallyReallyReallyReallyReallyReallyLong || reallyReallyReallyReallyReallyReallyReallyLong) && (reallyReallyReallyReallyReallyReallyReallyLong || reallyReallyReallyReallyReallyReallyReallyLong)"
  , "// blah blah blah\n(reallyReallyReallyReallyReallyReallyReallyLong ||\n// blah blah blah\nreallyReallyReallyReallyReallyReallyReallyLong) &&\n// blah blah blah\n(reallyReallyReallyReallyReallyReallyReallyLong ||\n// blah blah blah\nreallyReallyReallyReallyReallyReallyReallyLong)"
  , "\n// blah blah blah\n(reallyReallyReallyReallyReallyReallyReallyLong ||\n\n// blah blah blah\nreallyReallyReallyReallyReallyReallyReallyLong) &&\n\n// blah blah blah\n(reallyReallyReallyReallyReallyReallyReallyLong ||\n\n// blah blah blah\nreallyReallyReallyReallyReallyReallyReallyLong)"
  , "// blah blah blah\n\n(reallyReallyReallyReallyReallyReallyReallyLong ||\n// blah blah blah\n\nreallyReallyReallyReallyReallyReallyReallyLong) &&\n// blah blah blah\n\n(reallyReallyReallyReallyReallyReallyReallyLong ||\n// blah blah blah\n\nreallyReallyReallyReallyReallyReallyReallyLong)"
  , "return (reallyReallyReallyReallyReallyReallyReallyLong || reallyReallyReallyReallyReallyReallyReallyLong) && (reallyReallyReallyReallyReallyReallyReallyLong || reallyReallyReallyReallyReallyReallyReallyLong)"
  , "--x"
  , "-(-x)"
  , "-+x"
  , "-(+x)"
  , "!!x"
  , "!(!x)"
  , "!o.p"
  , "(!o).p"
  , "o  .  p"
  , "o\n.p"
  , "o.\np"
  , "o.p.q"
  , "reallyReallyReallyReallyReallyReallyReallyLong.reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong.reallyReallyReallyReallyReallyReallyReallyLong.reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong.p"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong.p.q"
  , "reallyReallyReallyReallyReallyReallyReallyLong.reallyReallyReallyReallyReallyReallyReallyLong.p"
  , "reallyReallyReallyReallyReallyReallyReallyLong.reallyReallyReallyReallyReallyReallyReallyLong.p.q"
  , "reallyReallyReallyReallyReallyReallyReallyLong.p.reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong.p.q.reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong.p.reallyReallyReallyReallyReallyReallyReallyLong.q"
  , "foo.bar.qux.lit.foo.bar.qux.lit.foo.bar.qux.lit.foo.bar.qux.lit.foo.bar.qux.lit.foo.bar.qux.lit"
  , "(a + b).c"
  , "let x = y"
  , "let x = y;"
  , "let    x    =    y;"
  , "let\nx\n=\ny;"
  , "a // a\n+ // +\nb // b"
  , "// a\na\n// +\n+\n// b\nb"
  , "// a\na\n/**/ // +\n+\n// b\nb"
  , "// a\na\n// +\n\n+\n// b\nb"
  , "// a\n\na\n// +\n+\n// b\nb"
  , "a /* a */ + /* + */ b /* b */"
  , "/* a */ a\n/* + */ +\n/* b */ b"
  , "a /* a\n */ + /* +\n */ b /* b\n */"
  , "/* a\n */ a\n/* +\n */ +\n/* b\n */ b"
  , "/* blah blah blah */ let x = y;"
  , "/* blah blah blah */\nlet x = y;"
  , "/*\n * blah blah blah\n */\nlet x = y;"
  , "let x = y; /* blah blah blah */"
  , "let x = y;   /* blah blah blah */"
  , "let x = y   /* blah blah blah */   ;"
  , "let x = y;\n/* blah blah blah */"
  , "let x = y;\n/*\n * blah blah blah\n */"
  , "/**/ a + b"
  , "a /**/ + b"
  , "a + /**/ b"
  , "a + b /**/"
  , "/*\n*/ a + b"
  , "a /*\n*/ + b"
  , "a + /*\n*/ b"
  , "a + b /*\n*/"
  , "/*\n*/ reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong /*\n*/ + reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong + /*\n*/ reallyReallyReallyReallyReallyReallyReallyLong"
  , "reallyReallyReallyReallyReallyReallyReallyLong + reallyReallyReallyReallyReallyReallyReallyLong /*\n*/"
  , "//\na"
  , "a //"
  , "a // a\n+ b // b"
  , "a + // b\nb"
  , "a +\n// b\nb"
  , "f(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa);"
  , "f(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa);"
  , "f(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa);"
  , "f(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa);"
  , "f(𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷);"
  , "f(𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷a);"
  , "f(𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷);"
  , "f(𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷a);"
  , "f(a /**/);"
  , "f(a /*\n*/);"
  , "f(a) /*\n*/;"
  , "f(\na // a\n+ // +\nb // b\n)"
  , "f(\n// a\na\n// +\n+\n// b\nb\n)"
  , "f(a + // b\nb)"
  , "f(a +\n// b\nb)"
  , "f(\na + b // blah blah blah\n)"
  , "f(/**/a)"
  , "f(a/**///\n)"
  , "f(\n//\na\n)"
  , "f(\n/**/ //\na\n)"
  , "f(\n/**/    //\na\n)"
  , "let x = 😈"
  , "let x =\n  😈"
  , "let x =\n\n\n😈"
  , "let x =     \n😈"
  , "let   x y; let   x=y;"
  , "let   x=y; let   x y;"
  , "(a + b let x = y;"
  , "let x = y; /**/ //"
  , "let x = y; /**/   //"
  , "/**/ //\nlet x = y;"
  , "/**/   //\nlet x = y;"
  , "/**/ //\n\n\nlet x = y;"
  , "//"
  , "//\n//"
  , "\n//\n//"
  , "\n\n//\n//"
  , "//\n//\n"
  , "//\n\n//\n"
  , "/**/ //"
  , "//\n/**/ //"
  , "\n/**/ //\n//"
  , "\n\n/**/ //\n//"
  , "//\n/**/ //\n"
  , "//\n\n/**/ //\n"
  , "/**/ /**/"
  , "/**/ //\n\n\n"
  , "/**/ //\n\n\n//\n"
  , "let x = y;\n//"
  , "let x = y;\n\n//"
  , "let x = y;\n\n\n//"
  , "let x = y;\n/**/"
  , "let x = y;\n\n/**/"
  , "let x = y;\n\n\n/**/"
  , "let x = y;\n//\nlet x = y;"
  , "let x = y;\n\n//\nlet x = y;"
  , "let x = y;\n\n\n//\nlet x = y;"
  , "let x = y;\n//\n\nlet x = y;"
  , "let x = y;\n//\n\n\nlet x = y;"
  , "let x = y;\n\n//\n\nlet x = y;"
  , "let x = y;\nlet x = y;"
  , "let x = y;\n\nlet x = y;"
  , "let x = y;\n\n\nlet x = y;"
  , "let x = y;\n"
  , "let x = y;\n\n"
  , "let x = y;\n\n\n"
  , "\nlet x = y;"
  , "\n\nlet x = y;"
  , "\n\n\nlet x = y;"
  , "let x = y;\n/**/ //"
  , "let x = y;\n/**/ //\n"
  , "let x = y;\n/**/ //\n\n"
  , "let x = y;\n/**/ //\n\n\n"
  , "let x = y;\n/**/ /**/"
  , "let x = y;\n/**/ /**/\n"
  , "let x = y;\n/**/ /**/\n\n"
  , "let x = y;\n/**/ /**/\n\n\n"
  , "let x = y;\n😈 let x = y;"
  , "let x = y;\n\n😈 let x = y;"
  , "let x = y;\n\n\n😈 let x = y;"
  , "let x = y; 😈\nlet x = y;"
  , "let x = y; 😈\n\nlet x = y;"
  , "let x = y; 😈\n\n\nlet x = y;"
  , "let x = y;\n😈\nlet x = y;"
  , "let x = y;\n\n😈\n\nlet x = y;"
  , "let x = y;\n😈"
  , "let x = y;\n\n😈"
  , "let x = y;\n\n\n😈"
  , "let x = y;\n😈\n\n\n\n\nlet x = y;"
  , "let x = a +\n//\nb"
  , "return"
  , "return; //"
  , "return a +\n//\nb"
  , "let x = y\n//\n;"
  , "let x = y\n//\n\n;"
  , "return a + b"
  , "/**/\n//\nlet x = y;"
  , "//\n/**/\nlet x = y;"
  , "/**/\n\n//\n\nlet x = y;"
  , "//\n\n/**/\n\nlet x = y;"
  , "a\n/**/\n+ b"
  , "a +\n/**/\nb"
  , "a\n//\n+ b"
  , "a +\n//\nb"
  , "/**/ /**/\nlet x = y;"
  , "f()"
  , "f(/**/)"
  , "f(/**/)"
  , "f(\n//\n)"
  , "f(a)"
  , "f(a, b)"
  , "f(a, b, c)"
  , "f(a,)"
  , "f(a, b,)"
  , "f(a, b, c,)"
  , "f(\na //\n)"
  , "f(\na, //\n)"
  , "f(a, /**/)"
  , "f(a, //\n)"
  , "f(a, //\n\n)"
  , "f(a\n/**/ ,)"
  , "f(a\n/**/\n,)"
  , "f(a\n/**/\n\n,)"
  , "f(a\n//\n,)"
  , "f(a\n\n//\n\n,)"
  , "f(a\n//\n//\n,)"
  , "f(a\n\n//\n\n//\n\n,)"
  , "f(reallyReallyReallyReallyReallyReallyReallyLong)"
  , "f(reallyReallyReallyReallyReallyReallyReallyLong, reallyReallyReallyReallyReallyReallyReallyLong)"
  , "f(reallyReallyReallyReallyReallyReallyReallyLong, reallyReallyReallyReallyReallyReallyReallyLong, reallyReallyReallyReallyReallyReallyReallyLong)"
  , "f(a, b, c,)"
  , "f(a, b, c, /**/)"
  , "f(do{})"
  , "f(do{x})"
  , "f(do{f(\n//\na)})"
  , "f(do{a;b})"
  , "f(do{let x = y})"
  , "f(do{return x})"
  , "f(do{let x = y; let z = y; z})"
  , "f(do{😈;})"
  , "f(do{let  x = y;😈})"
  , "f(do{let  x = y;😈  let   x = y})"
  , "f(do{let  x = y;😈; let   x = y})"
  , ";"
  , ";;"
  , ";\n;"
  , ";\n\n;"
  , "let x = y;;"
  , "let x = y;\n;"
  , "let x = y;\n\n;"
  , "let x = y;; let x = y;"
  , "let x = y;;\nlet x = y;"
  , "let x = y;;\n\nlet x = y;"
  , "let x = y;\n; let x = y;"
  , "let x = y;\n\n; let x = y;"
  , "let x = y;\n;\nlet x = y;"
  , "let x = y;\n;\n\nlet x = y;"
  , "let x = y;\n\n;\nlet x = y;"
  , "let x = y;\n//\n;\nlet x = y;"
  , "let x = y;\n\n//\n;\nlet x = y;"
  , "let x = y;\n\n//\n\n;\n\nlet x = y;"
  , "// Hello, world!\n;"
  , "// Hello, world!\n\n;"
  , "// Hello, world!\n\n\n;"
  , "; // Hello, world!"
  , "// Hello, world!\n;\nlet x = y;"
  , "// Hello, world!\n;\n\nlet x = y;"
  , "// Hello, world!\n\n;\nlet x = y;"
  , "// Hello, world!\n\n;\n\nlet x = y;"
  , "do {\n//\n}"
  , "do {\n//\n//\n}"
  , "do {\n// a\n// b\n}"
  , "do {\n//\n\n//\n}"
  , "do {\n//\n\n//\n\n}"
  , "do {\n//\n//\n\n}"
  , "do {\n\n//\n//\n}"
  , "do {x\n//\n}"
  , "do {x\n//\n//\n}"
  , "do {x\n//\n\n//\n}"
  , "do {x\n//\n\n//\n\n}"
  , "do {x\n//\n//\n\n}"
  , "do {x\n\n//\n//\n}"
  , "f(\n//\n)"
  , "f(\n//\n//\n)"
  , "f(\n//\n\n//\n)"
  , "f(\n//\n\n//\n\n)"
  , "f(\n//\n//\n\n)"
  , "f(\n\n//\n//\n)"
  , "f(x\n//\n)"
  , "f(x\n//\n//\n)"
  , "f(x\n//\n\n//\n)"
  , "f(x\n//\n\n//\n\n)"
  , "f(x\n//\n//\n\n)"
  , "f(x\n\n//\n//\n)"
  , "do {/**/}"
  , "do {/**/\n}"
  , "do {\n/**/\n}"
  , "do {let x = y;\n\nlet x = y;}"
  , "f(x,\n//\ng(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)\n);"
  , "f(x,\n//\ng(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)\n);"
  , "f(x,\n//\ng(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)\n);"
  , "f(x,\n//\ng(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)\n);"
  , "f(x,\n//\ng(𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷)\n);"
  , "f(x,\n//\ng(𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷a)\n);"
  , "f(x,\n//\ng(𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷)\n);"
  , "f(x,\n//\ng(𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷𐐷a)\n);"
  , "do {\n//\nlet x = y;}"
  , "do {\n//\n\nlet x = y;}"
  , "f(\n  // b\n  a + \n  b\n);"
  , "do {}; //"
  , "do { x; }; //"
  , "do { let x = y; }; //"
  , "do {} //"
  , "//\ndo {}"
  , "let x = f(\n//\n);"
  , "(do {});"
  , "//\n(a + b) * c;"
  , "//\n\n(a + b) * c;"
  , "(a + b) //\n * c;"
  , "//\n((a + b)) * c;"
  , "//\n\n((a + b)) * c;"
  , "((a + b)) //\n * c;"
  , "//\na + b"
  , "(a +\n//\nb) * c"
  , "let x = a + b;"
  , "let x =\n//\na + b;"
  , "let x = a +\n//\nb;"
  , "return (\n//\na + b);"
  , "return (a +\n//\nb);"
  , "a + b +\n//\nc + d"
  , "//\na + b + c + d"
  , "- // -\nx;"
  , "- // -\n\nx;"
  , "- // -\n\n\nx;"
  , "a\n//\n;"
  , "a /**/;"
  , "a //\n;"
  , "(x T)"
  , "/* b */ x /* c */"
  , "(/* b */ x /* c */)"
  , "/* a */ (/* b */ x /* c */) /* d */;"
  , "/* a */ (/* b */ x /* c */\n//\n) /* d */;"
  , "/* a */ (/* b */\n//\nx /* c */) /* d */;"
  , "/* a */ (\n//\n/* b */ x /* c */) /* d */;"
  , "/**/ x /**/"
  , "/**/ x; /**/"
  , "/**/ x /**/;"
  , "/**/ x /**/; /**/"
  , "x //"
  , "x; //"
  , "x\n//\n"
  , "x\n//\n;"
  , "/**/ ; /**/"
  , "//\n\n/**/;"
  , "   😈"
  , "😈   "
  , "😈\n    \n😈"
  , "😈\n//    \n😈"
  , "😈\n/*\n    \n*/\n😈"
  , "😈\n/*\n    \n */\n😈"
  , "😈\n/*\n    */\n😈"
  , "😈 /*   "
  , "; /*   "
  , "// a\n// b"
  , "// a\n// b\nx"
  , "/* a */ /* b */ x"
  , "x /* a */ /* b */"
  , "/* a */ /* b */"
  , "/**/-x"
  , "-/**/x"
  , "-x/**/"
  , "-/**/x/**/"
  , "-\n/**/x"
  , "/* a */ -\n/* b */x"
  , "/* a */ -/* b */x"
  , "-(x /* a */) /* b */"
  , "let x = y /* a */ /* b */"
  , "return x /* a */ /* b */"
  , "break x /* a */ /* b */"
  , "let x = -y /* a */ /* b */"
  , "return -x /* a */ /* b */"
  , "break -x /* a */ /* b */"
  , "/* a */ do /* b */ { /* c */ } /* d */"
  , "/* a */ do\n/* b */ { /* c */ } /* d */"
  , "loop{  };"
  , "a\n/**/ + /**/ b"
  , "a +\n/**/ b"
  , "a +\n// x\n\n// y\nb"
  , "a +\n\n// x\n// y\nb"
  , "a +\n\n\n// x\n// y\nb"
  , "o\n// a\n.p"
  , "o\n\n// a\n.p"
  , "o\n// a\n\n.p"
  , "o\n// a\n// b\n.p"
  , "o\n// a\n\n// b\n.p"
  , "foo.bar\n// blah blah blah\n.qux.lit"
  , "f(\n//\na)"
  , "f(a\n//\n)"
  , "f(\n//\na, b)"
  , "f(a, b\n//\n)"
  , "f(\n//\na, /* a */\nb, /* b */)"
  , "f(\n//\na, /* a */)"
  , "f(\n\n//\na)"
  , "f(\n\n//\na, b)"
  , "return(\n//\na)"
  , "return(\n//\na + b)"
  , "return(a +\n//\nb)"
  , "return(\n//\na +\n//\nb)"
  , "return(a +\n//\nb/**/)"
  , "return(a +\n//\nb//\n)"
  , "return(\n\n\n//\na)"
  , "x;\nreturn y;"
  , "x;\n\nreturn y;"
  , "x;\nreturn (\n//\ny);"
  , "x;\nreturn (\n//\n\ny);"
  , "x;\n\nreturn (\n//\ny);"
  , "x;\n\nreturn (\n//\n\ny);"
  , "x;\n\nreturn (\n\n//\ny);"
  , "return (\n// a\n// b\ny);"
  , "return (\n// a\n\n// b\ny);"
  , "return (\n\n// a\n// b\ny);"
  , "return (\n\n// a\n\n// b\n\ny);"
  , "f(a/**/,b);"
  , "f(a/**/,\n//\nb);"
  , "f(-a/**/,b);"
  , "f(-a/**/,\n//\nb);"
  , "return x /**/"
  , "break x /**/"
  , "return x /* really really really really really really really really really long */"
  , "break x /* really really really really really really really really really long */"
  , "return x //"
  , "break x //"
  , "return x // really really really really really really really really really really long"
  , "break x // really really really really really really really really really really long"
  , "return reallyReallyReallyReallyReallyReallyReallyReallyReallyReallyReallyReallyLong;"
  , "break reallyReallyReallyReallyReallyReallyReallyReallyReallyReallyReallyReallyLong;"
  , "return x + y /* really really really really really really really really really long */"
  , "break x + y /* really really really really really really really really really long */"
  , "return x + y /* */"
  , "break x + y /* */"
  , "f/**/()"
  , "o/**/.p"
  , "o./**/p"
  , "/* a */ f/* b */()"
  , "/* a */ o/* b */.p"
  , "/* a */ o./* b */p"
  , "//\n\nx;"
  , "//\n\n/**/ x;"
  , "//\n\n/**/ /**/ x;"
  , "//\n\rx;"
  , "do { x } /* a */ /* b */"
  , "do { x /* a */ } /* b */"
  , "do { x /* a */ /* b */ }"
  , "f(x) /* a */ /* b */"
  , "f(x /* a */) /* b */"
  , "f(x /* a */ /* b */)"
  , "f(x\n/* a */ ) /* b */"
  , "g(\n//\nf(x) /* a */ /* b */)"
  , "g(\n//\nf(x /* a */) /* b */)"
  , "g(\n//\nf(x /* a */ /* b */))"
  , "g(y,\n//\nf(x) /* a */ /* b */)"
  , "g(y,\n//\nf(x /* a */) /* b */)"
  , "g(y,\n//\nf(x /* a */ /* b */))"
  , "g(\n//\nf(x), /* a */ /* b */\ny)"
  , "g(\n//\nf(x /* a */), /* b */\ny)"
  , "g(\n//\nf(x /* a */ /* b */),\ny)"
  , "g(\n//\nf(x) /* a */ /* b */,y)"
  , "g(\n//\nf(x /* a */) /* b */,y)"
  , "g(\n//\nf(x /* a */ /* b */),y)"
  , "g(\n//\nf(x), /* a */ /* b */y)"
  , "g(\n//\nf(x /* a */), /* b */y)"
  , "g(\n//\nf(x /* a */ /* b */),y)"
  , "let x = y /**/"
  , "let x =\n//\ny /**/"
  , "if x {}"
  , "if x   {}"
  , "if   x {}"
  , "if f() {}"
  , "if x {} else {}"
  , "if x {} else if y {}"
  , "if x {} else if y {} else {}"
  , "if x {} else if y {} else if z {}"
  , "if x {} else if y {} else if z {} else {}"
  , "if x {} else if reallyReallyReallyReallyReallyReallyReallyReallyReallyLong {} else {}"
  , "if x { a } else if reallyReallyReallyReallyReallyReallyReallyReallyReallyLong { a } else { a }"
  , "if x { a } else if reallyReallyReallyReallyReallyReallyReallyReallyReallyLong {} else {}"
  , "if x {} else if reallyReallyReallyReallyReallyReallyReallyReallyReallyLong { a } else {}"
  , "if x {} else if reallyReallyReallyReallyReallyReallyReallyReallyReallyLong {} else { a }"
  , "if x { a } else if reallyReallyReallyReallyReallyReallyReallyReallyReallyLong { a } else {}"
  , "if x { a } else if reallyReallyReallyReallyReallyReallyReallyReallyReallyLong {} else { a }"
  , "if x {} else if reallyReallyReallyReallyReallyReallyReallyReallyReallyLong { a } else { a }"
  , "if x {\na\n} else { b }"
  , "if x { let x = y } else { b }"
  , "if x { a } else { let x = y }"
  , "if x {\n//\na\n} else { b }"
  , "if x { a } else {\n//\nb\n}"
  , "if x {\n//\na\n} else {\n//\nb\n}"
  , "if x { let x = y } else { let x = y }"
  , "let a = if x { b } else { c };"
  , "let a = if x {\n//\nb } else { c };"
  , "if x {};"
  , "if f(\n//\na) {}"
  , "if f(\n//\na) { x }"
  , "if f(\n//\na) {} else {}"
  , "if f(\n//\na) {} else { x }"
  , "if f(\n//\na) { x } else { x }"
  , "if a && b {}"
  , "if a &&\n//\nb {}"
  , "if a &&\n//\nb { x }"
  , "if a &&\n//\nb {} else {}"
  , "if a &&\n//\nb {} else { x }"
  , "if a &&\n//\nb { x } else { x }"
  , "if a &&\n//\nb { x } else if y { x } else { x }"
  , "if\n//\na && b {}"
  , "if\n//\na && b { x }"
  , "if\n//\na && b {} else {}"
  , "if\n//\na && b { x } else {}"
  , "if\n//\na && b {} else { x }"
  , "if a {}\n//\nelse {}"
  , "if a {}\n\n//\nelse {}"
  , "if a {}\n//\n\nelse {}"
  , "if a {}\n\n//\n\nelse {}"
  , "if a {}\n//\nelse if b {}"
  , "if a {}\n\n//\nelse if b {}"
  , "if a {}\n//\n\nelse if b {}"
  , "if a {}\n//\nelse if b {}\n//\nelse {}"
  , "if a {}\n\n//\nelse if b {}\n//\nelse {}"
  , "if a {}\n//\n\nelse if b {}\n//\nelse {}"
  , "if a {}\n//\nelse if b {}\n\n//\nelse {}"
  , "if a {}\n//\nelse if b {}\n//\n\nelse {}"
  , "\n//\nif x {}"
  , "if\n//\nx {}"
  , "if\n//\nx && y {}"
  , "if\n//\nx &&\n//\ny {}"
  , "\n\n//\nif x {}"
  , "if\n\n//\nx {}"
  , "if\n\n//\nx && y {}"
  , "if\n\n//\nx &&\n//\ny {}"
  , "if z {} else\n//\nif x {}"
  , "if z {} else if\n//\nx {}"
  , "if z {} else if\n//\nx && y {}"
  , "if z {} else if\n//\nx &&\n//\ny {}"
  , "if z {} else\n\n//\nif x {}"
  , "if z {} else if\n\n//\nx {}"
  , "if z {} else if\n\n//\nx && y {}"
  , "if z {} else if\n\n//\nx &&\n//\ny {}"
  , "{}"
  , "{\n//\n}"
  , "{\n//\n//\n}"
  , "{\n//\n\n//\n}"
  , "{\n\n//\n}"
  , "{\n//\n\n}"
  , "{\n\n//\n\n}"
  , "{a}"
  , "{a, b}"
  , "{a, b, c}"
  , "{a,}"
  , "{a, b,}"
  , "{a, b, c,}"
  , "{\n//\na}"
  , "{\n//\na, b}"
  , "{\n//\na, b, c}"
  , "{a\n//\n}"
  , "{a\n//\n, b}"
  , "{a\n//\n, b, c}"
  , "{a,\n//\nb}"
  , "{a,\n//\nb, c}"
  , "{a, b\n//\n}"
  , "{a, b\n//\n, c}"
  , "{a, b,\n//\nc}"
  , "{a, b, c\n//\n}"
  , "{p: a}"
  , "{p: a, q: b}"
  , "{p, q: b}"
  , "{p: a, q}"
  , "{p:\n//\na}"
  , "{p:\n//\na, q: b}"
  , "{q: b, p:\n//\na}"
  , "{p: a +\n//\nb}"
  , "{p:\n//\na +\n//\nb}"
  , "{p: f(\n//\na)}"
  , "{/**/ p}"
  , "{p /**/}"
  , "{/**/ p: a}"
  , "{p /**/: a}"
  , "{p: /**/ a}"
  , "{p: a /**/}"
  , "{/**/ p,}"
  , "{p /**/,}"
  , "{p, /**/}"
  , "{/**/ p: a,}"
  , "{p /**/: a,}"
  , "{p: /**/ a,}"
  , "{p: a, /**/}"
  , "{p: a /**/,}"
  , "{q /**/, p}"
  , "{q, /**/ p}"
  , "{q, p /**/}"
  , "{q /**/, p: a}"
  , "{q, /**/ p: a}"
  , "{q, p /**/: a}"
  , "{q, p: /**/ a}"
  , "{q, p: a /**/}"
  , "{q /**/, p,}"
  , "{q, /**/ p,}"
  , "{q, p /**/,}"
  , "{q, p, /**/}"
  , "{q /**/, p: a,}"
  , "{q, /**/ p: a,}"
  , "{q, p /**/: a,}"
  , "{q, p: /**/ a,}"
  , "{q, p: a, /**/}"
  , "{q, p: a /**/,}"
  , "{q: b /**/, p}"
  , "{q: b, /**/ p}"
  , "{q: b, p /**/}"
  , "{q: b /**/, p: a}"
  , "{q: b, /**/ p: a}"
  , "{q: b, p /**/: a}"
  , "{q: b, p: /**/ a}"
  , "{q: b, p: a /**/}"
  , "{q: b /**/, p,}"
  , "{q: b, /**/ p,}"
  , "{q: b, p /**/,}"
  , "{q: b, p, /**/}"
  , "{q: b /**/, p: a,}"
  , "{q: b, /**/ p: a,}"
  , "{q: b, p /**/: a,}"
  , "{q: b, p: /**/ a,}"
  , "{q: b, p: a, /**/}"
  , "{q: b, p: a /**/,}"
  , "{\n//\n/**/ p}"
  , "{\n//\np /**/}"
  , "{\n//\n/**/ p: a}"
  , "{\n//\np /**/: a}"
  , "{\n//\np: /**/ a}"
  , "{\n//\np: a /**/}"
  , "{\n//\n/**/ p,}"
  , "{\n//\np /**/,}"
  , "{\n//\np, /**/}"
  , "{\n//\n/**/ p: a,}"
  , "{\n//\np /**/: a,}"
  , "{\n//\np: /**/ a,}"
  , "{\n//\np: a, /**/}"
  , "{\n//\np: a /**/,}"
  , "{\n//\np: a, q: b}"
  , "{p: {a: b}, q: {b: c}}"
  , "{\n//\np: {a: b}, q: {b: c}}"
  , "{p: {\n//\na: b}, q: {b: c}}"
  , "{q, /**/ p}"
  , "{q,\n/**/ p}"
  , "{\n//\nq, /**/ p}"
  , "{\n//\nq,\n/**/ p}"
  , "f(a,\n/**/ b)"
  , "f(\n//\na, /**/ b)"
  , "{\n//\na, /**/ b}"
  , "f(a /**/, b)"
  , "{a /**/, b}"
  , "f(\n//\na /**/, b)"
  , "{\n//\na /**/, b}"
  , "{p: p}"
  , "{p: (p)}"
  , "{p:\n//\np}"
  , "{p:\n//\n(p)}"
  , "{p:(\n//\np)}"
  , "{p | o}"
  , "{p: a | o}"
  , "{p, q | o}"
  , "{p: a, q | o}"
  , "{p, q: b | o}"
  , "{p: a, q: b | o}"
  , "{\n//\np: a, q: b | o}"
  , "{p: a, q: b |\n//\no}"
  , "{p: a, q: b |\n\n//\no}"
  , "{p: a, q: b |\n//\n\no}"
  , "{p: a, q: b |\n\n//\n\no}"
  , "{p: a, q: b |\n//\nx + y}"
  , "{p: a, q: b | x +\n//\ny}"
  , "{p: a, q: b |\n//\nx +\n//\ny}"
  , "do { {\n//\np: a, q: b | o} }"
  , "{|o}"
  , "{|\n//\no}"
  , "{/* a */ | /* b */ o}"
  , "{\n/* a */ | /* b */ o}"
  , "{/* a */ | /* b */\no}"
  , "{\n/* a */ | /* b */\no}"
  , "{p, q /* a */ | /* b */ o}"
  , "{p, q, /* a */ | /* b */ o}"
  , "{p, q /* a */ | /* b */\no}"
  , "{p, q, /* a */ | /* b */\no}"
  , "{p, q\n/* a */ | /* b */ o}"
  , "{p, q,\n/* a */ | /* b */ o}"
  , "{p, q\n/* a */ | /* b */\no}"
  , "{p, q,\n/* a */ | /* b */\no}"
  , "let _ = x;"
  , "let {} = x;"
  , "let {p, q} = x;"
  , "let {p: a, q} = x;"
  , "let {p, q: b} = x;"
  , "let {p: a, q: b} = x;"
  , "let {\n//\np, q} = x;"
  , "let {\n//\np: a, q} = x;"
  , "let {\n//\np, q: b} = x;"
  , "let {\n//\np: a, q: b} = x;"
  , "let {p/* a */,/* b */ q} = x;"
  , "let {p: x/* a */,/* b */ q} = x;"
  , "let {\n//\np/* a */,/* b */ q} = x;"
  , "let {\n//\np: x/* a */,/* b */ q} = x;"
  , "let {p, q | o} = x;"
  , "let {p, q | _} = x;"
  , "let {\n//\np, q | _} = x;"
  , "let {p, q |\n//\n_} = x;"
  , "let {| _} = x;"
  , "let {/* a */|/* b */ _} = x;"
  , "let {p: p} = x;"
  , "let {p: (p)} = x;"
  , "let {p:\n//\na} = x;"
  , "let {q: b, p:\n//\na} = x;"
  , "let {\n//\nq /**/} = x;"
  , "let {\n//\nq, /**/} = x;"
  , "let {\n//\nq /**/,} = x;"
  , "(x: T)"
  , "let x: T = y"
  , "(f(\n//\n): T)"
  , "(a +\n//\nb: T)"
  , "let {\n//\n}: T = y"
  , "/* a */ (/* b */ x /* c */ : /* d */ T /* e */) /* f */"
  , "/* a */ let /* b */ x /* c */ : /* d */ T /* e */ = /* f */ y /* g */ ; /* h */"
  , "(x: !)"
  , "(x: {})"
  , "(x: {p: T})"
  , "(x: {p: T, q: U})"
  , "(x: {p: T,})"
  , "(x: {p: T, q: U,})"
  , "(x: {| T})"
  , "(x: {q: U | T})"
  , "(x: {q: U, | T})"
  , "(x: {\n//\nq: U | T})"
  , "(x: {q: U |\n//\nT})"
  , "(x: {q: U |T\n//\n})"
  , "(x: <> T)"
  , "(x: <T> T)"
  , "(x: <T, U> T)"
  , "(x: <T,> T)"
  , "(x: <T, U,> T)"
  , "(x: <T: U> T)"
  , "(x: <T = U> T)"
  , "(x: <A: T, B> T)"
  , "(x: <A, B: T> T)"
  , "(x: <A = T, B> T)"
  , "(x: <A, B = T> T)"
  , "(x: <A: T, B: U> T)"
  , "(x: <A = T, B = U> T)"
  , "(x: <A: T, B = U> T)"
  , "(x: <A = T, B: U> T)"
  , "(x: <\n//\n> T)"
  , "(x: <\n//\nT> T)"
  , "(x: <\n//\nT, U> T)"
  , "(x: <T:\n//\nT> T)"
  , "(x: </* a */ T /* b */, /* c */ U> T)"
  , "(x: </* a */ T: U /* b */, /* c */ V> T)"
  , "(x: <\n//\n/* a */ T /* b */, /* c */ U> T)"
  , "(x: <\n//\n/* a */ T: U /* b */, /* c */ V> T)"
  , "(x: <T: <T> T /**/> T)"
  , "(x: <\n//\nT: <T> T /**/> T)"
  ]

openSnapshotFile :: IO Handle
openSnapshotFile = do
  h <- openFile "test/Brite/Syntax/PrinterSpecSnapshot.md" WriteMode
  hPutStrLn h "# PrinterSpecSnapshot"
  return h

closeSnapshotFile :: Handle -> IO ()
closeSnapshotFile h = do
  hPutStrLn h ""
  hPutStrLn h (replicate 80 '-')
  hClose h

spec :: Spec
spec = beforeAll openSnapshotFile $ afterAll closeSnapshotFile $ do
  flip mapM_ testData $ \input ->
    it (Text.unpack (escape input)) $ \h ->
      let
        (inputModule, diagnostics) = runDiagnosticWriter (parseModule (tokenize input))
        output = Text.Lazy.toStrict $ Text.Builder.toLazyText $
          printModule (PrinterAST.convertModule inputModule)
        (outputModule, _) = runDiagnosticWriter (parseModule (tokenize output))
        reprintedOutput = Text.Lazy.toStrict $ Text.Builder.toLazyText $
          printModule (PrinterAST.convertModule outputModule)
      in seq output $ do
        hPutStrLn h ""
        hPutStrLn h (replicate 80 '-')
        hPutStrLn h ""
        hPutStrLn h "### Input"
        hPutStrLn h "```ite"
        hPutStrLn h (Text.unpack input)
        hPutStrLn h "```"
        hPutStrLn h ""
        hPutStrLn h "### Output"
        hPutStrLn h "```"
        hPutStr h (Text.unpack output)
        hPutStrLn h "```"
        if null diagnostics then return () else (do
          hPutStrLn h ""
          hPutStrLn h "### Errors"
          flip mapM_ diagnostics (\diagnostic ->
            hPutStrLn h (Text.Lazy.unpack (Text.Builder.toLazyText
              (Text.Builder.fromText "- " <> debugDiagnostic diagnostic)))))

        -- Test that when we parse and re-print the output we get the same thing.
        reprintedOutput `shouldBe` output

        -- Test to make sure the output has no trailing spaces, but only if there were no
        -- diagnostics. If there were parse errors then we may print raw text with trailing spaces.
        (Text.foldl'
          (\s c ->
            case c of
              '\n' | s -> error "Has trailing spaces."
              '\r' | s -> error "Has trailing spaces."
              ' ' -> True
              _ -> False)
          False
          output)
            `shouldBe` False

escape :: Text -> Text
escape = Text.concatMap
  (\c ->
    case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ -> Text.singleton c)
