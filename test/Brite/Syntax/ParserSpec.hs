{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.ParserSpec (spec) where

import Brite.Diagnostics
import Brite.Syntax.CST
import Brite.Syntax.Parser
import Brite.Syntax.ParserFramework
import Brite.Syntax.Tokens
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import System.IO
import Test.Hspec

testData :: [T.Text]
testData =
  [ "let x = y;"
  , "let x = y"
  , "let"
  , "let x"
  , "let ="
  , "let y"
  , "let ;"
  , "let x ="
  , "let x y"
  , "let x ;"
  , "let = y"
  , "let = ;"
  , "😈 let x = y;"
  , "let 😈 x = y;"
  , "let x 😈 = y;"
  , "let x = 😈 y;"
  , "let x = y 😈;"
  , "let x = y; 😈"
  , "let x = y 😈"
  , ") let x = y;"
  , "let ) x = y;"
  , "let x ) = y;"
  , "let x = ) y;"
  , "let x = y );"
  , "let x = y; )"
  , "let x = y )"
  , "let 😈 = y;"
  , "let x 😈 y;"
  , "let x = 😈;"
  , "let 😈 y;"
  , "let 😈 =;"
  , "let x 😈;"
  , "let = 😈;"
  , "let 😈;"
  , "let ) = y;"
  , "let x ) y;"
  , "let x = );"
  , "let ) y;"
  , "let ) =;"
  , "let x );"
  , "let = );"
  , "let );"
  , "x"
  , "x;"
  , "😈 x"
  , "x 😈"
  , "😈 x;"
  , "x 😈;"
  , "x; 😈"
  , "="
  , "😈"
  , ")"
  , ";"
  , "true"
  , "false"
  , "("
  , "(x"
  , "()"
  , "(x)"
  , "x)"
  , "(x;"
  , "let x = (y);"
  , "let x = (y;"
  , "let x = y; let x = y;"
  , "let x = y; let x = y; let x = y;"
  , "let x = y; let x = y; let x = y; let x = y;"
  , "let x = y let x = y"
  , "let x = y let x = y let x = y"
  , "let x = y let x = y let x = y let x = y"
  , "let x = y\nlet x = y"
  , "let x = y\nlet x = y\nlet x = y"
  , "let x = y\nlet x = y\nlet x = y\nlet x = y"
  , "😈 let x = y; let x = y; let x = y;"
  , "let x = y; 😈 let x = y; let x = y;"
  , "let x = y; let x = y; 😈 let x = y;"
  , "let x = y; let x = y; let x = y; 😈"
  , "😈 let x = y let x = y let x = y"
  , "let x = y 😈 let x = y let x = y"
  , "let x = y let x = y 😈 let x = y"
  , "let x = y let x = y let x = y 😈"
  , ") let x = y; let x = y; let x = y;"
  , "let x = y; ) let x = y; let x = y;"
  , "let x = y; let x = y; ) let x = y;"
  , "let x = y; let x = y; let x = y; )"
  , ") let x = y let x = y let x = y"
  , "let x = y ) let x = y let x = y"
  , "let x = y let x = y ) let x = y"
  , "let x = y let x = y let x = y )"
  , "do {}"
  , "do { "
  , "do }"
  , "do } do }"
  , "do"
  , "do do"
  , "do { let x = y; }"
  , "do { let x = y; "
  , "do let x = y; }"
  , "do let x = y }"
  , "do let x = y;"
  , "do let x = y"
  , "let x = (do {);"
  , "let x = (do { let y = z; );"
  , "let x = (do);"
  , "let x = (do let y = z; );"
  , "let x = (do { let y = z );"
  , "let x = (do { let y = );"
  , "do { let x = y; }"
  , "do { let x = y }"
  , "do { let x = y; let x = y; }"
  , "do { let x = y; let x = y; let x = y; }"
  , "do { let x = y; let x = y; let x = y; let x = y; }"
  , "do { let x = y let x = y }"
  , "do { let x = y let x = y let x = y }"
  , "do { let x = y let x = y let x = y let x = y }"
  , "do { let x = y\nlet x = y\n }"
  , "do { let x = y\nlet x = y\nlet x = y\n }"
  , "do { let x = y\nlet x = y\nlet x = y\nlet x = y\n }"
  , "do { 😈 let x = y; let x = y; let x = y; }"
  , "do { let x = y; 😈 let x = y; let x = y; }"
  , "do { let x = y; let x = y; 😈 let x = y; }"
  , "do { let x = y; let x = y; let x = y; 😈 }"
  , "do { 😈 let x = y let x = y let x = y }"
  , "do { let x = y 😈 let x = y let x = y }"
  , "do { let x = y let x = y 😈 let x = y }"
  , "do { let x = y let x = y let x = y 😈 }"
  , "do { ) let x = y; let x = y; let x = y; }"
  , "do { let x = y; ) let x = y; let x = y; }"
  , "do { let x = y; let x = y; ) let x = y; }"
  , "do { let x = y; let x = y; let x = y; ) }"
  , "do { ) let x = y let x = y let x = y }"
  , "do { let x = y ) let x = y let x = y }"
  , "do { let x = y let x = y ) let x = y }"
  , "do { let x = y let x = y let x = y ) }"
  , "let x = ) let x = )"
  , "let x = ) )"
  , ") let x = )"
  , "let x = ) ) let x = )"
  , "do do 😈"
  , "do do )"
  , "if x {}"
  , "if x { y }"
  , "if x {} else {}"
  , "if x { y } else {}"
  , "if x {} else { y }"
  , "if x { y } else { z }"
  , "if { let x = y }"
  , "if {}"
  , "if x }"
  , "if x {"
  , "if x"
  , "if {"
  , "if }"
  , "if {} else {}"
  , "if x } else {}"
  , "if x { else {}"
  , "if x else {}"
  , "if { else {}"
  , "if } else {}"
  , "if {} {}"
  , "if x } {}"
  , "if x { {}"
  , "if { {}"
  , "if } {}"
  , "if {} else }"
  , "if x } else }"
  , "if x { else }"
  , "if x else }"
  , "if { else }"
  , "if } else }"
  , "if {} else {"
  , "if x } else {"
  , "if x { else {"
  , "if x else {"
  , "if { else {"
  , "if } else {"
  , "if {} else"
  , "if x } else"
  , "if x { else"
  , "if x else"
  , "if { else"
  , "if } else"
  , "if {} {"
  , "if x } {"
  , "if x { {"
  , "if x {"
  , "if { {"
  , "if } {"
  , "if {} }"
  , "if x } }"
  , "if x { }"
  , "if x }"
  , "if { }"
  , "if } }"
  , "x 😈 😈 ;"
  , "x ) ) ;"
  , "let 😈 😈 x = y;"
  , "let ) ) x = y;"
  , "😈 😈"
  , ") )"
  , "if x {} { let y = z }"
  , "o.p"
  , "o.p.q"
  , "o."
  , "o.p."
  , "o..p"
  , "o p"
  , "o😈.p"
  , "o.😈p"
  , "o.😈p.q"
  , "o.p😈.q"
  , "o.p.😈q"
  , "o).p"
  , "o.)p"
  , "o.)p.q"
  , "o.p).q"
  , "o.p.)q"
  , "if x {} 😈"
  , "if x {} 😈 else {}"
  , "if x {} )"
  , "if x {} ) else {}"
  , "if 😈 x {}"
  , "if x 😈 {}"
  , "if ) x {}"
  , "if x ) {}"
  , "do {}.p"
  , "if x {}.p"
  , "if x {} else {}.p"
  , "f()"
  , "f ()"
  , "f😈()"
  , "f)()"
  , "f\n()"
  , "f;()"
  , "f\n😈()"
  , "f😈\n()"
  , "f\n)()"
  , "f)\n()"
  , "f(😈)"
  , "f("
  , "😈.p"
  , "(😈.p)"
  , "let 😈) = y;"
  , "let )😈 = y;"
  , "let 😈)x = y;"
  , "let )😈x = y;"
  , "let x = y 😈);"
  , "let x = y )😈;"
  , ")😈 let x = y;"
  , "😈) let x = y;"
  , "let x = y; )😈"
  , "let x = y; 😈)"
  , ")😈"
  , "😈)"
  , "let x = 🐶🐱 y;"
  , "if x {} 🐶🐱 else {}"
  , "if x {} 🐶🐱 else {"
  , "let x = y; do { let x = y; 😈 let x = y; } let x = y;"
  , "let x = 😈 let x = y"
  , "f(x)"
  , "f\n(x)"
  , "f;(x)"
  , "f(a)"
  , "f(a😈)"
  , "f(a})"
  , "f(a😈})"
  , "f(a}😈)"
  , "f(a😈,)"
  , "f(a},)"
  , "f(a😈},)"
  , "f(a}😈,)"
  , "f(a😈, b)"
  , "f(a}, b)"
  , "f(a😈}, b)"
  , "f(a}😈, b)"
  , "f(a😈, b.)" -- NOTE: `b.` is used here and below to enter “yield” mode for the expression.
  , "f(a}, b.)"
  , "f(a😈}, b.)"
  , "f(a}😈, b.)"
  , "f(a😈 b)"
  , "f(a} b)"
  , "f(a😈} b)"
  , "f(a}😈 b)"
  , "f(a😈 b.)"
  , "f(a} b.)"
  , "f(a😈} b.)"
  , "f(a}😈 b.)"
  , "f(a, b)"
  , "f(a, b😈)"
  , "f(a, b})"
  , "f(a, b}😈)"
  , "f(a, b😈})"
  , "f(a, b😈,)"
  , "f(a, b},)"
  , "f(a, b}😈,)"
  , "f(a, b😈},)"
  , "f(a, b😈, c)"
  , "f(a, b}, c)"
  , "f(a, b}😈, c)"
  , "f(a, b😈}, c)"
  , "f(a, b😈, c.)"
  , "f(a, b}, c.)"
  , "f(a, b}😈, c.)"
  , "f(a, b😈}, c.)"
  , "f(a, b😈 c)"
  , "f(a, b} c)"
  , "f(a, b}😈 c)"
  , "f(a, b😈} c)"
  , "f(a, b😈 c.)"
  , "f(a, b} c.)"
  , "f(a, b}😈 c.)"
  , "f(a, b😈} c.)"
  , "f(a.)"
  , "f(a.,)"
  , "f(a., b)"
  , "f(a. (b))"
  , "f(a. (b).)"
  , "f(a.😈)"
  , "f(a.})"
  , "f(a.😈})"
  , "f(a.}😈)"
  , "f(a, b.)"
  , "f(a, b.,)"
  , "f(a, b., c)"
  , "f(a, b. (c))"
  , "f(a, b. (c).)"
  , "f(a, b.😈)"
  , "f(a, b.})"
  , "f(a, b.😈})"
  , "f(a, b.}😈)"
  , "f(😈)"
  , "f(})"
  , "f(😈})"
  , "f(}😈)"
  , "f(a, 😈)"
  , "f(a, })"
  , "f(a, 😈})"
  , "f(a, }😈)"
  , "f(a b)"
  , "f(a, b c)"
  , "f(a b, c)"
  , "f(a b c)"
  , "f(a b, c, d)"
  , "f(a, b c, d)"
  , "f(a, b, c d)"
  , "f(a b c, d)"
  , "f(a, b c d)"
  , "f(a b, c d)"
  , "f(a b c d)"
  , "f(a,, b)"
  , "f(a,, b, c)"
  , "f(a, b,, c)"
  , "f(a,, b,, c)"
  , "f(a,, b, c, d)"
  , "f(a, b,, c, d)"
  , "f(a, b, c,, d)"
  , "f(a, b,, c,, d)"
  , "f(a,, b, c,, d)"
  , "f(a,, b,, c, d)"
  , "f(a,, b,, c,, d)"
  , "f(a, b, c, d,)"
  , "f(a, b, c, d,,)"
  , "f(a, b, c,,)"
  , "f(a, b,,)"
  , "f(a,,)"
  , "f(a, 😈, c)"
  , "f(😈, b, c)"
  , "f(a, b, 😈)"
  , "f(a, }, c)"
  , "f(}, b, c)"
  , "f(a, b, })"
  , "f(a, 😈}, c)"
  , "f(😈}, b, c)"
  , "f(a, b, 😈})"
  , "f(a, }😈, c)"
  , "f(}😈, b, c)"
  , "f(a, b, }😈)"
  , "f(a, b😈, c)"
  , "f(a😈, b, c)"
  , "f(a, b, c😈)"
  , "f(a, b}, c)"
  , "f(a}, b, c)"
  , "f(a, b, c})"
  , "f(a, b😈}, c)"
  , "f(a😈}, b, c)"
  , "f(a, b, c😈})"
  , "f(a, b}😈, c)"
  , "f(a}😈, b, c)"
  , "f(a, b, c}😈)"
  , "f(a, 😈b, c)"
  , "f(😈a, b, c)"
  , "f(a, b, 😈c)"
  , "f(a, }b, c)"
  , "f(}a, b, c)"
  , "f(a, b, }c)"
  , "f(a, 😈}b, c)"
  , "f(😈}a, b, c)"
  , "f(a, b, 😈}c)"
  , "f(a, }😈b, c)"
  , "f(}😈a, b, c)"
  , "f(a, b, }😈c)"
  , "f(a, 😈b}, c)"
  , "f(😈a}, b, c)"
  , "f(a, b, 😈c})"
  , "f(a, }b😈, c)"
  , "f(}a😈, b, c)"
  , "f(a, b, }c😈)"
  , "f(, a)"
  , "f(, a, b)"
  , "f(, a, b, c)"
  , "f(a,)"
  , "f(,)"
  , "f()"
  , "f(a)"
  , "f(a,)"
  , "f(a, b)"
  , "f(a, b,)"
  , "f(a, b, c)"
  , "f(a, b, c,)"
  , "let _ = x;"
  , "let x = _;"
  , "let _ = _;"
  , "fun() {}"
  , "(fun() {})"
  , "fun f() {}"
  , "fun(a) {}"
  , "(fun(a) {})"
  , "fun(a, b) {}"
  , "fun(a, b, c) {}"
  , "(fun(a, b) {})"
  , "(fun(a, b, c) {})"
  , "fun f(a) {}"
  , "fun f(a, b) {}"
  , "fun f(a, b, c) {}"
  , "fun(a,) {}"
  , "fun(a, b,) {}"
  , "fun(a, b, c,) {}"
  , "(fun(a,) {})"
  , "(fun(a, b,) {})"
  , "(fun(a, b, c,) {})"
  , "fun f(a,) {}"
  , "fun f(a, b,) {}"
  , "fun f(a, b, c,) {}"
  , "fun() { let x = y; }"
  , "(fun() { let x = y; })"
  , "fun f() { let x = y; }"
  , "fun() {}()"
  , "(fun() {}())"
  , "(fun() {})()"
  , "let f = fun() {};"
  , "let f = fun f() {};"
  , "let f = fun(a) {};"
  , "let f = fun(a, b) {};"
  , "let f = fun(a, b, c) {};"
  , "let f = fun f(a) {};"
  , "let f = fun f(a, b) {};"
  , "let f = fun f(a, b, c) {};"
  , "let f = fun(a,) {};"
  , "let f = fun(a, b,) {};"
  , "let f = fun(a, b, c,) {};"
  , "let f = fun f(a,) {};"
  , "let f = fun f(a, b,) {};"
  , "let f = fun f(a, b, c,) {};"
  , "let f = fun() { let x = y; };"
  , "let f = fun f() { let x = y; };"
  , "let f = fun() {}();"
  , "let f = (fun() {})();"
  , "fun f() {}"
  , "fun f) {}"
  , "fun f( {}"
  , "fun f( { let x = y }"
  , "fun f() }"
  , "fun f() {"
  , "fun) {}"
  , "fun( {}"
  , "fun() }"
  , "fun() {"
  , "fun(a, b let x = y; }"
  , "(fun) {})"
  , "(fun( {})"
  , "(fun() })"
  , "(fun() {)"
  , "(fun(a, b let x = y; })"
  , "fun f(a, b let x = y; }"
  , "fun 😈 f() {}"
  , "fun f😈() {}"
  , "fun f(😈) {}"
  , "fun f() 😈 {}"
  , "fun f() {😈}"
  , "fun ] f() {}"
  , "fun f]() {}"
  , "fun f(]) {}"
  , "fun f() ] {}"
  , "fun f() {]}"
  , "fun f(,) {}"
  , "return"
  , "return x"
  , "return\nx"
  , "return;"
  , "return;x"
  , "return x;"
  , "return 😈 x;"
  , "return\n😈 x;"
  , "return 😈\nx;"
  , "return ) x;"
  , "return\n) x;"
  , "return )\nx;"
  , "return 😈) x;"
  , "return\n😈) x;"
  , "return 😈)\nx;"
  , "return )😈 x;"
  , "return\n)😈 x;"
  , "return )😈\nx;"
  , "break"
  , "break x"
  , "break\nx"
  , "break;"
  , "break;x"
  , "break x;"
  , "loop {}"
  , "loop { let x = y; }"
  , "!x"
  , "+x"
  , "-x"
  , "!x.p"
  , "!x()"
  , "!"
  , "!😈x"
  , "!)x"
  , "!😈)x"
  , "!)😈x"
  , "!!x"
  , "++x"
  , "--x"
  , "+-x"
  , "-+x"
  , "a + b"
  , "a + b + c"
  , "a - b"
  , "a - b - c"
  , "a * b"
  , "a * b * c"
  , "a / b"
  , "a / b / c"
  , "a % b"
  , "a % b % c"
  , "a == b"
  , "a == b == c"
  , "a != b"
  , "a != b != c"
  , "a < b"
  , "a < b < c"
  , "a <= b"
  , "a <= b <= c"
  , "a > b"
  , "a > b > c"
  , "a >= b"
  , "a >= b >= c"
  , "a + b - c"
  , "a - b + c"
  , "a + b * c"
  , "a * b + c"
  , "a + b / c"
  , "a / b + c"
  , "a * b / c"
  , "a / b * c"
  , "a + b * c + d"
  , "a * b + c * d"
  , "a ^ b + c"
  , "a + b ^ c"
  , "a ^ b * c"
  , "a * b ^ c"
  , "a > b + c"
  , "a + b > c"
  , "a < b + c"
  , "a + b < c"
  , "a >= b + c"
  , "a + b >= c"
  , "a <= b + c"
  , "a + b <= c"
  , "a + b == c"
  , "a == b + c"
  , "a + b != c"
  , "a != b + c"
  , "a =="
  , "== b"
  , "a 😈 == b"
  , "a == 😈 b"
  , "a == b 😈"
  , "a ) == b"
  , "a == ) b"
  , "a.p + b.q"
  , "!a + !b"
  , "a() + b()"
  , "a + b +"
  , "a + b + c +"
  , "a 😈 + b + c"
  , "a + 😈 b + c"
  , "a + b 😈 + c"
  , "a + b + 😈 c"
  , "a + b + c 😈"
  , "^ b * c ^ d"
  , "a ^ * c ^ d"
  , "a ^ b * ^ d"
  , "a * ^ c * d"
  , "a * b ^ * d"
  , "a ^ b * c ^"
  , "a 😈 * b + c * d"
  , "a * 😈 b + c * d"
  , "a * b 😈 + c * d"
  , "a * b + 😈 c * d"
  , "a * b + c 😈 * d"
  , "a * b + c * 😈 d"
  , "a * b + c * d 😈"
  , "a - b + c"
  , "a + -b + c"
  , "if x {} else if y {}"
  , "if x {} else if y {} else {}"
  , "if x {} else if y {} else if z {}"
  , "if x {} else if y {} else if z {} else {}"
  , "if x {} else if {}"
  , "if x {} else 😈 if y {}"
  , "if x {} else 😈 if y + z {}"
  , "if x {} else { if y {} }"
  , "a + b * c ^ d"
  , "a 😈 + * b"
  , "{}"
  , "{p: a}"
  , "{p: a, q: b}"
  , "{,}"
  , "{p: a,}"
  , "{p: a, q: b,}"
  , "{p: a q: b}"
  , "{p: a,, q: b}"
  , "{p: a, q: b,,}"
  , "{p: a q: b,}"
  , "{| o}"
  , "{p: a | o}"
  , "{p: a, | o}"
  , "{p: a, q: b | o}"
  , "{p: a | {q: b}}"
  , "{p: a | {q: b | {}}}"
  , "{p: a | {q: b | o}}"
  , "{: a}"
  , "{p a}"
  , "{p: }"
  , "{: a, q: b}"
  , "{p a, q: b}"
  , "{p: , q: b}"
  , "{p}"
  , "{p, q}"
  , "{p: a, q}"
  , "{p, q: b}"
  , "if {} {}"
  , "if {p: a}.p {}"
  , "{p: a}.p"
  , "{😈 p: a}"
  , "{p 😈 : a}"
  , "{p: 😈 a}"
  , "{p: a 😈}"
  , "{😈}"
  , "a && b"
  , "a && b && c"
  , "a || b || c"
  , "a && b || c"
  , "a || b && c"
  , "a && b && c && d"
  , "a || b && c && d"
  , "a && b || c && d"
  , "a && b && c || d"
  , "a && b || c || d"
  , "a || b && c || d"
  , "a || b || c && d"
  , "a || b || c || d"
  , "case V"
  , "case V()"
  , "case V(a)"
  , "case V(a, b)"
  , "case V(a, b, c)"
  , "case V(,)"
  , "case V(a,)"
  , "case V(a, b,)"
  , "case V(a, b, c,)"
  , "case"
  , "case V"
  , "case V("
  , "case V)"
  , "case ()"
  , "case (a)"
  , "case 😈(a)"
  , "let true = x"
  , "let {} = o"
  , "let {a} = o"
  , "let {a, b} = o"
  , "let {a, b, c} = o"
  , "let {,} = o"
  , "let {a,} = o"
  , "let {a, b,} = o"
  , "let {a, b, c,} = o"
  , "let {a: a2} = o"
  , "let {a: a2, b: b2} = o"
  , "let {a: a2, b: b2, c: c2} = o"
  , "let {a: a2, b} = o"
  , "let {a, b: b2} = o"
  , "let {| o} = o"
  , "let {a | o} = o"
  , "let {a, | o} = o"
  , "let {a, b | o} = o"
  , "let {a, b, c | o} = o"
  , "let {a | {b | o}} = o"
  , "let {a | {b | {c | o}}} = o"
  , "let {a | {b | {c | {}}}} = o"
  , "{a: {b: c}}"
  , "{a {}}"
  , "{a true}"
  , "let case V = x"
  , "let case V() = x"
  , "let case V(a) = x"
  , "let case V(a, b) = x"
  , "let case V(a, b, c) = x"
  , "let case V(,) = x"
  , "let case V(a,) = x"
  , "let case V(a, b,) = x"
  , "let case V(a, b, c,) = x"
  , "switch x { y -> {} }"
  , "switch x { y -> {} z -> {} }"
  , "switch x { case Red -> {} case Green -> {} case Blue -> {} }"
  , "switch x {\n\
    \  y -> {}\n\
    \  z -> {}\n\
    \}"
  , "switch x {\n\
    \  case Red -> {}\n\
    \  case Green -> {}\n\
    \  case Blue -> {}\n\
    \}"
  , "switch {} {}"
  , "switch {}"
  , "switch { y -> {} }"
  , "switch x y -> {} }"
  , "switch x { y -> {}"
  , "switch x { -> {} }"
  , "switch x { -> { let a = b } }"
  , "switch x { y {} }"
  , "switch x { y -> } }"
  , "switch x { y -> { }"
  , "switch x { y -> {} let a = b }"
  , "switch x { y -> {} let a = b z -> {} }"
  , "switch x { y -> { let a = b } }"
  , "let x: T = y;"
  , "let x: = y;"
  , "let x T = y;"
  , "(x: T)"
  , "(x:)"
  , "(x T)"
  , "fun f g() {}"
  , "fun f(a: T) {}"
  , "fun f(a: T, b: U) {}"
  , "fun f(a, b: U) {}"
  , "fun f(a: T, b) {}"
  , "fun f(a: T, b: U, c: V) {}"
  , "let f = fun(a: T) {};"
  , "let f = fun(a: T, b: U) {};"
  , "let f = fun(a, b: U) {};"
  , "let f = fun(a: T, b) {};"
  , "let f = fun(a: T, b: U, c: V) {};"
  , "fun f() -> T {}"
  , "fun f() -> {}"
  , "fun f() T {}"
  , "fun f() -> {} {}"
  , "(x: !)"
  , "(x: <> X)"
  , "(x: <T> X)"
  , "(x: <T: A> X)"
  , "(x: <T = A> X)"
  , "(x: <T, U> X)"
  , "(x: <T: A, U: B> X)"
  , "(x: <T = A, U = B> X)"
  , "(x: <T = A, U: B> X)"
  , "(x: <T: A, U = B> X)"
  , "(x: <T, U: B> X)"
  , "(x: <T: A, U> X)"
  , "(x: <T, U = B> X)"
  , "(x: <T = A, U> X)"
  , "(x: <,> X)"
  , "(x: <T,> X)"
  , "(x: <T, U,> X)"
  , "fun f<>() {}"
  , "fun f<T>() {}"
  , "fun f<T: A>() {}"
  , "fun f<T = A>() {}"
  , "fun f<T, U>() {}"
  , "fun f<T: A, U: B>() {}"
  , "fun f<T = A, U = B>() {}"
  , "fun f<T = A, U: B>() {}"
  , "fun f<T: A, U = B>() {}"
  , "fun f<T, U: B>() {}"
  , "fun f<T: A, U>() {}"
  , "fun f<T, U = B>() {}"
  , "fun f<T = A, U>() {}"
  , "fun f<,>() {}"
  , "fun f<T,>() {}"
  , "fun f<T, U,>() {}"
  , "(x: {})"
  , "(x: {a: T})"
  , "(x: {a: T, b: U})"
  , "(x: {,})"
  , "(x: {a: T,})"
  , "(x: {a: T, b: U,})"
  , "(x: {a})"
  , "(x: {a, b})"
  , "(x: {a: T, b})"
  , "(x: {a, b: U})"
  , "(x: {| O})"
  , "(x: {a: T | O})"
  , "(x: {a: T, | O})"
  , "(x: {a: T, b: U | O})"
  , "(x: {a: T, b: U, | O})"
  , "(x: {a: T | {b: U | O}})"
  , "(x: {a: T | {b: U | {}}})"
  , "x\n.Foo"
  , "x;\n.Foo;"
  , "switch x { case V😈 -> {} }"
  , "switch x { | case V -> {} }"
  , "switch x { |😈 -> {} }"
  , "switch x { |😈 case V -> {} }"
  , "switch x { case V | 😈 -> {} }"
  , "switch x { case V | case 😈 -> {} }"
  , "switch x { case V | case W -> {} }"
  , "switch x { case V | case W | case X -> {} }"
  , "switch x { | case V | case W -> {} }"
  , "switch x { | case V | case W | case X -> {} }"
  , "switch x { case V(y) | case W(y) -> {} }"
  , "switch x { case V(y) | case W(y) | case X(y) -> {} }"
  , "switch x { | case V(y) | case W(y) -> {} }"
  , "switch x { | case V(y) | case W(y) | case X(y) -> {} }"
  , "let case V | case W = x;"
  , "let case V | | case W = x;"
  , "(x: case V)"
  , "(x: case V | case W)"
  , "(x: case V | case W | case X)"
  , "(x: | case V)"
  , "(x: | case V | case W)"
  , "(x: | case V | case W | case X)"
  , "(x: case V(A))"
  , "(x: case V(A) | case W(B))"
  , "(x: case V(A) | case W(B) | case X(C))"
  , "(x: | case V(A))"
  , "(x: | case V(A) | case W(B))"
  , "(x: | case V(A) | case W(B) | case X(C))"
  , "(x: case V | | case W)"
  , "(x: 😈 case V | case W)"
  , "(x: | 😈 case V | case W)"
  , "(x: | case V 😈 | case W)"
  , "(x: | case V | 😈 case W)"
  , "(x: fun)"
  , "(x: fun😈)"
  , "(x: fun())"
  , "(x: fun() ->)"
  , "(x: fun() -> A)"
  , "(x: fun(A) -> B)"
  , "(x: fun(A, B) -> C)"
  , "(x: fun(A, B, C) -> D)"
  , "(x: fun(,) -> A)"
  , "(x: fun(A,) -> B)"
  , "(x: fun(A, B,) -> C)"
  , "(x: fun(A, B, C,) -> D)"
  , "(x: fun<>() -> A)"
  , "(x: fun<A>() -> B)"
  , "(x: fun<A, B>() -> C)"
  , "(x: fun<A, B, C>() -> D)"
  , "(x: fun<,>() -> A)"
  , "(x: fun<A,>() -> B)"
  , "(x: fun<A, B,>() -> C)"
  , "(x: fun<A, B, C,>() -> D)"
  , "(x: fun<A, B>(C, D) -> E)"
  , "(x: <A, B> fun<C, D>(E, F) -> G)"
  , "let (x) = y;"
  , "(x: (T));"
  , "(x: <A> (<B> T));"
  , "(x: <A> <B> T);"
  , "/*"
  , "/*/"
  , "/**/"
  , "/* *"
  , "/* **"
  , "/* * /"
  , "/* */"
  , "/* **/"
  , "/* x"
  , "/*/ x"
  , "/**/ x"
  , "/* * x"
  , "/* ** x"
  , "/* * / x"
  , "/* */ x"
  , "/* **/ x"
  ]

openSnapshotFile :: IO Handle
openSnapshotFile = do
  h <- openFile "test/Brite/Syntax/ParserSpecSnapshot.md" WriteMode
  hPutStrLn h "# ParserSpecSnapshot"
  return h

closeSnapshotFile :: Handle -> IO ()
closeSnapshotFile h = do
  hPutStrLn h ""
  hPutStrLn h (replicate 80 '-')
  hClose h

spec :: Spec
spec = beforeAll openSnapshotFile $ afterAll closeSnapshotFile $ do
  flip mapM_ testData $ \source ->
    it (T.unpack (escape source)) $ \h ->
      let
        (module_, diagnostics) = runDiagnosticWriter (parseModule (tokenize source))
        rebuiltSource = L.toStrict (B.toLazyText (moduleSource module_))
      in do
        hPutStrLn h ""
        hPutStrLn h (replicate 80 '-')
        hPutStrLn h ""
        hPutStrLn h "### Source"
        hPutStrLn h "```ite"
        hPutStrLn h (T.unpack source)
        hPutStrLn h "```"
        hPutStrLn h ""
        hPutStrLn h "### AST"
        hPutStrLn h "```"
        hPutStr h (L.unpack (B.toLazyText (debugModule module_)))
        hPutStrLn h "```"
        if null diagnostics then return () else (do
          hPutStrLn h ""
          hPutStrLn h "### Errors"
          flip mapM_ diagnostics (\diagnostic ->
            hPutStrLn h (L.unpack (B.toLazyText (B.fromText "- " <> debugDiagnostic diagnostic)))))
        rebuiltSource `shouldBe` source

escape :: T.Text -> T.Text
escape = T.concatMap
  (\c ->
    case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      _ -> T.singleton c)