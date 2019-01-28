{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.UnifySpec (spec) where

import Brite.Diagnostic
import qualified Brite.Semantics.AST as AST
import Brite.Semantics.Check (checkPolytype)
import Brite.Semantics.CheckMonad
import qualified Brite.Semantics.Prefix as Prefix
import qualified Brite.Semantics.Type as Type
import Brite.Semantics.TypePrinter
import Brite.Semantics.Unify
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Parser
import Brite.Syntax.ParserFramework
import Brite.Syntax.Printer
import Brite.Syntax.Tokens
import Data.Foldable (traverse_, toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Test.Hspec

testData :: [(Text, Text, [Text])]
testData =
  [ ("unify(<>, Bool, Bool)", "<>", [])
  , ("unify(<>, Int, Int)", "<>", [])
  , ("unify(<A>, Int, Int)", "<A>", [])
  , ("unify(<A, B>, Int, Int)", "<A, B>", [])
  , ("unify(<A, B, C>, Int, Int)", "<A, B, C>", [])
  , ("unify(<>, fun(Int) -> Int, fun(Int) -> Int)", "<>", [])
  , ("unify(<>, fun(Int) -> Bool, fun(Int) -> Bool)", "<>", [])
  , ("unify(<>, fun(Int) -> fun(Bool) -> Bool, fun(Int) -> fun(Bool) -> Bool)", "<>", [])
  , ("unify(<>, fun(Int) -> Int, fun(Int) -> Bool)", "<>", ["Int ≢ Bool"])
  , ("unify(<>, fun(Int) -> Int, fun(Bool) -> Int)", "<>", ["Int ≢ Bool"])
  , ("unify(<>, fun(Int) -> Int, fun(Bool) -> Bool)", "<>", ["Int ≢ Bool", "Int ≢ Bool"])
  , ("unify(<>, fun(Int) -> fun(Int) -> Int, fun(Bool) -> fun(Bool) -> Bool)", "<>", ["Int ≢ Bool", "Int ≢ Bool", "Int ≢ Bool"])
  , ("unify(<>, fun(fun(Int) -> Int) -> Int, fun(fun(Bool) -> Bool) -> Bool)", "<>", ["Int ≢ Bool", "Int ≢ Bool", "Int ≢ Bool"])
  , ("unify(<>, Int, fun(Int) -> Int)", "<>", ["Int ≢ fun(Int) -> Int"])
  , ("unify(<>, fun(Int) -> Int, Int)", "<>", ["fun(Int) -> Int ≢ Int"])
  , ("unify(<A = Int>, A, Int)", "<A = Int>", [])
  , ("unify(<A = Int>, Int, A)", "<A = Int>", [])
  , ("unify(<A = Int>, A, Bool)", "<A = Int>", ["Int ≢ Bool"])
  , ("unify(<A = Int>, Bool, A)", "<A = Int>", ["Bool ≢ Int"])
  , ("unify(<A: Int>, A, Int)", "<A: Int>", [])
  , ("unify(<A: Int>, Int, A)", "<A: Int>", [])
  , ("unify(<A: Int>, A, Bool)", "<A: Int>", ["Int ≢ Bool"])
  , ("unify(<A: Int>, Bool, A)", "<A: Int>", ["Bool ≢ Int"])
  , ("unify(<B = Int, A = B>, A, Int)", "<B = Int, A = B>", [])
  , ("unify(<B = Int, A = B>, Int, A)", "<B = Int, A = B>", [])
  , ("unify(<B = Int, A = B>, A, Bool)", "<B = Int, A = B>", ["Int ≢ Bool"])
  , ("unify(<B = Int, A = B>, Bool, A)", "<B = Int, A = B>", ["Bool ≢ Int"])
  , ("unify(<B = Int, A: B>, A, Int)", "<B = Int, A: B>", [])
  , ("unify(<B = Int, A: B>, Int, A)", "<B = Int, A: B>", [])
  , ("unify(<B = Int, A: B>, A, Bool)", "<B = Int, A: B>", ["Int ≢ Bool"])
  , ("unify(<B = Int, A: B>, Bool, A)", "<B = Int, A: B>", ["Bool ≢ Int"])
  , ("unify(<B: Int, A = B>, A, Int)", "<B: Int, A = B>", [])
  , ("unify(<B: Int, A = B>, Int, A)", "<B: Int, A = B>", [])
  , ("unify(<B: Int, A = B>, A, Bool)", "<B: Int, A = B>", ["Int ≢ Bool"])
  , ("unify(<B: Int, A = B>, Bool, A)", "<B: Int, A = B>", ["Bool ≢ Int"])
  , ("unify(<B: Int, A: B>, A, Int)", "<B: Int, A: B>", [])
  , ("unify(<B: Int, A: B>, Int, A)", "<B: Int, A: B>", [])
  , ("unify(<B: Int, A: B>, A, Bool)", "<B: Int, A: B>", ["Int ≢ Bool"])
  , ("unify(<B: Int, A: B>, Bool, A)", "<B: Int, A: B>", ["Bool ≢ Int"])
  , ("unify(<A>, A, A)", "<A>", [])
  , ("unify(<A: !>, A, A)", "<A>", [])
  , ("unify(<A = !>, A, A)", "<A = !>", [])
  , ("unify(<A, B = A, C = B>, B, C)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = A, C = B>, C, B)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = A, C = B>, A, B)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = A, C = B>, A, C)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = A, C = B>, B, A)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = A, C = B>, C, A)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = A, C = B>, B, A)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = A, C = B>, B, B)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = A, C = B>, C, C)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, B, C)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, C, B)", "<A, B = <Z> A, C = B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, A, B)", "<A, B = A, C = <Z> B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, A, C)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, B, A)", "<A, B = A, C = <Z> B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, C, A)", "<A, B = A, C = B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, B, A)", "<A, B = A, C = <Z> B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, B, B)", "<A, B = <Z> A, C = <Z> B>", [])
  , ("unify(<A, B = <Z> A, C = <Z> B>, C, C)", "<A, B = <Z> A, C = <Z> B>", [])
  , ("unify(<A>, A, Int)", "<A = Int>", [])
  , ("unify(<A>, Int, A)", "<A = Int>", [])
  , ("unify(<A: !>, A, Int)", "<A = Int>", [])
  , ("unify(<A: !>, Int, A)", "<A = Int>", [])
  , ("unify(<A = !>, A, Int)", "<A = !>", ["! ≢ Int"])
  , ("unify(<A = !>, Int, A)", "<A = !>", ["! ≢ Int"])
  , ("unify(<B: !, A: B>, A, Int)", "<B = Int, A: B>", [])
  , ("unify(<B: !, A: B>, Int, A)", "<B = Int, A: B>", [])
  , ("unify(<B: !, A = B>, A, Int)", "<B = Int, A = B>", [])
  , ("unify(<B: !, A = B>, Int, A)", "<B = Int, A = B>", [])
  , ("unify(<B = !, A: B>, A, Int)", "<B = !, A: B>", ["! ≢ Int"])
  , ("unify(<B = !, A: B>, Int, A)", "<B = !, A: B>", ["! ≢ Int"])
  , ("unify(<B = !, A = B>, A, Int)", "<B = !, A = B>", ["! ≢ Int"])
  , ("unify(<B = !, A = B>, Int, A)", "<B = !, A = B>", ["! ≢ Int"])
  , ("unify(<B, A = fun(B) -> B>, A, B)", "<B, A = fun(B) -> B>", ["Infinite type since `B` occurs in `fun(B) -> B`."])
  , ("unify(<B, A = fun(B) -> B>, B, A)", "<B, A = fun(B) -> B>", ["Infinite type since `B` occurs in `fun(B) -> B`."])
  , ("unify(<B, C = fun(B) -> B, A = C>, A, B)", "<B, C = fun(B) -> B, A = C>", ["Infinite type since `B` occurs in `fun(B) -> B`."])
  , ("unify(<B, C = fun(B) -> B, A = C>, B, A)", "<B, C = fun(B) -> B, A = C>", ["Infinite type since `B` occurs in `fun(B) -> B`."])
  , ("unify(<B, C = fun(B) -> B, A = fun(C) -> C>, A, B)", "<B, C = fun(B) -> B, A = fun(C) -> C>", ["Infinite type since `B` occurs in `fun(C) -> C`."])
  , ("unify(<B, C = fun(B) -> B, A = fun(C) -> C>, B, A)", "<B, C = fun(B) -> B, A = fun(C) -> C>", ["Infinite type since `B` occurs in `fun(C) -> C`."])
  , ("unify(<A, B>, A, B)", "<A, B = A>", [])
  , ("unify(<A, B>, B, A)", "<B, A = B>", [])
  , ("unify(<A, B>, A, fun(B) -> B)", "<B, A = fun(B) -> B>", [])
  , ("unify(<A, B>, fun(B) -> B, A)", "<B, A = fun(B) -> B>", [])
  , ("unify(<A: fun<X>(X) -> X>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A: fun<X>(X) -> X>, A, fun(Int) -> Bool)", "<A: fun<X>(X) -> X>", ["Int ≢ Bool"])
  , ("unify(<A = fun<X>(X) -> X>, A, fun(Int) -> Int)", "<A = fun<X>(X) -> X>", ["fun<X>(X) -> X ≢ fun(Int) -> Int"])
  , ("unify(<A = fun<X>(X) -> X>, A, fun(Int) -> Bool)", "<A = fun<X>(X) -> X>", ["Int ≢ Bool"])
  , ("unify(<A: fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, A, B)", "<A: fun<X>(X) -> X, B = A>", [])
  , ("unify(<A: fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, B, A)", "<B: fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, A, B)", "<A: fun<X>(X) -> X, B = A>", [])
  , ("unify(<A: fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, B, A)", "<B: fun<Z>(Z) -> Z, A = B>", [])
  , ("unify(<A = fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, B, A)", "<B = fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A: fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, B, A)", "<B = fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, B, A)", "<B = fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, A, B)", "<A: fun<X>(X) -> X, B = A>", [])
  , ("unify(<A: fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, B, A)", "<B: fun<Z>(Z) -> Z, A = B>", [])
  , ("unify(<A = fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B: <Y, Z> fun(Y) -> Z>, B, A)", "<B = fun<Z>(Z) -> Z, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>, A, B)", "<A: fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>", ["<Y, Z> fun(Y) -> Z ≢ fun<X>(X) -> X"])
  , ("unify(<A: fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>, B, A)", "<A: fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>", ["<Y, Z> fun(Y) -> Z ≢ fun<Z>(Z) -> Z"])
  , ("unify(<A = fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>, A, B)", "<A = fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>", ["<Y, Z> fun(Y) -> Z ≢ fun<X>(X) -> X"])
  , ("unify(<A = fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>, B, A)", "<A = fun<X>(X) -> X, B = <Y, Z> fun(Y) -> Z>", ["<Y, Z> fun(Y) -> Z ≢ fun<Z>(Z) -> Z"])
  , ("unify(<A: fun<X>(X) -> X, B = fun(Int) -> Int>, A, B)", "<A = fun(Int) -> Int, B = fun(Int) -> Int>", [])
  , ("unify(<A: fun<X>(X) -> X, B = fun(Int) -> Int>, B, A)", "<A = fun(Int) -> Int, B = fun(Int) -> Int>", [])
  , ("unify(<A, B = fun(A) -> A>, A, B)", "<A, B = fun(A) -> A>", ["Infinite type since `A` occurs in `fun(A) -> A`."])
  , ("unify(<A, B = fun(A) -> A>, B, A)", "<A, B = fun(A) -> A>", ["Infinite type since `A` occurs in `fun(A) -> A`."])
  , ("unify(<A, B = fun<C>(C) -> A>, A, B)", "<A, B = fun<C>(C) -> A>", ["Infinite type since `A` occurs in `fun<C>(C) -> A`."])
  , ("unify(<A, B = fun<C>(C) -> A>, B, A)", "<A, B = fun<C>(C) -> A>", ["Infinite type since `A` occurs in `fun<C>(C) -> A`."])
  , ("unify(<A = !, B = !>, A, B)", "<A = !, B = A>", [])
  , ("unify(<A = !, B = !>, B, A)", "<B = !, A = B>", [])
  , ("unify(<A = !, B = fun<C>(C) -> C>, A, B)", "<A = !, B = fun<C>(C) -> C>", ["! ≢ fun<C>(C) -> C"])
  , ("unify(<A = !, B = fun<C>(C) -> C>, B, A)", "<A = !, B = fun<C>(C) -> C>", ["! ≢ fun<C>(C) -> C"])
  , ("unify(<A, B>, A, B)", "<A, B = A>", [])
  , ("unify(<A, B>, B, A)", "<B, A = B>", [])
  , ("unify(<A: !, B: !>, A, B)", "<A, B = A>", [])
  , ("unify(<A: !, B: !>, B, A)", "<B, A = B>", [])
  , ("unify(<A = !, B: !>, A, B)", "<A = !, B = A>", [])
  , ("unify(<A = !, B: !>, B, A)", "<B = !, A = B>", [])
  , ("unify(<A: !, B = !>, A, B)", "<A = !, B = A>", [])
  , ("unify(<A: !, B = !>, B, A)", "<B = !, A = B>", [])
  , ("unify(<A = !, B = !>, A, B)", "<A = !, B = A>", [])
  , ("unify(<A = !, B = !>, B, A)", "<B = !, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, A, B)", "<A: fun<X>(X) -> X, B = A>", [])
  , ("unify(<A: fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, B, A)", "<B: fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<A = fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B: fun<Y>(Y) -> Y>, B, A)", "<B = fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A: fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, B, A)", "<B = fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, B, A)", "<B = fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X, B: fun<X>(X) -> X>, A, B)", "<A: fun<X>(X) -> X, B = A>", [])
  , ("unify(<A: fun<X>(X) -> X, B: fun<X>(X) -> X>, B, A)", "<B: fun<X>(X) -> X, A = B>", [])
  , ("unify(<A = fun<X>(X) -> X, B: fun<X>(X) -> X>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B: fun<X>(X) -> X>, B, A)", "<B = fun<X>(X) -> X, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X, B = fun<X>(X) -> X>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A: fun<X>(X) -> X, B = fun<X>(X) -> X>, B, A)", "<B = fun<X>(X) -> X, A = B>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<X>(X) -> X>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<X>(X) -> X>, B, A)", "<B = fun<X>(X) -> X, A = B>", [])
  , ("unify(<X>, X, X)", "<X>", [])
  , ("unify(<A: Int>, A, Int)", "<A: Int>", [])
  , ("unify(<A = Int>, A, Int)", "<A = Int>", [])
  , ("unify(<A: Int>, A, Bool)", "<A: Int>", ["Int ≢ Bool"])
  , ("unify(<A = Int>, A, Bool)", "<A = Int>", ["Int ≢ Bool"])
  , ("unify(<A: Int>, Int, A)", "<A: Int>", [])
  , ("unify(<A = Int>, Int, A)", "<A = Int>", [])
  , ("unify(<A: Int>, Bool, A)", "<A: Int>", ["Bool ≢ Int"])
  , ("unify(<A = Int>, Bool, A)", "<A = Int>", ["Bool ≢ Int"])
  , ("unify(<A, B = A>, A, B)", "<A, B = A>", [])
  , ("unify(<A, B = A>, B, A)", "<A, B = A>", [])
  , ("unify(<A, B: A>, A, B)", "<A, B: A>", [])
  , ("unify(<A, B: A>, B, A)", "<A, B: A>", [])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, A, B)", "<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>", ["Int ≢ Bool"])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, B, A)", "<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>", ["Bool ≢ Int"])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, A, B)", "<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>", ["Int ≢ Bool"])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, B, A)", "<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>", ["Bool ≢ Int"])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, A, B)", "<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>", ["Int ≢ Bool"])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, B, A)", "<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>", ["Bool ≢ Int"])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, A, B)", "<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>", ["Int ≢ Bool"])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, B, A)", "<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>", ["Bool ≢ Int"])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, A, B)", "<A: fun<X>(X) -> Int, B = A>", [])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, B, A)", "<B: fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, A, B)", "<A = fun<X>(X) -> Int, B = A>", [])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, B, A)", "<B = fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, A, B)", "<A = fun<X>(X) -> Int, B = A>", [])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, B, A)", "<B = fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, A, B)", "<A = fun<X>(X) -> Int, B = A>", [])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, B, A)", "<B = fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<C, A: fun<X>(X) -> C, B: fun<Y>(Y) -> A>, A, B)", "<C, A: fun<X>(X) -> C, B: fun<Y>(Y) -> A>", ["Infinite type since `C` occurs in `fun<X>(X) -> C`."])
  , ("unify(<C, A: fun<X>(X) -> C, B: fun<Y>(Y) -> A>, B, A)", "<C, A: fun<X>(X) -> C, B: fun<Y>(Y) -> A>", ["Infinite type since `C` occurs in `fun<X>(X) -> C`."])
  , ("unify(<C, A = fun<X>(X) -> C, B: fun<Y>(Y) -> A>, A, B)", "<C, A = fun<X>(X) -> C, B: fun<Y>(Y) -> A>", ["Infinite type since `C` occurs in `fun<X>(X) -> C`."])
  , ("unify(<C, A = fun<X>(X) -> C, B: fun<Y>(Y) -> A>, B, A)", "<C, A = fun<X>(X) -> C, B: fun<Y>(Y) -> A>", ["Infinite type since `C` occurs in `fun<X>(X) -> C`."])
  , ("unify(<C, A: fun<X>(X) -> C, B = fun<Y>(Y) -> A>, A, B)", "<C, A: fun<X>(X) -> C, B = fun<Y>(Y) -> A>", ["Infinite type since `C` occurs in `fun<X>(X) -> C`."])
  , ("unify(<C, A: fun<X>(X) -> C, B = fun<Y>(Y) -> A>, B, A)", "<C, A: fun<X>(X) -> C, B = fun<Y>(Y) -> A>", ["Infinite type since `C` occurs in `fun<X>(X) -> C`."])
  , ("unify(<C, A = fun<X>(X) -> C, B = fun<Y>(Y) -> A>, A, B)", "<C, A = fun<X>(X) -> C, B = fun<Y>(Y) -> A>", ["Infinite type since `C` occurs in `fun<X>(X) -> C`."])
  , ("unify(<C, A = fun<X>(X) -> C, B = fun<Y>(Y) -> A>, B, A)", "<C, A = fun<X>(X) -> C, B = fun<Y>(Y) -> A>", ["Infinite type since `C` occurs in `fun<X>(X) -> C`."])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>, A, B)", "<A: fun(Int) -> Int, B = A>", [])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>, B, A)", "<B: fun(Int) -> Int, A = B>", [])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>, A, B)", "<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>", ["fun<X>(X) -> Int ≢ fun(Int) -> Int"])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>, B, A)", "<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Y>", ["fun<X>(X) -> Int ≢ fun(Int) -> Int"])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>, A, B)", "<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>", ["fun<Y>(Y) -> Y ≢ fun(Int) -> Int"])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>, B, A)", "<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>", ["fun<Y>(Y) -> Y ≢ fun(Int) -> Int"])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>, A, B)", "<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>", ["fun<X>(X) -> Int ≢ fun(Int) -> Int", "fun<Y>(Y) -> Y ≢ fun(Int) -> Int"])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>, B, A)", "<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Y>", ["fun<Y>(Y) -> Y ≢ fun(Int) -> Int", "fun<X>(X) -> Int ≢ fun(Int) -> Int"])
  , ("unify(<A: fun<X>(X) -> Int, B>, A, fun(B) -> Bool)", "<A: fun<X>(X) -> Int, B>", ["Int ≢ Bool"])
  , ("unify(<A: fun<X>(X) -> Int, B>, A, fun(B) -> Int)", "<B, A = fun(B) -> Int>", [])
  , ("unify(<A = fun<X>(X) -> Int, B>, A, fun(B) -> Bool)", "<A = fun<X>(X) -> Int, B>", ["Int ≢ Bool"])
  , ("unify(<A = fun<X>(X) -> Int, B>, A, fun(B) -> Int)", "<A = fun<X>(X) -> Int, B>", ["fun<X>(X) -> Int ≢ fun(B) -> Int"])
  , ("unify(<A: fun<X>(X) -> Int>, A, fun(Bool) -> Bool)", "<A: fun<X>(X) -> Int>", ["Int ≢ Bool"])
  , ("unify(<A: fun<X>(X) -> Int>, A, fun(Bool) -> Int)", "<A = fun(Bool) -> Int>", [])
  , ("unify(<A = fun<X>(X) -> Int>, A, fun(Bool) -> Bool)", "<A = fun<X>(X) -> Int>", ["Int ≢ Bool"])
  , ("unify(<A = fun<X>(X) -> Int>, A, fun(Bool) -> Int)", "<A = fun<X>(X) -> Int>", ["fun<X>(X) -> Int ≢ fun(Bool) -> Int"])
  , ("unify(<A: fun<X>(X) -> Int>, A, fun(A) -> Bool)", "<A: fun<X>(X) -> Int>", ["Int ≢ Bool"])
  , ("unify(<A: fun<X>(X) -> Int>, A, fun(A) -> Int)", "<A: fun<X>(X) -> Int>", ["Infinite type since `A` occurs in `fun(A) -> Int`."])
  , ("unify(<A = fun<X>(X) -> Int>, A, fun(A) -> Bool)", "<A = fun<X>(X) -> Int>", ["Int ≢ Bool"])
  , ("unify(<A = fun<X>(X) -> Int>, A, fun(A) -> Int)", "<A = fun<X>(X) -> Int>", ["Infinite type since `A` occurs in `fun(A) -> Int`."])
  , ("unify(<A: fun<X>(X) -> Int, B>, fun(B) -> Bool, A)", "<A: fun<X>(X) -> Int, B>", ["Bool ≢ Int"])
  , ("unify(<A: fun<X>(X) -> Int, B>, fun(B) -> Int, A)", "<B, A = fun(B) -> Int>", [])
  , ("unify(<A = fun<X>(X) -> Int, B>, fun(B) -> Bool, A)", "<A = fun<X>(X) -> Int, B>", ["Bool ≢ Int"])
  , ("unify(<A = fun<X>(X) -> Int, B>, fun(B) -> Int, A)", "<A = fun<X>(X) -> Int, B>", ["fun<X>(X) -> Int ≢ fun(B) -> Int"])
  , ("unify(<A: fun<X>(X) -> Int>, fun(Bool) -> Bool, A)", "<A: fun<X>(X) -> Int>", ["Bool ≢ Int"])
  , ("unify(<A: fun<X>(X) -> Int>, fun(Bool) -> Int, A)", "<A = fun(Bool) -> Int>", [])
  , ("unify(<A = fun<X>(X) -> Int>, fun(Bool) -> Bool, A)", "<A = fun<X>(X) -> Int>", ["Bool ≢ Int"])
  , ("unify(<A = fun<X>(X) -> Int>, fun(Bool) -> Int, A)", "<A = fun<X>(X) -> Int>", ["fun<X>(X) -> Int ≢ fun(Bool) -> Int"])
  , ("unify(<A: fun<X>(X) -> Int>, fun(A) -> Bool, A)", "<A: fun<X>(X) -> Int>", ["Bool ≢ Int"])
  , ("unify(<A: fun<X>(X) -> Int>, fun(A) -> Int, A)", "<A: fun<X>(X) -> Int>", ["Infinite type since `A` occurs in `fun(A) -> Int`."])
  , ("unify(<A = fun<X>(X) -> Int>, fun(A) -> Bool, A)", "<A = fun<X>(X) -> Int>", ["Bool ≢ Int"])
  , ("unify(<A = fun<X>(X) -> Int>, fun(A) -> Int, A)", "<A = fun<X>(X) -> Int>", ["Infinite type since `A` occurs in `fun(A) -> Int`."])
  , ("unify(<A, B = fun<X>(X) -> X>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A, B = fun<X>(X) -> X>, B, A)", "<B = fun<X>(X) -> X, A = B>", [])
  , ("unify(<A, B: fun<X>(X) -> X>, A, B)", "<A: fun<X>(X) -> X, B = A>", [])
  , ("unify(<A, B: fun<X>(X) -> X>, B, A)", "<B: fun<X>(X) -> X, A = B>", [])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, A, B)", "<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>", ["Int ≢ Bool"])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, B, A)", "<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>", ["Bool ≢ Int"])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, A, B)", "<A: fun<X>(X) -> Int, B = A>", [])
  , ("unify(<A: fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, B, A)", "<B: fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, A, B)", "<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>", ["Int ≢ Bool"])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>, B, A)", "<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Bool>", ["Bool ≢ Int"])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, A, B)", "<A = fun<X>(X) -> Int, B = A>", [])
  , ("unify(<A = fun<X>(X) -> Int, B: fun<Y>(Y) -> Int>, B, A)", "<B = fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, A, B)", "<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>", ["Int ≢ Bool"])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, B, A)", "<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>", ["Bool ≢ Int"])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, A, B)", "<A = fun<X>(X) -> Int, B = A>", [])
  , ("unify(<A: fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, B, A)", "<B = fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, A, B)", "<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>", ["Int ≢ Bool"])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>, B, A)", "<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Bool>", ["Bool ≢ Int"])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, A, B)", "<A = fun<X>(X) -> Int, B = A>", [])
  , ("unify(<A = fun<X>(X) -> Int, B = fun<Y>(Y) -> Int>, B, A)", "<B = fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<X>(X) -> X>, A, B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<X>(X) -> X>, B, A)", "<B = fun<X>(X) -> X, A = B>", [])
  , ("unify(<A, B = A, C = B, D = C, E = D, F = fun(E) -> E>, A, F)", "<A, B = A, C = B, D = C, E = D, F = fun(E) -> E>", ["Infinite type since `A` occurs in `fun(E) -> E`."])
  , ("unify(<A, B = A, C = B, D = C, E = D, F = fun(E) -> E>, F, A)", "<A, B = A, C = B, D = C, E = D, F = fun(E) -> E>", ["Infinite type since `A` occurs in `fun(E) -> E`."])
  , ("unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, fun(A) -> A, fun(A) -> B)", "<A = fun<X>(X) -> X, B = A>", [])
  , ("unify(<A = fun<X>(X) -> X, B = fun<Y>(Y) -> Y>, fun(A) -> B, fun(A) -> A)", "<B = fun<Y>(Y) -> Y, A = B>", [])
  , ("unify(<T = <A = fun<X>(X) -> X> fun(A) -> A, U = <B = fun<Y>(Y) -> Y, C = fun<Z>(Z) -> Z> fun(B) -> C>, T, U)", "<T = <A = fun<X>(X) -> X> fun(A) -> A, U = T>", [])
  , ("unify(<T = <A = fun<X>(X) -> X> fun(A) -> A, U = <B = fun<Y>(Y) -> Y, C = fun<Z>(Z) -> Z> fun(B) -> C>, U, T)", "<U = <C = fun<Z>(Z) -> Z>fun(C) -> C, T = U>", [])
  , ("unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B>, A, B)", "<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B = A>", [])
  , ("unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B>, B, A)", "<B = <X, X = fun<Z>(Z) -> X> fun(X) -> X, A = B>", [])
  , ("unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B, C, D, E>, A, fun(fun(B) -> C) -> fun(D) -> E)", "<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, C, E = C, D, B>", ["fun<Z>(Z) -> X ≢ fun(B) -> C", "fun<Z>(Z) -> X ≢ fun(D) -> E"])
  , ("unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B, C, D, E>, fun(fun(B) -> C) -> fun(D) -> E, A)", "<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, E, D, B, C = E>", ["fun<Z>(Z) -> X ≢ fun(B) -> C", "fun<Z>(Z) -> X ≢ fun(D) -> E"])
  , ("unify(<A: <X, X: fun<Z>(Z) -> X> fun(X) -> X, B, C, D, E>, A, fun(fun(B) -> C) -> fun(D) -> E)", "<B, C, D = B, E = C, A = fun(fun(B) -> C) -> fun(D) -> E>", [])
  , ("unify(<A: <X, X: fun<Z>(Z) -> X> fun(X) -> X, B, C, D, E>, fun(fun(B) -> C) -> fun(D) -> E, A)", "<D, B = D, E, C = E, A = fun(fun(B) -> C) -> fun(D) -> E>", [])
  , ("unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B = <X, X = fun<Z>(Z) -> X> fun(X) -> X>, A, B)", "<A = <X, X2 = fun<Z>(Z) -> X> fun(X2) -> X2, B = A>", [])
  , ("unify(<A = <X, X2, X = fun<Z>(Z) -> fun(X2) -> X> fun(X) -> X, B = <X, X2, X = fun<Z>(Z) -> fun(X2) -> X> fun(X) -> X>, A, B)", "<A = <X, X2, X3 = fun<Z>(Z) -> fun(X2) -> X> fun(X3) -> X3, B = A>", [])
  , ("unify(<A = <X1, X1 = fun<Z>(Z) -> X1> fun(X1) -> X1, B = <X1, X1 = fun<Z>(Z) -> X1> fun(X1) -> X1>, A, B)", "<A = <X1, X2 = fun<Z>(Z) -> X1> fun(X2) -> X2, B = A>", [])
  , ("unify(<A = <X1, X2, X1 = fun<Z>(Z) -> fun(X2) -> X1> fun(X1) -> X1, B = <X1, X2, X1 = fun<Z>(Z) -> fun(X2) -> X1> fun(X1) -> X1>, A, B)", "<A = <X1, X2, X3 = fun<Z>(Z) -> fun(X2) -> X1> fun(X3) -> X3, B = A>", [])
  , ("unify(<A = <X5, X5 = fun<Z>(Z) -> X5> fun(X5) -> X5, B = <X5, X5 = fun<Z>(Z) -> X5> fun(X5) -> X5>, A, B)", "<A = <X5, X6 = fun<Z>(Z) -> X5> fun(X6) -> X6, B = A>", [])
  , ("unify(<A = <X, X = fun<Z>(Z) -> X> fun(X) -> X, B = <X, X = fun<Z>(Z) -> X> fun(X) -> X>, B, A)", "<B = <X, X2 = fun<Z>(Z) -> X> fun(X2) -> X2, A = B>", [])
  , ("unify(<A = <X, X2, X = fun<Z>(Z) -> fun(X2) -> X> fun(X) -> X, B = <X, X2, X = fun<Z>(Z) -> fun(X2) -> X> fun(X) -> X>, B, A)", "<B = <X, X2, X3 = fun<Z>(Z) -> fun(X2) -> X> fun(X3) -> X3, A = B>", [])
  , ("unify(<A = <X1, X1 = fun<Z>(Z) -> X1> fun(X1) -> X1, B = <X1, X1 = fun<Z>(Z) -> X1> fun(X1) -> X1>, B, A)", "<B = <X1, X2 = fun<Z>(Z) -> X1> fun(X2) -> X2, A = B>", [])
  , ("unify(<A = <X1, X2, X1 = fun<Z>(Z) -> fun(X2) -> X1> fun(X1) -> X1, B = <X1, X2, X1 = fun<Z>(Z) -> fun(X2) -> X1> fun(X1) -> X1>, B, A)", "<B = <X1, X2, X3 = fun<Z>(Z) -> fun(X2) -> X1> fun(X3) -> X3, A = B>", [])
  , ("unify(<A = <X5, X5 = fun<Z>(Z) -> X5> fun(X5) -> X5, B = <X5, X5 = fun<Z>(Z) -> X5> fun(X5) -> X5>, B, A)", "<B = <X5, X6 = fun<Z>(Z) -> X5> fun(X6) -> X6, A = B>", [])
  , ("unify(<A: fun<X>(X) -> X>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A: fun<X>(X) -> X, B>, A, fun(Int) -> B)", "<B = Int, A = fun(Int) -> B>", [])
  , ("unify(<A = <X = Int> X>, A, Int)", "<A = Int>", [])
  , ("unify(<A: <X: fun<Y>(Y) -> Y> X>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A = <X: fun<Y>(Y) -> Y> X>, A, fun(Int) -> Int)", "<A = fun<Y>(Y) -> Y>", ["fun<Y>(Y) -> Y ≢ fun(Int) -> Int"])
  , ("unify(<A: <X = fun<Y>(Y) -> Y> X>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A = <X = fun<Y>(Y) -> Y> X>, A, fun(Int) -> Int)", "<A = fun<Y>(Y) -> Y>", ["fun<Y>(Y) -> Y ≢ fun(Int) -> Int"])
  , ("unify(<A: fun<Y>(Y) -> Y>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A: <B = fun<X>(X) -> X> B>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A: <B = <C = fun<X>(X) -> X> C> B>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A: <B = <C = <D = fun<X>(X) -> X> D> C> B>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A: <B = fun<X>(X) -> X> fun(B) -> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)", "<A: <B = fun<X>(X) -> X> fun(B) -> B>", ["fun<X>(X) -> X ≢ fun(Int) -> Int", "fun<X>(X) -> X ≢ fun(Int) -> Int"])
  , ("unify(<A: <B = <C = fun<X>(X) -> X> C> fun(B) -> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)", "<A: <B = fun<X>(X) -> X> fun(B) -> B>", ["fun<X>(X) -> X ≢ fun(Int) -> Int", "fun<X>(X) -> X ≢ fun(Int) -> Int"])
  , ("unify(<A: <B = <C = <D = fun<X>(X) -> X> D> C> fun(B) -> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)", "<A: <B = fun<X>(X) -> X> fun(B) -> B>", ["fun<X>(X) -> X ≢ fun(Int) -> Int", "fun<X>(X) -> X ≢ fun(Int) -> Int"])
  , ("unify(<A: <B = fun<X>(X) -> X> fun(B) -> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)", "<A: <B = fun<X>(X) -> X> fun(B) -> B>", ["fun<X>(X) -> X ≢ fun(Int) -> Int", "fun<X>(X) -> X ≢ fun(Int) -> Int"])
  , ("unify(<A: <B = <C = fun<X>(X) -> X> fun(C) -> C> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)", "<A: <C = fun<X>(X) -> X> fun(C) -> C>", ["fun<X>(X) -> X ≢ fun(Int) -> Int", "fun<X>(X) -> X ≢ fun(Int) -> Int"])
  , ("unify(<A: <B = <C = <D = fun<X>(X) -> X> fun(D) -> D> C> B>, A, fun(fun(Int) -> Int) -> fun(Int) -> Int)", "<A: <D = fun<X>(X) -> X> fun(D) -> D>", ["fun<X>(X) -> X ≢ fun(Int) -> Int", "fun<X>(X) -> X ≢ fun(Int) -> Int"])
  , ("unify(<A: <X> X>, A, Int)", "<A = Int>", [])
  , ("unify(<A: <X> X>, Int, A)", "<A = Int>", [])
  , ("unify(<A: <X> X, B: <X> X>, A, B)", "<A, B = A>", [])
  , ("unify(<A: <X> X, B: <X> X>, B, A)", "<B, A = B>", [])
  , ("unify(<A = <X, Y> fun(Int) -> Int>, A, fun(Int) -> Int)", "<A = fun(Int) -> Int>", [])
  , ("unify(<A = <X, Y> fun(Y) -> Int, B = <X, Y> fun(Y) -> Int>, A, B)", "<A = fun<Y>(Y) -> Int, B = A>", [])
  , ("unify(<A = <X, Y> fun(Y) -> Int, B = fun<Y>(Y) -> Int>, A, B)", "<A = fun<Y>(Y) -> Int, B = A>", [])
  , ("unify(<A = <X, Y> fun(Y) -> Int, B = fun<Y>(Y) -> Int>, B, A)", "<B = fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<A = <Y, X> fun(Y) -> Int, B = <Y, X> fun(Y) -> Int>, A, B)", "<A = fun<Y>(Y) -> Int, B = A>", [])
  , ("unify(<A = <Y, X> fun(Y) -> Int, B = fun<Y>(Y) -> Int>, A, B)", "<A = fun<Y>(Y) -> Int, B = A>", [])
  , ("unify(<A = <Y, X> fun(Y) -> Int, B = fun<Y>(Y) -> Int>, B, A)", "<B = fun<Y>(Y) -> Int, A = B>", [])
  , ("unify(<X = <A: fun<B>(B) -> B> fun(A) -> A, Y = fun<C>(fun(C) -> C) -> (fun(C) -> C)>, X, Y)", "<Y = fun<C>(fun(C) -> C) -> fun(C) -> C, X = <A: fun<B>(B) -> B> fun(A) -> A>", ["<A: fun<B>(B) -> B> fun(A) -> A ≢ fun<C>(fun(C) -> C) -> fun(C) -> C"])
  , ("unify(<X: <A: fun<B>(B) -> B> fun(A) -> A, Y = fun<C>(fun(C) -> C) -> (fun(C) -> C)>, X, Y)", "<X = fun<C>(fun(C) -> C) -> fun(C) -> C, Y = X>", [])
  , ("unify(<X = <A: fun<B>(B) -> B> fun(A) -> A, Y: fun<C>(fun(C) -> C) -> (fun(C) -> C)>, X, Y)", "<Y: fun<C>(fun(C) -> C) -> fun(C) -> C, X = <A: fun<B>(B) -> B> fun(A) -> A>", ["<A: fun<B>(B) -> B> fun(A) -> A ≢ fun<C>(fun(C) -> C) -> fun(C) -> C"])
  , ("unify(<X: <A: fun<B>(B) -> B> fun(A) -> A, Y: fun<C>(fun(C) -> C) -> (fun(C) -> C)>, X, Y)", "<X: fun<C>(fun(C) -> C) -> fun(C) -> C, Y = X>", [])
  , ("unify(<X = <A: fun<B>(B) -> B> fun(A) -> A, Y = fun<C>(fun(C) -> C) -> (fun(C) -> C)>, Y, X)", "<Y = fun<C>(fun(C) -> C) -> fun(C) -> C, X = <A: fun<B>(B) -> B> fun(A) -> A>", ["<A: fun<B>(B) -> B> fun(A) -> A ≢ fun<C>(fun(C) -> C) -> fun(C) -> C"])
  , ("unify(<X: <A: fun<B>(B) -> B> fun(A) -> A, Y = fun<C>(fun(C) -> C) -> (fun(C) -> C)>, Y, X)", "<Y = fun<C>(fun(C) -> C) -> fun(C) -> C, X = Y>", [])
  , ("unify(<X = <A: fun<B>(B) -> B> fun(A) -> A, Y: fun<C>(fun(C) -> C) -> (fun(C) -> C)>, Y, X)", "<Y: fun<C>(fun(C) -> C) -> fun(C) -> C, X = <A: fun<B>(B) -> B> fun(A) -> A>", ["<A: fun<B>(B) -> B> fun(A) -> A ≢ fun<C>(fun(C) -> C) -> fun(C) -> C"])
  , ("unify(<X: <A: fun<B>(B) -> B> fun(A) -> A, Y: fun<C>(fun(C) -> C) -> (fun(C) -> C)>, Y, X)", "<Y: fun<C>(fun(C) -> C) -> fun(C) -> C, X = Y>", [])
  , ("unify(<T = nope>, T, T)", "<T = %error>", ["We could not find type `nope`."])
  , ("unify(<T1 = nope, T2 = nope>, T1, T2)", "<T1 = %error, T2 = %error>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<X, Y = Int>, X, Y)", "<Y = Int, X = Int>", [])
  , ("unify(<X, Y = Int>, Y, X)", "<Y = Int, X = Int>", [])
  , ("unify(<X, Y = nope>, X, Y)", "<Y = %error, X = %error>", ["We could not find type `nope`."])
  , ("unify(<X, Y = nope>, Y, X)", "<Y = %error, X = %error>", ["We could not find type `nope`."])
  , ("unify(<A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope>, A, B)", "<A = fun<Z>(Z) -> %error, B = fun<Z>(Z) -> %error>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope>, B, A)", "<A = fun<Z>(Z) -> %error, B = fun<Z>(Z) -> %error>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<C = nope, A = fun<Z>(Z) -> C, B = fun<Z>(Z) -> C>, A, B)", "<C = %error, A = fun<Z>(Z) -> C, B = A>", ["We could not find type `nope`."])
  , ("unify(<C = nope, A = fun<Z>(Z) -> C, B = fun<Z>(Z) -> C>, B, A)", "<C = %error, B = fun<Z>(Z) -> C, A = B>", ["We could not find type `nope`."])
  , ("unify(<X, Y, T = <E = Int, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, T, fun(X) -> Y)", "<Y = fun<Z>(Z) -> Int, X = fun<Z>(Z) -> Int, T = fun(X) -> Y>", [])
  , ("unify(<X, Y, T = <E = Int, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, fun(X) -> Y, T)", "<Y = fun<Z>(Z) -> Int, X = fun<Z>(Z) -> Int, T = fun(X) -> Y>", [])
  , ("unify(<X, Y, T = <E = nope, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, T, fun(X) -> Y)", "<Y = fun<Z>(Z) -> %error, X = fun<Z>(Z) -> %error, T = fun(X) -> Y>", ["We could not find type `nope`."])
  , ("unify(<X, Y, T = <E = nope, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, fun(X) -> Y, T)", "<Y = fun<Z>(Z) -> %error, X = fun<Z>(Z) -> %error, T = fun(X) -> Y>", ["We could not find type `nope`."])
  , ("unify(<X, Y, T = <E1 = nope, E2 = nope, A = fun<Z>(Z) -> E1, B = fun<Z>(Z) -> E2> fun(A) -> B>, T, fun(X) -> Y)", "<Y = fun<Z>(Z) -> %error, X = fun<Z>(Z) -> %error, T = fun(X) -> Y>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<X, Y, T = <E1 = nope, E2 = nope, A = fun<Z>(Z) -> E1, B = fun<Z>(Z) -> E2> fun(A) -> B>, fun(X) -> Y, T)", "<Y = fun<Z>(Z) -> %error, X = fun<Z>(Z) -> %error, T = fun(X) -> Y>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<X, Y, T = <A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope> fun(A) -> B>, T, fun(X) -> Y)", "<Y = fun<Z>(Z) -> %error, X = fun<Z>(Z) -> %error, T = fun(X) -> Y>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<X, Y, T = <A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope> fun(A) -> B>, fun(X) -> Y, T)", "<Y = fun<Z>(Z) -> %error, X = fun<Z>(Z) -> %error, T = fun(X) -> Y>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<X, Y, T = <E = nope, A = fun<Z>(Z) -> E, B = fun<Z>(Z) -> E> fun(A) -> B>, fun(T) -> (fun(Y) -> X), fun(fun(X) -> Y) -> (fun(X) -> Y))", "<Y = fun<Z>(Z) -> %error, X = Y, T = fun(X) -> Y>", ["We could not find type `nope`."])
  , ("unify(<X, Y, T = <E1 = nope, E2 = nope, A = fun<Z>(Z) -> E1, B = fun<Z>(Z) -> E2> fun(A) -> B>, fun(T) -> (fun(Y) -> X), fun(fun(X) -> Y) -> (fun(X) -> Y))", "<Y = fun<Z>(Z) -> %error, X = fun<Z>(Z) -> %error, T = fun(X) -> Y>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<X, Y, T = <A = fun<Z>(Z) -> nope, B = fun<Z>(Z) -> nope> fun(A) -> B>, fun(T) -> (fun(Y) -> X), fun(fun(X) -> Y) -> (fun(X) -> Y))", "<Y = fun<Z>(Z) -> %error, X = fun<Z>(Z) -> %error, T = fun(X) -> Y>", ["We could not find type `nope`.", "We could not find type `nope`."])
  , ("unify(<A, B>, A, B)", "<A, B = A>", [])
  ]

unifyParser :: Parser (Recover CST.QuantifierList, Recover CST.Type, Recover CST.Type)
unifyParser = identifier *> glyph ParenLeft *> args <* glyph ParenRight
  where
    args =
      (,,)
        <$> (retry tryQuantifierListParser <* glyph Comma)
        <*> (typeParser <* glyph Comma)
        <*> typeParser

spec :: Spec
spec =
  flip traverse_ testData $ \(input, expectedPrefix, expectedDiagnostics) ->
    it (Text.unpack input) $ do
      let ((cqs, ct1, ct2), ds1) = runDiagnosticWriter (fst <$> (runParser unifyParser (tokenize input)))
      traverse_ (error . Text.Lazy.unpack . Text.Builder.toLazyText . debugDiagnostic) ds1
      let
        -- Use the quantifier list to quantify a boolean type. Could be anything really. We just
        -- need to send it through our conversion and type checking pipeline.
        (t3, ds2) = runDiagnosticWriter . checkPolytype mempty . AST.convertRecoverType . Ok $ case cqs of
          Recover _ _ _ -> undefined
          Fatal _ _ -> undefined
          Ok cqs' -> CST.QuantifiedType cqs' (Ok (CST.VariableType (CST.Name (unsafeIdentifier "Bool") undefined)))

        -- Run some code in the check monad...
        (allBindings, ds3) = runCheck $ do
          prefix <- Prefix.new
          Prefix.withLevel prefix $ do
            -- Instantiate the quantifications for the mock type we created.
            case Type.polytypeDescription t3 of
              Type.Quantify bindings body -> Prefix.instantiate prefix bindings body *> return ()
              _ -> return ()
            -- Get all the names currently bound in our prefix.
            ctx <- Prefix.allBindingNames prefix
            pt1 <- liftDiagnosticWriter (checkPolytype ctx (AST.convertRecoverType ct1))
            pt2 <- liftDiagnosticWriter (checkPolytype ctx (AST.convertRecoverType ct2))
            -- We need monotypes. We can’t take polytypes.
            let t1 = case Type.polytypeDescription pt1 of { Type.Monotype' t -> t; _ -> undefined }
            let t2 = case Type.polytypeDescription pt2 of { Type.Monotype' t -> t; _ -> undefined }
            -- Yay! We can actually call unify now 😉
            _ <- unify prefix t1 t2
            -- Return a list of all the bindings in our prefix.
            Prefix.allBindings prefix

      -- Compare the actual prefix to the expected prefix.
      let actualPrefix = strictify (printCompactQuantifierList (map printBindingWithoutInlining allBindings))
      actualPrefix `shouldBe` expectedPrefix

      -- Compare all the expected diagnostics to each other.
      let actualDiagnostics = map (strictify . diagnosticMessageText) (toList (ds2 <> ds3))
      if not (null actualDiagnostics) then actualDiagnostics `shouldBe` expectedDiagnostics else mempty -- TODO: Remove this. It is only to pretty up our test output.

strictify :: Text.Builder -> Text
strictify = Text.Lazy.toStrict . Text.Builder.toLazyText
