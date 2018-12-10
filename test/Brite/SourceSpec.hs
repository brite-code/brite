{-# LANGUAGE OverloadedStrings #-}

module Brite.SourceSpec (spec) where

import Brite.Source
import Test.Hspec

toList :: TokenList -> ([(Range, Token)], Position)
toList (NextToken range token l) = ((range, token) : l', end) where (l', end) = toList l
toList (EndToken end) = ([], end)

spec = describe "tokenize" $ do
  it "parses glyphs" $
    toList (tokenize initialPosition "{},.=();/") `shouldBe`
      ( [ (Range (Position 0 0) (Position 0 1), GlyphToken BraceLeft)
        , (Range (Position 0 1) (Position 0 2), GlyphToken BraceRight)
        , (Range (Position 0 2) (Position 0 3), GlyphToken Comma)
        , (Range (Position 0 3) (Position 0 4), GlyphToken Dot)
        , (Range (Position 0 4) (Position 0 5), GlyphToken Equals)
        , (Range (Position 0 5) (Position 0 6), GlyphToken ParenLeft)
        , (Range (Position 0 6) (Position 0 7), GlyphToken ParenRight)
        , (Range (Position 0 7) (Position 0 8), GlyphToken Semicolon)
        , (Range (Position 0 8) (Position 0 9), GlyphToken Slash)
        ]
      , Position 0 9
      )

  it "parses unexpected characters" $
    toList (tokenize initialPosition "€") `shouldBe`
      ([(Range (Position 0 0) (Position 0 1), UnexpectedChar '€')], Position 0 1)

  it "parses unexpected characters made up of two UTF-16 code units" $
    toList (tokenize initialPosition "𐐷") `shouldBe`
      ([(Range (Position 0 0) (Position 0 2), UnexpectedChar '𐐷')], Position 0 2)

  it "skips whitespace" $ do
    toList (tokenize initialPosition "") `shouldBe` ([], Position 0 0)
    toList (tokenize initialPosition " ") `shouldBe` ([], Position 0 1)
    toList (tokenize initialPosition "  ") `shouldBe` ([], Position 0 2)
    toList (tokenize initialPosition "   ") `shouldBe` ([], Position 0 3)
    toList (tokenize initialPosition "\t") `shouldBe` ([], Position 0 1)
    toList (tokenize initialPosition "\f") `shouldBe` ([], Position 0 1)
    toList (tokenize initialPosition "\v") `shouldBe` ([], Position 0 1)
    toList (tokenize initialPosition "\x00A0") `shouldBe` ([], Position 0 1)
    toList (tokenize initialPosition "\x2002") `shouldBe` ([], Position 0 1)
    toList (tokenize initialPosition "\x2003") `shouldBe` ([], Position 0 1)
    toList (tokenize initialPosition "\x2009") `shouldBe` ([], Position 0 1)

  it "skips newlines" $ do
    toList (tokenize initialPosition "") `shouldBe` ([], Position 0 0)
    toList (tokenize initialPosition "\n") `shouldBe` ([], Position 1 0)
    toList (tokenize initialPosition "\n\n") `shouldBe` ([], Position 2 0)
    toList (tokenize initialPosition " \n") `shouldBe` ([], Position 1 0)
    toList (tokenize initialPosition "\n ") `shouldBe` ([], Position 1 1)
    toList (tokenize initialPosition "\r") `shouldBe` ([], Position 1 0)
    toList (tokenize initialPosition "\r\r") `shouldBe` ([], Position 2 0)
    toList (tokenize initialPosition " \r") `shouldBe` ([], Position 1 0)
    toList (tokenize initialPosition "\r ") `shouldBe` ([], Position 1 1)
    toList (tokenize initialPosition "\r\n") `shouldBe` ([], Position 1 0)
    toList (tokenize initialPosition "\r\n\r\n") `shouldBe` ([], Position 2 0)
    toList (tokenize initialPosition " \r\n") `shouldBe` ([], Position 1 0)
    toList (tokenize initialPosition "\r\n ") `shouldBe` ([], Position 1 1)
    toList (tokenize initialPosition "\n\r\r\n") `shouldBe` ([], Position 3 0)
    toList (tokenize initialPosition "\n\r") `shouldBe` ([], Position 2 0)
