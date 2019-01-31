{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.TokenStreamSpec (spec) where

import Brite.Diagnostic
import Brite.Syntax.Token
import Brite.Syntax.TokenStream
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import Test.Hspec
import System.IO

testData :: [Text]
testData =
  [ "{},.=();/"
  , "€"
  , "😈"
  , "//"
  , " //"
  , "  //"
  , "   //"
  , "\t"
  , "\t\t"
  , "\t\t\t"
  , "\f"
  , "\v"
  , "\x00A0"
  , "\x2002"
  , "\x2003"
  , "\x2009"
  , "\n"
  , "\n\n"
  , " \n"
  , "\n "
  , "\r"
  , "\r\r"
  , " \r"
  , "\r "
  , "\r\n"
  , "\r\n\r\n"
  , " \r\n"
  , "\r\n "
  , "\n\r\r\n"
  , "\r\n\r\n"
  , "\r\r\n\n"
  , "\n\r\n\r"
  , "\n\n\r\r"
  , "\n\r"
  , "x"
  , "foo"
  , "Bar"
  , "_42"
  , "Θ"
  , "𐐷"
  , "u𐐷"
  , "𐐷w"
  , "_"
  , "true"
  , "false"
  , "void"
  , "let"
  , "if"
  , "else"
  , "do"
  , "/"
  , "//"
  , "// abc"
  , "// abc\n"
  , "// abc\nx"
  , "// abc\rx"
  , "// abc\r\nx"
  , "// 😈"
  , "/"
  , "/*"
  , "/* "
  , "/* /"
  , "/* *"
  , "/* */"
  , "/* **"
  , "/* **/"
  , "/* * */"
  , "/* / */"
  , "/* \n */"
  , "/* \n\n */"
  , "/* \r */"
  , "/* \r\r */"
  , "/* \r\n */"
  , "/* \r\n\r\n */"
  , "/* 😈 */"
  , "/* € */"
  , "/* 😈 😈 */"
  , "/* € € */"
  , "/* */ x"
  , "/* **/ x"
  , "/*/ x"
  , "/**/ x"
  , "a   b"
  , "a \n b"
  , "a \r b"
  , "a \r\n b"
  , "a \n\n b"
  , "a \n\n\n b"
  , "a \r\r b"
  , "a \r\r\r b"
  , "a \r\n\r\n b"
  , "a \r\n\r\n\r\n b"
  , "a /* */ b"
  , "a /* */ /* */ b"
  , "a /* \n */ b"
  , "a /* \r */ b"
  , "a /* \r\n */ b"
  , "= ="
  , "=="
  , "! !"
  , "!!"
  , "! ="
  , "!="
  , "> ="
  , ">="
  , "< ="
  , "<="
  , "= >"
  , "=>"
  , "&&"
  , "- >"
  , "->"
  , "/* /* x"
  , "/* /* */ x"
  , "/* /* */ */ x"
  , "/* /* /* x"
  , "/* /* /* */ x"
  , "/* /* /* */ */ x"
  , "/* /* /* */ */ */ x"
  , "/* /* */ /* */ x"
  , "/* /* */ /* */ */ x"
  , "/* */* */ x"
  , "x /*"
  , "0b"
  , "0B"
  , "0b "
  , "0B "
  , "0b2"
  , "0B2"
  , "0bc"
  , "0Bc"
  , "0b😈"
  , "0B😈"
  , "0b0"
  , "0B0"
  , "0b1"
  , "0B1"
  , "0b01"
  , "0B01"
  , "0b10"
  , "0B10"
  , "0b101010100011"
  , "0B101010100011"
  , "0b000001010111"
  , "0B000001010111"
  , "/**/ 0b"
  , "/**/ 0B"
  , "/**/ 0b "
  , "/**/ 0B "
  , "/**/ 0b2"
  , "/**/ 0B2"
  , "/**/ 0b😈"
  , "/**/ 0B😈"
  , "0b02"
  , "0b0x"
  , "0b0𐐷"
  , "0b0x2"
  , "0b02x"
  , "0b0𐐷x"
  , "0b0x𐐷"
  , "0b0x𐐷x"
  , "0b02𐐷2"
  , "0b2"
  , "0bx"
  , "0b𐐷"
  , "0bx2"
  , "0b2x"
  , "0b𐐷x"
  , "0bx𐐷"
  , "0bx𐐷x"
  , "0b2𐐷2"
  , "0b0 x"
  , "x 0b0"
  , "x0b0"
  , "0b0x"
  ]

openSnapshotFile :: IO Handle
openSnapshotFile = do
  h <- openFile "test/Brite/Syntax/TokenStreamSpecSnapshot.md" WriteMode
  hPutStrLn h "# TokenStreamSpecSnapshot"
  return h

closeSnapshotFile :: Handle -> IO ()
closeSnapshotFile h = do
  hPutStrLn h ""
  hPutStrLn h (replicate 80 '-')
  hClose h

spec :: Spec
spec = beforeAll openSnapshotFile $ afterAll closeSnapshotFile $ do
  flip mapM_ testData $ \source ->
    it (Text.unpack (escape source)) $ \h ->
      let
        ((tokens, endToken), diagnostics) = runDiagnosticWriter (tokenStreamToList (tokenize source))
        rebuiltSource = Text.Lazy.toStrict $ Text.Builder.toLazyText $
          mconcat (map tokenSource tokens) <> endTokenSource endToken
      in do
        hPutStrLn h ""
        hPutStrLn h (replicate 80 '-')
        hPutStrLn h ""
        hPutStrLn h "### Source"
        hPutStrLn h "```ite"
        hPutStrLn h (Text.unpack source)
        hPutStrLn h "```"
        hPutStrLn h ""
        hPutStrLn h "### Tokens"
        hPutStrLn h "```"
        hPutStr h (Text.Lazy.unpack (Text.Builder.toLazyText (debugTokens tokens endToken)))
        hPutStrLn h "```"
        if Seq.null diagnostics then return () else (do
          hPutStrLn h ""
          hPutStrLn h "### Errors"
          flip mapM_ diagnostics (\diagnostic ->
            hPutStrLn h (Text.Lazy.unpack (Text.Builder.toLazyText
              (Text.Builder.fromText "- " <> debugDiagnostic diagnostic)))))
        rebuiltSource `shouldBe` source

escape :: Text -> Text
escape = Text.concatMap
  (\c ->
    case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\f' -> "\\f"
      '\v' -> "\\v"
      '\x00A0' -> "\\x00A0"
      '\x2002' -> "\\x2002"
      '\x2003' -> "\\x2003"
      '\x2009' -> "\\x2009"
      _ -> Text.singleton c)
