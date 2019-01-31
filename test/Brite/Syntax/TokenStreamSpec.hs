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
