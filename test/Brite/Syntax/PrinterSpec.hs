{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.PrinterSpec (spec) where

import Brite.Diagnostics
import Brite.Syntax.Parser
import Brite.Syntax.Printer
import Brite.Syntax.Tokens
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Test.Hspec
import System.IO

testData :: [T.Text]
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
  , "//\n//\ntrue"
  , "//\n\n//\n\ntrue"
  , "//\n\n\n//\n\n\ntrue"
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
    it (T.unpack (escape input)) $ \h ->
      let
        (inputModule, _) = runDiagnosticWriter (parseModule (tokenize input))
        output = L.toStrict (B.toLazyText (printModule inputModule))
        (outputModule, _) = runDiagnosticWriter (parseModule (tokenize output))
        reprintedOutput = L.toStrict (B.toLazyText (printModule outputModule))
      in do
        hPutStrLn h ""
        hPutStrLn h (replicate 80 '-')
        hPutStrLn h ""
        hPutStrLn h "### Input"
        hPutStrLn h "```ite"
        hPutStrLn h (T.unpack input)
        hPutStrLn h "```"
        hPutStrLn h ""
        hPutStrLn h "### Output"
        hPutStrLn h "```"
        hPutStr h (T.unpack output)
        hPutStrLn h "```"
        reprintedOutput `shouldBe` output

escape :: T.Text -> T.Text
escape = T.concatMap
  (\c ->
    case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ -> T.singleton c)
