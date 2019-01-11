module Main (main) where

import Brite.Diagnostics
import Brite.Syntax.Parser
import qualified Brite.Syntax.PrinterAST as PrinterAST
import Brite.Syntax.Printer
import Brite.Syntax.Tokens
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

main :: IO ()
main = do
  input <- Text.IO.readFile ".play/test.ite"
  let (mod, diagnostics) = runDiagnosticWriter (parseModule (tokenize input))
  traverse_ (Text.IO.putStrLn . toStrictText . debugDiagnostic) diagnostics
  let output = toStrictText (printModule (PrinterAST.convertModule mod))
  seq output $ Text.IO.writeFile ".play/test.ite" output

toStrictText :: Text.Builder -> Text
toStrictText = Text.Lazy.toStrict . Text.Builder.toLazyText
