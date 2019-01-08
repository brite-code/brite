-- Module for debugging an AST as S-expressions. Uses the printing framework from
-- `Brite.Syntax.PrinterFramework`.

module Brite.Semantics.ASTDebug
  ( debugModule
  ) where

import Brite.Semantics.AST
import Brite.Syntax.PrinterFramework
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder)

-- An [S-expression][1].
--
-- [1]: https://en.wikipedia.org/wiki/S-expression
data S
  = A Text
  | E S S

-- Prints a Brite AST module to an S-expression text form for debugging purposes.
debugModule :: Module -> Text.Builder
debugModule = error "unimplemented"
