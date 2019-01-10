{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Printer2
  ( printModule
  ) where

import Brite.Syntax.PrinterAST
import Brite.Syntax.PrinterFramework
import Data.Text.Lazy.Builder as Text (Builder)

-- Pretty prints a Brite module. The module must be from the printer AST.
printModule :: Module -> Text.Builder
printModule = printDocument maxWidth . error "unimplemented"

-- We pick 80 characters as our max width. That width will fit almost anywhere: Split pane IDEs,
-- GitHub, Terminals. It is also the best for plain text comments.
--
-- * Plain text (like comments) is best at 80 characters for readability.
-- * 88 characters is the most that will fit in a GitHub PR or issue comment.
-- * 125 characters is the most that will fit in a GitHub file view.
-- * The default max width for Prettier is 80 with 2 spaces of indentation. This can be a bit tight
--   on large screens with only one code window.
-- * The default max width for Rust is 100 with 4 spaces of indentation. This will fit less
--   horizontally then Prettier. Especially considering that all code must be inside a declaration.
--   Often times multiple declarations like `fn`s in `impl`s.
maxWidth :: Int
maxWidth = 80
