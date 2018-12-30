-- Responsible for pretty printing Brite programs from a tree structure back into text. This
-- printer will not print the _exact_ source text that constructed the trees, but rather a pretty
-- version. As a community, we expect all valid Brite programs to be formatted using this printer.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Printer
  ( printModule
  ) where

import Brite.Syntax.CST
import Brite.Syntax.PrinterFramework
import Brite.Syntax.Tokens
import qualified Data.Text.Lazy.Builder as B

-- Pretty prints a Brite module.
printModule :: Module -> B.Builder
printModule = printDocument maxWidth . module_

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

module_ :: Module -> Document
module_ (Module ss t) =
  mconcat (map (recover statement) ss)

recover :: (a -> Document) -> Recover a -> Document
recover f (Ok a) = f a

trivia :: [Trivia] -> Document
trivia = error "unimplemented"

statement :: Statement -> Document
statement (ExpressionStatement e Nothing) = expression e <> text ";" <> hardline

constant :: Constant -> Document
constant (BooleanConstant True t) = text "true"
constant (BooleanConstant False t) = text "false"

expression :: Expression -> Document
expression (ConstantExpression c) = constant c
