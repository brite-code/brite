{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Printer2
  ( printModule
  ) where

import Brite.Syntax.PrinterAST
import Brite.Syntax.PrinterFramework
import Brite.Syntax.Tokens
import Data.Char (isSpace)
import Data.Text as Text
import Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Builder as Text (Builder)
import Data.Text.Lazy.Builder as Text.Builder

-- Pretty prints a Brite module. The module must be from the printer AST.
printModule :: Module -> Text.Builder
printModule = printDocument maxWidth . printStatementSequence . moduleStatements

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

-- Prints an unattached comment from the printer AST.
printUnattachedComment :: UnattachedComment -> Document
printUnattachedComment (UnattachedComment l c0) = (if l then hardline else mempty) <> case c0 of
  LineComment c -> text "//" <> text (Text.dropWhileEnd isSpace c)
  BlockComment c _ ->
    text "/*"
      <> text (Text.Lazy.toStrict (Text.Builder.toLazyText (removeTrailingSpaces c)))
      <> text "*/"

-- Prints a sequence of statements or comments.
printStatementSequence :: [MaybeComment Statement] -> Document
printStatementSequence ss0 = loopStart ss0
  where
    loopStart [] = mempty
    loopStart (Left (UnattachedComment True c) : ss) =
      loop (Left (UnattachedComment False c) : ss)
    loopStart (Right (Statement True cs1 cs2 s) : ss) =
      loop (Right (Statement False cs1 cs2 s) : ss)
    loopStart ss =
      loop ss

    loop [] = mempty
    loop (Left c : ss) = printUnattachedComment c <> hardline <> loop ss
    loop (Right s : ss) = printStatement s <> hardline <> loop ss

-- Prints a single statement.
printStatement :: Statement -> Document
printStatement = error "unimplemented"
