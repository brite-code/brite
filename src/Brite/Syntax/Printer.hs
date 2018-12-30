-- Responsible for pretty printing Brite programs from a tree structure back into text. This
-- printer will not print the _exact_ source text that constructed the trees, but rather a pretty
-- version. As a community, we expect all valid Brite programs to be formatted using this printer.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Printer
  ( printModule
  ) where

import Brite.Syntax.CST
import Brite.Syntax.PrinterFramework hiding (nest)
import qualified Brite.Syntax.PrinterFramework (nest)
import Brite.Syntax.Tokens
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
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

-- Adds indentation to the document. Uses the default indentation level.
nest :: Document -> Document
nest = Brite.Syntax.PrinterFramework.nest 2

-- Pretty prints a Brite AST module.
module_ :: Module -> Document
module_ (Module ss t) =
  mconcat (map (recover statement) ss)

-- Pretty prints a recovered AST node.
recover :: (a -> Document) -> Recover a -> Document
recover f (Ok a) = f a

-- Pretty prints a token.
token :: Token -> Document
token (Token _ k ts1 ts2) = trivia ts1 <> kind k
  where
    kind (Glyph g) = text (glyphText g)
    kind (IdentifierToken i) = text (identifierText i)
    kind (UnexpectedChar c) = text (T.singleton c)

-- Pretty prints some trivia.
trivia :: [Trivia] -> Document
trivia = loop0
  where
    raw = text . L.toStrict . B.toLazyText . triviaSource

    loop0 [] = mempty
    loop0 (Spaces _ : ts) = loop0 ts
    loop0 (Tabs _ : ts) = loop0 ts
    loop0 (Newlines _ _ : ts) = loop0 ts
    loop0 (OtherWhitespace _ : ts) = loop0 ts
    loop0 (t@(Comment _) : ts) = raw t <> loop1 PrettySpace ts

    loop1 acc [] = prettyTriviaText acc
    loop1 acc (Spaces _ : ts) = loop1 (prettyTriviaMerge acc PrettySpace) ts
    loop1 acc (Tabs _ : ts) = loop1 (prettyTriviaMerge acc PrettySpace) ts
    loop1 acc (Newlines _ n : ts) = loop1 (prettyTriviaMerge acc (PrettyNewlines n)) ts
    loop1 acc (OtherWhitespace _ : ts) = loop1 (prettyTriviaMerge acc PrettySpace) ts
    loop1 acc (t@(Comment _) : ts) = prettyTriviaText acc <> raw t <> loop1 PrettyEmpty ts

-- Intermediate Representation of pretty printed trivia.
data PrettyTrivia
  = PrettyEmpty
  | PrettySpace
  | PrettyNewlines Int

-- Merges two pretty printed trivia together.
prettyTriviaMerge :: PrettyTrivia -> PrettyTrivia -> PrettyTrivia
prettyTriviaMerge (PrettyNewlines n) (PrettyNewlines m) = PrettyNewlines (n + m)
prettyTriviaMerge (PrettyNewlines n) _ = PrettyNewlines n
prettyTriviaMerge _ (PrettyNewlines n) = PrettyNewlines n
prettyTriviaMerge PrettySpace _ = PrettySpace
prettyTriviaMerge _ PrettySpace = PrettySpace
prettyTriviaMerge PrettyEmpty PrettyEmpty = PrettyEmpty

-- Gets the document for pretty printed trivia.
prettyTriviaText :: PrettyTrivia -> Document
prettyTriviaText PrettyEmpty = mempty
prettyTriviaText PrettySpace = text " "
prettyTriviaText (PrettyNewlines n) | n > 1 = hardline <> hardline
prettyTriviaText (PrettyNewlines _) = hardline

-- Pretty prints a statement.
statement :: Statement -> Document
statement (ExpressionStatement e Nothing) = expression e <> text ";" <> hardline

-- Pretty prints a constant.
constant :: Constant -> Document
constant (BooleanConstant _ t) = token t

-- Pretty prints an expression.
expression :: Expression -> Document
expression (ConstantExpression c) = constant c
expression (WrappedExpression t1 (Ok e) a t2) =
  group (text "(" <> nest (expression e) <> text ")")
