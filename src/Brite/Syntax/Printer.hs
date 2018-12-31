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
import Data.Functor.Identity
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

-- Pretty prints a Brite module.
module_ :: Module -> Document
module_ (Module ss t) =
  mconcat (map (recover statement) ss)

-- Pretty prints a recovered value.
recover :: (a -> Document) -> Recover a -> Document
recover f = runIdentity . recoverM (Identity . f)

-- Pretty prints a recovered value when the provided function returns a functor.
recoverM :: Functor f => (a -> f Document) -> Recover a -> f Document
recoverM f (Ok a) = f a

-- Pretty prints a name.
name :: Name -> Document
name = token . nameToken

-- Pretty prints a token.
token :: Token -> Document
token (Token _ k lt tt) =
  text (tokenKindSource k) <> trailingTrivia tt

-- Pretty prints some trivia.
trailingTrivia :: [Trivia] -> Document
trailingTrivia = loop
  where
    loop [] = mempty
    loop (Spaces _ : ts) = loop ts
    loop (Tabs _ : ts) = loop ts
    loop (Newlines _ _ : ts) = loop ts
    loop (Comment (LineComment comment) : ts) = lineSuffix (text " //" <> text comment) <> loop ts
    loop (Comment (BlockComment _ _) : ts) = loop ts -- TODO: Block comments!!!
    loop (OtherWhitespace _ : ts) = loop ts

-- Pretty prints a statement. Always inserts a semicolon after every statement.
statement :: Statement -> Document
statement (ExpressionStatement e t) =
  neverWrap (expression e) <> maybe (text ";") (recover token) t <> hardline
statement (BindingStatement t1 p Nothing t2 e t3) =
  token t1
    <> text " "
    <> recover pattern p
    <> text " "
    <> recover token t2
    <> text " "
    <> neverWrap (recoverM expression e)
    <> maybe (text ";") (recover token) t3
    <> hardline

-- Pretty prints a constant.
constant :: Constant -> Document
constant (BooleanConstant _ t) = token t

-- The precedence level of an expression.
data Precedence
  = Primary
  | Unary
  | Exponentiation
  | Multiplicative
  | Additive
  | Relational
  | Equality
  | LogicalAnd
  | LogicalOr
  deriving (Eq, Ord)

-- Small tuple shortcut.
pair :: a -> b -> (a, b)
pair = (,)
{-# INLINE pair #-}

-- Never wrap the expression in parentheses.
neverWrap :: (Precedence, Document) -> Document
neverWrap (_, e) = e

-- Wrap expressions at a precedence level higher than the one provided.
wrap :: Precedence -> (Precedence, Document) -> Document
wrap p1 (p2, e) | p2 > p1 = text "(" <> e <> text ")"
wrap _ (_, e) = e

-- Pretty prints an expression.
expression :: Expression -> (Precedence, Document)
expression (ConstantExpression c) = pair Primary $ constant c
expression (VariableExpression n) = pair Primary $ name n

-- Unary expressions are printed as expected.
expression (UnaryExpression _ t e) = pair Unary $
  token t <> wrap Unary (recoverM expression e)

-- Binary expressions of the same precedence level are placed in a single group.
expression (BinaryExpression l (Ok (BinaryExpressionExtra op t r))) = pair precedence $ group $
  wrapOperand (recoverM expression l)
    <> text " "
    <> token t
    <> line
    <> wrapOperand (recoverM expression r)
  where
    -- If our operand is at a greater precedence then we need to wrap it up.
    wrapOperand (p, e) | p > precedence = text "(" <> e <> text ")"
    -- If our operand is at a lesser precedence then we want to leave it grouped.
    wrapOperand (p, e) | p < precedence = e
    -- If our operand is at the same precedence then we want to inline it into our group. Only other
    -- binary expressions should be at the same precedence.
    wrapOperand (_, e) = shamefullyUngroup e

    precedence = case op of
      Add -> Additive
      Subtract -> Additive
      Multiply -> Multiplicative
      Divide -> Multiplicative
      Remainder -> Multiplicative
      Exponent -> Exponentiation
      Equals -> Equality
      NotEquals -> Equality
      LessThan -> Relational
      LessThanOrEqual -> Relational
      GreaterThan -> Relational
      GreaterThanOrEqual -> Relational
      And -> LogicalAnd
      Or -> LogicalOr

-- Always remove unnecessary parentheses.
expression (WrappedExpression _ e Nothing _) =
  recoverM expression e

-- Group a property expression and nest its property on a newline if the group breaks.
expression (ExpressionExtra e (Ok (PropertyExpressionExtra t n))) = pair Primary $ group $
  wrap Primary (expression e) <> nest (softline <> token t <> recover name n)

-- Pretty prints a pattern.
pattern :: Pattern -> Document
pattern (VariablePattern n) = name n
