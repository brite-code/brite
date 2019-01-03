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
import Control.Applicative
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

-- Pretty prints a Brite module.
module_ :: Module -> Document
module_ (Module ss t) =
  mconcat (map (fromJust . (>>= statement) . recover) ss)
  where
    fromJust (Panic (Just a)) = a
    fromJust (Panic Nothing) = error "fromJust"

-- A monad around `Maybe` that provides a fine-tuned interface for panicking an operation. We use a
-- `newtype` to help reduce confusion and to rename operations.
newtype Panic a = Panic { panicToMaybe :: Maybe a }

instance Functor Panic where
  fmap f (Panic a) = Panic (fmap f a)

instance Applicative Panic where
  pure a = Panic (pure a)
  Panic a <*> Panic b = Panic (a <*> b)
  liftA2 f (Panic a) (Panic b) = Panic (liftA2 f a b)

instance Alternative Panic where
  empty = Panic empty
  Panic a <|> Panic b = Panic (a <|> b)

instance Monad Panic where
  Panic a >>= f = Panic (a >>= panicToMaybe . f)
  Panic a >> Panic b = Panic (a >> b)

-- Recovers a value from a `Recover a` type. We only successfully recover if the `Recover` is `Ok`.
recover :: Recover a -> Panic a
recover (Ok a) = pure a
recover (Recover _ _ _) = empty
recover (Fatal _ _) = empty

-- Recovers an optional value from a `Maybe (Recover a)` type. We only successfully recover if the
-- `Recover` is `Ok`.
recoverMaybe :: Maybe (Recover a) -> Panic (Maybe a)
recoverMaybe Nothing = pure Nothing
recoverMaybe (Just (Ok a)) = pure (Just a)
recoverMaybe (Just (Recover _ _ _)) = empty
recoverMaybe (Just (Fatal _ _)) = empty

-- Pretty prints a name.
name :: Name -> Document
name = token . nameToken

----------------------------------------------------------------------------------------------------
-- # Comment Aesthetics
--
-- We need to answer the question: How are we going to pretty print comments? A programmer may put
-- a comment anywhere in their source code which might seriously disrupt the printer. First, to
-- understand what a “pretty” print involving comments might look like, let’s consider the aesthetic
-- of comments.
--
-- ## Line Comment Aesthetic
--
-- Line comments are used in Brite to document code. Programs are read left-to-right (sorry rtl
-- readers...) so the only thing which will end a line comment is a new line. No code may come after
-- a line comment on the same line. This makes the printing of line comments extra challenging.
--
-- The aesthetic for line comments we will say is “fluid decoration”. Line comments should not
-- impede the pretty printing of your code. They should not make your code less aesthetically
-- pleasing. After all, they are decorations. We are also free to move line comments around as we
-- please since part of their aesthetic is “fluid”. Just not too far from the author’s
-- original location.
--
-- There are two states a line comment might be in:
--
-- 1. Preceded by code: `a // ...`
-- 2. Not preceded by code: `// ...`
--
-- This makes the printing rules rather straightforward:
--
-- * A line comment that is preceded by code will stay on the same line as that code, but will be
--   printed at the end of the line the code was formatted onto. So if we have `a // ...` and `b`
--   and the printer chooses to put `a` and `b` on the same line the comment will move to the end of
--   the line like so `a b // ...`. This also means that if `b` has a line comment then both line
--   comments will be moved to the end of the line like so `a b // ... // ...`.
-- * A line comment that is not preceded by code will either stay on the line where it was written
--   or it will be moved to the next new line if the printer removes the line the comment was
--   sitting on. Ideally, we’d move the line _up_ to the previous line instead of down to the next
--   line, but this is a relatively uncommon case and moving comments down is easier to implement.
--   Users can manually move the comment back up if they like.
--
-- A line comment that is not preceded by code will break the group it was placed in. Simply because
-- it must considering we can’t put code after a line comment on the same line.
--
-- ## Block Comment Aesthetic
--
-- Block comments are included in Brite almost only to quickly hide some code. The programmer merely
-- needs to add `/*` at the start and `*/` at the end of the code they want to go bye-bye. Block
-- comments are not used for documentation as they are more difficult to type.
--
-- As such, the aesthetic for block comments can most succinctly be described as “quick and dirty”.
-- A block comment is used by a programmer who needs a quick and dirty comment.
--
-- There are four states a block comment might be in:
--
-- 1. Surrounded by code on both sides: `a /* */ b` (this comment is considered “attached”)
-- 2. Surrounded by code on the left: `a /* */` (this comment is considered “attached”)
-- 3. Surrounded by code on the right: `/* */ b` (this comment is considered “attached”)
-- 4. Surrounded by code on neither side: `/* */` (this comment is considered “detached”)
--
-- The printing rules for block comments are as follows:
--
-- * A block comment that is “attached” to a token (only spaces, not lines, between the comment and
--   the token) will be printed attached to the very same token.
-- * A block comment that is detached will continue to be detached. There may be at most one empty
--   line between the block comment and the next token.
--
-- In all these states a block comment can also contain a new line. If a block comment has a new
-- line it will automatically fail to fit its group on one line. However, the block comment will
-- stay attached.
----------------------------------------------------------------------------------------------------

-- Pretty prints a token.
token :: Token -> Document
token (Token _ k ts1 ts2) =
  leading ts1
    <> text (tokenKindSource k)
    <> trailing ts2
  where
    leading [] = mempty
    leading (Spaces _ : ts) = leading ts
    leading (Tabs _ : ts) = leading ts
    leading (Newlines _ _ : ts) = leading ts
    leading (OtherWhitespace _ : ts) = leading ts

    -- We know, for sure, that no code comes before a leading line comment. Code will eat a line
    -- comment that comes after it as trailing trivia. See the `trailing` function below. Line
    -- comments with no preceding code insert at most one empty new line.
    leading (Comment (LineComment c) : ts) =
      let (ls, ts') = newlines 0 ts in
        linePrefix (text "//" <> text c <> (if ls > 1 then hardline <> hardline else hardline))
          <> forceBreak
          <> leading ts'

    -- We know that there will never be any code before a leading block comment. We can also tell
    -- whether or not there is code after a leading block comment by counting the newlines which
    -- come after it.
    leading (Comment (BlockComment c _) : ts) =
      let
        (ls, ts') = newlines 0 ts
        sep = case ls of
          0 -> text " "
          1 -> hardline
          _ -> hardline <> hardline
      in
        text "/*" <> text c <> text "*/" <> sep <> leading ts'

    newlines n [] = (n, [])
    newlines n (Spaces _ : ts) = newlines n ts
    newlines n (Tabs _ : ts) = newlines n ts
    newlines n (Newlines _ m : ts) = newlines (n + m) ts
    newlines n (OtherWhitespace _ : ts) = newlines n ts
    newlines n ts@(Comment _ : _) = (n, ts)

    trailing [] = mempty
    trailing (Spaces _ : ts) = trailing ts
    trailing (Tabs _ : ts) = trailing ts
    trailing (Newlines _ _ : ts) = trailing ts
    trailing (OtherWhitespace _ : ts) = trailing ts

    -- We know that some code always comes before a trailing line comment. Defer printing the
    -- comment until the printer inserts a new line. This way the printer maintains the opportunity
    -- to format code as it pleases.
    trailing (Comment (LineComment c) : ts) =
      lineSuffix (text " //" <> text c) <> trailing ts

    -- We know that some code always comes before a trailing block comment. Add a space before the
    -- block comment to separate us from that code.
    --
    -- However, there is no way to tell whether or not there is code that comes after this
    -- block comment.
    trailing (Comment (BlockComment c _) : ts) =
      text " /*" <> text c <> text "*/" <> trailing ts

-- Pretty prints a statement. Always inserts a semicolon after every statement.
--
-- Returns `Nothing` if there was a parsing error anywhere in this statement.
statement :: Statement -> Panic Document

-- Pretty print an expression statement. Always print the semicolon! Even if the semicolon was
-- not included.
statement (ExpressionStatement e' t') = do
  e <- expression e'
  t <- recoverMaybe t'
  return $ neverWrap e <> maybe (text ";") token t <> hardline

-- Pretty print a binding  statement. Always print the semicolon! Even if the semicolon was
-- not included.
statement (BindingStatement t1 p' Nothing t2' e' t3') = do
  p <- recover p' >>= pattern
  t2 <- recover t2'
  e <- recover e' >>= expression
  t3 <- recoverMaybe t3'
  return $
    token t1 <> text " " <> p <> text " " <> token t2 <> text " " <> neverWrap e
      <> maybe (text ";") token t3 <> hardline

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
--
-- Returns `Nothing` if there was a parsing error anywhere in this expression.
expression :: Expression -> Panic (Precedence, Document)

-- Print a constant expression.
expression (ConstantExpression c) = return $ pair Primary $ constant c

-- Print a variable expression.
expression (VariableExpression n) = return $ pair Primary $ name n

-- Unary expressions are printed as expected.
expression (UnaryExpression _ t e) =
  pair Unary . (token t <>) . wrap Unary <$> (recover e >>= expression)

-- Binary expressions of the same precedence level are placed in a single group.
expression (BinaryExpression l' (Ok (BinaryExpressionExtra op t r'))) = do
  l <- recover l' >>= expression
  r <- recover r' >>= expression
  return $ pair precedence $ group $
    wrapOperand l <> text " " <> token t <> line <> wrapOperand r
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
expression (WrappedExpression _ e Nothing _) = recover e >>= expression

-- Group a property expression and indent its property on a newline if the group breaks.
expression (ExpressionExtra e' (Ok (PropertyExpressionExtra t n'))) = do
  e <- expression e'
  n <- recover n'
  return $ pair Primary $ group $
    wrap Primary e <> indent (softline <> token t <> name n)

-- TODO: Finish call expressions
expression (ExpressionExtra e' (Ok (CallExpressionExtra t1 (CommaList [] (Just (Ok arg'))) t2'))) = do
  e <- expression e'
  arg <- expression arg'
  t2 <- recover t2'
  return $ pair Primary $
    wrap Primary e <> group
      (token t1
        <> indent (softline <> neverWrap arg)
        <> lineSuffixFlush
        <> token t2)

-- Pretty prints a pattern.
--
-- Returns `Nothing` if there was a parsing error anywhere in this expression.
pattern :: Pattern -> Panic Document
pattern (VariablePattern n) = return $ name n
