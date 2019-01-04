-- Responsible for pretty printing Brite programs from a tree structure back into text. This
-- printer will not print the _exact_ source text that constructed the trees, but rather a pretty
-- version. As a community, we expect all valid Brite programs to be formatted using this printer.
--
-- Unlike other components of Brite, the printer is “best effort” based on heuristics we choose as a
-- community. It isn’t “this must be correct at all costs” like the type system. If there is even a
-- small edge case implemented incorrectly in the type system it could open Brite up to security
-- bugs. It’s fine for the printer to have small edge cases with suboptimal output, though. As long
-- as these edge cases are uncommon.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Printer
  ( printModule
  ) where

import Brite.Syntax.CST
import Brite.Syntax.PrinterFramework
import Brite.Syntax.Tokens
import Control.Applicative
import qualified Data.Text.Lazy.Builder as Text (Builder)

-- Pretty prints a Brite module.
printModule :: Module -> Text.Builder
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
module_ (Module [] t) = endTrivia (dropWhile isTriviaWhitespace (endTokenTrivia t))
module_ (Module ss t) = statementList ss <> endTrivia (endTokenTrivia t)

-- A monad around `Maybe` that provides a fine-tuned interface for panicking an operation. We use a
-- `newtype` to help reduce confusion and to rename operations.
newtype Panic a = Panic { toMaybe :: Maybe a }

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
  Panic a >>= f = Panic (a >>= toMaybe . f)

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

-- Pretty prints a list of statements either from a block or a module. If we panic when printing a
-- statement then this function catches that panic and prints the statement source instead.
statementList :: [Recover Statement] -> Document
statementList [] = mempty
statementList [s] = statementListItem s
statementList (s1 : ss@(s2 : _)) =
  statementListItem s1
    <> (if hasLeadingLine (recoverStatementLeadingTrivia s2) then hardline else mempty)
    <> statementList ss
  where
    hasLeadingLine [] = False
    hasLeadingLine (Spaces _ : ts) = hasLeadingLine ts
    hasLeadingLine (Tabs _ : ts) = hasLeadingLine ts
    hasLeadingLine (Newlines _ _ : _) = True
    hasLeadingLine (OtherWhitespace _ : ts) = hasLeadingLine ts
    hasLeadingLine (Comment _ : _) = False

-- Prints a single statement in a list of statements. If the statement has a parse error then this
-- function catches the parse error and prints the statement source instead of panicking. Always
-- prints a new line at the end of the statement.
statementListItem :: Recover Statement -> Document
statementListItem s =
  case toMaybe (recover s >>= statement) of
    Just t -> t <> hardline
    Nothing -> rawText (statementTrimmedSource s)

-- Pretty prints a statement. Always inserts a semicolon after every statement.
statement :: Statement -> Panic Document

-- Pretty print an expression statement. Always print the semicolon! Even if the semicolon was
-- not included.
statement (ExpressionStatement e' t') = do
  e <- expression Standalone e'
  t <- recoverMaybe t'
  return $ e <> maybe (text ";") token t

-- Pretty print a binding statement. Always print the semicolon! Even if the semicolon was
-- not included.
statement (BindingStatement t1 p' Nothing t2' e' t3') = do
  p <- recover p' >>= pattern
  t2 <- recover t2'
  e <- recover e' >>= (expression AssignmentValue)
  t3 <- recoverMaybe t3'
  return $
    token t1 <> text " " <> p <> text " " <> token t2 <> text " " <> e
      <> maybe (text ";") token t3

-- Pretty print a return statement. Always print the semicolon! Even if the semicolon was
-- not included.
statement (ReturnStatement t1 e' t2') = do
  e <- recoverMaybe e' >>= sequence . fmap (expression KeywordArgument)
  t2 <- recoverMaybe t2'
  return $ token t1 <> maybe mempty (text " " <>) e <> maybe (text ";") token t2

-- Pretty prints a constant.
constant :: Constant -> Document
constant (BooleanConstant _ t) = token t

-- A description of the location in which an expression is printed. Some expression printing rules
-- depend on context to pick the best print.
data ExpressionLocation
  -- There is no immediate surrounding code. For example `E;` or `f(E)`.
  = Standalone
  -- The value being assign in an assignment. For example `let x = E;` or `x = E;`
  | AssignmentValue
  -- An argument to some code coming after a keyword. For example `return E` or `if E {}`.
  | KeywordArgument
  -- Some part of an operation expression. Operation expressions have different precedence levels
  -- which we include here. For example `E + E` or `!E`.
  | Operand Precedence

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

-- Wrap expressions at a precedence level higher than the one provided.
wrap :: Precedence -> ExpressionLocation -> Document -> Document
wrap p1 (Operand p2) e | p2 < p1 = text "(" <> e <> text ")"
wrap _ _ e = e

-- Pretty prints an expression.
expression :: ExpressionLocation -> Expression -> Panic Document

-- Print a constant expression.
expression _ (ConstantExpression c) = return (constant c)

-- Print a variable expression.
expression _ (VariableExpression n) = return (name n)

-- Print a unary expression.
expression loc (UnaryExpression _ t e') = wrap Unary loc <$> do
  e <- recover e' >>= expression (Operand Unary)
  return (token t <> e)

-- Binary expressions of the same precedence level are placed in a single group.
expression loc (BinaryExpression l' (Ok (BinaryExpressionExtra op t r'))) = do
  l <- recover l' >>= expression (Operand precedence)
  r <- recover r' >>= expression (Operand precedence)
  return $ case loc of
    -- If our operation is at a greater precedence then we need to wrap it up.
    Operand p | p < precedence ->
      group (text "(" <> l <> text " " <> token t <> indent1 (line <> r) <> text ")")
    -- If our operation is at a lower precedence then we want to group it in case we can fit the
    -- operation on one line.
    Operand p | p > precedence -> group (l <> text " " <> token t <> line <> r)
    -- If our operation is at the same precedence then we want to inline it into the
    -- parent operation.
    Operand _ -> l <> text " " <> token t <> line <> r
    -- Don’t indent or anything at the top level.
    Standalone -> group (l <> text " " <> token t <> line <> r)
    -- In an assignment value indent but don’t wrap.
    AssignmentValue -> group (indent (softline <> l <> text " " <> token t <> line <> r))
    -- In a keyword argument both indent _and_ wrap.
    KeywordArgument ->
      group
        (text "("
          <> indent (softline <> l <> text " " <> token t <> line <> r)
          <> softline
          <> text ")")
  where
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
expression loc (WrappedExpression _ e Nothing t) = recover t *> recover e >>= expression loc

-- Group a property expression and indent its property on a newline if the group breaks.
expression _ (ExpressionExtra e' (Ok (PropertyExpressionExtra t n'))) = do
  e <- expression (Operand Primary) e'
  n <- recover n'
  return $ group $ (e <> indent (softline <> token t <> name n))

-- TODO: Finish call expressions
expression _ (ExpressionExtra e' (Ok (CallExpressionExtra t1 (CommaList [] (Just (Ok arg'))) t2'))) = do
  e <- expression (Operand Primary) e'
  arg <- expression Standalone arg'
  t2 <- recover t2'
  return $ e <> group
    (token t1
      <> indent (softline <> arg)
      <> lineSuffixFlush
      <> token t2)

-- Panic for all the other parse errors in expression extensions.
expression _ (BinaryExpression _ (Recover _ _ _)) = empty
expression _ (BinaryExpression _ (Fatal _ _)) = empty
expression _ (ExpressionExtra _ (Recover _ _ _)) = empty
expression _ (ExpressionExtra _ (Fatal _ _)) = empty

-- Pretty prints a pattern.
pattern :: Pattern -> Panic Document
pattern (VariablePattern n) = return $ name n

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
token (Token _ k ts1 ts2) = leadingTrivia ts1 <> text (tokenKindSource k) <> trailingTrivia ts2

-- Pretty prints the leading trivia of a token.
leadingTrivia :: [Trivia] -> Document
leadingTrivia ts = let (_, cs) = triviaToComments ts in comments cs
  where
    comments [] = mempty
    comments ((BlockComment c1 _, 0) : (LineComment c2, ls) : cs) =
      text "/*" <> text c1 <> text "*/ //" <> text c2
        <> (if ls > 1 then hardline <> hardline else hardline)
        <> comments cs
    comments ((LineComment c, ls) : cs) =
      linePrefix (text "//" <> text c <> (if ls > 1 then hardline <> hardline else hardline))
        <> forceBreak
        <> comments cs
    comments ((BlockComment c _, ls) : cs) =
      text "/*" <> text c <> text "*/"
        <> case ls of { 0 -> text " "; 1 -> hardline; _ -> hardline <> hardline }
        <> comments cs

-- Pretty prints the trailing trivia of a token.
trailingTrivia :: [Trivia] -> Document
trailingTrivia [] = mempty
trailingTrivia (Spaces _ : ts) = trailingTrivia ts
trailingTrivia (Tabs _ : ts) = trailingTrivia ts
trailingTrivia (Newlines _ _ : ts) = trailingTrivia ts
trailingTrivia (OtherWhitespace _ : ts) = trailingTrivia ts
trailingTrivia (Comment (LineComment c) : ts) = lineSuffix (text " //" <> text c) <> trailingTrivia ts
trailingTrivia (Comment (BlockComment c _) : ts) = text " /*" <> text c <> text "*/" <> trailingTrivia ts

-- Converts a list of trivia to a list of comments and the number of new lines in between
-- each comment.
triviaToComments :: [Trivia] -> (Int, [(Comment, Int)])
triviaToComments [] = (0, [])
triviaToComments (Spaces _ : ts) = triviaToComments ts
triviaToComments (Tabs _ : ts) = triviaToComments ts
triviaToComments (Newlines _ n : ts) = let (ls, cs) = triviaToComments ts in (ls + n, cs)
triviaToComments (OtherWhitespace _ : ts) = triviaToComments ts
triviaToComments (Comment c : ts) = let (ls, cs) = triviaToComments ts in (0, (c, ls) : cs)

-- Prints the trivia at the very end of a document.
endTrivia :: [Trivia] -> Document
endTrivia ts =
  let (ls, cs) = triviaToComments ts in
    if null cs then
      mempty
    else
      (if ls > 0 then hardline else mempty) <> comments cs
  where
    comments [] = mempty
    comments [(LineComment c, _)] = linePrefix (text "//" <> text c <> hardline)
    comments [(BlockComment c _, _)] = text "/*" <> text c <> text "*/" <> hardline
    comments ((BlockComment c1 _, 0) : (LineComment c2, ls) : cs) =
      text "/*" <> text c1 <> text "*/ //" <> text c2
        <> (if ls > 1 && not (null cs) then hardline <> hardline else hardline)
        <> comments cs
    comments ((LineComment c, ls) : cs) =
      linePrefix (text "//" <> text c <> (if ls > 1 then hardline <> hardline else hardline))
        <> comments cs
    comments ((BlockComment c _, ls) : cs) =
      text "/*" <> text c <> text "*/"
        <> case ls of { 0 -> text " "; 1 -> hardline; _ -> hardline <> hardline }
        <> comments cs
