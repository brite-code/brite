-- Responsible for pretty printing Brite programs from a tree structure back into text. This
-- printer will not print the _exact_ source text that constructed the trees, but rather a pretty
-- version. As a community, we expect all valid Brite programs to be formatted using this printer.
--
-- Unlike other components of Brite, the printer is “best effort” based on heuristics we choose as a
-- community. It isn’t “this must be correct at all costs” like the type system. If there is even a
-- small edge case implemented incorrectly in the type system it could open Brite up to security
-- bugs. It’s fine for the printer to have small edge cases with suboptimal output, though. As long
-- as these edge cases are uncommon.
--
-- NOTE: It would be a good idea to generate Brite programs and make sure that
-- `print(code) = print(print(code))`. Also that there are never any trailing spaces.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Printer
  ( printModule
  ) where

import Brite.Syntax.CST
import Brite.Syntax.PrinterFramework
import Brite.Syntax.Tokens
import Control.Applicative
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
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
module_ (Module ss t) = statementSequence ss <> endTrivia (endTokenTrivia t)

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

-- Pretty prints a comma list. Always inserts a trailing comma if the list is broken onto
-- multiple lines.
commaList :: (a -> Panic Document) -> CommaList a -> Panic Document
commaList _ (CommaList [] Nothing) = return (indent softline)
commaList f (CommaList as0 an0) = do
  x <- loop as0 an0
  return $ indent (softline <> x <> softline)
  where
    loop [] Nothing = return mempty
    loop [(a', t')] Nothing = do
      a <- recover a' >>= f
      t <- recover t'
      return $ a <> ifBreakElse (token t) (removeToken t)
    loop [] (Just an') = do
      an <- recover an' >>= f
      return $ an <> ifBreak (text ",")
    loop ((a', t') : as) an = do
      a <- recover a' >>= f
      t <- recover t'
      (a <>) . (token t <>) . (line <>) <$> loop as an

-- Pretty prints a list of statements either from a block or a module. If we panic when printing a
-- statement then this function catches that panic and prints the statement source instead.
--
-- Empty statements get special handling since we are removing them from the source.
statementSequence :: [Recover Statement] -> Document

-- An empty statement between two other statements should do the `triviaHasLeadingLine` check for
-- both of them and insert an extra new line.
statementSequence (s1 : Ok (EmptyStatement t) : ss@(s2 : _)) =
  statementListItem s1
    <> (if extra then hardline else mempty)
    <> removeToken t
    <> statementSequence ss
  where
    extra =
      triviaHasLeadingLine (tokenLeadingTrivia t) ||
      triviaHasLeadingLine (error "unimplemented")

-- Empty statements do not get trailing new lines or extra lines for extra spaces. We are removing
-- empty statements so two empty lines after re-printing will be collapsed to one empty line.
statementSequence (Ok (EmptyStatement t) : ss) = removeToken t <> statementSequence ss
statementSequence [s1, Ok (EmptyStatement t)] = statementListItem s1 <> removeToken t

-- Insert an extra new line between statements who in source have one empty line between them.
statementSequence [] = mempty
statementSequence [s] = statementListItem s
statementSequence (s1 : ss@(s2 : _)) =
  statementListItem s1
    <> (if triviaHasLeadingLine (error "unimplemented") then hardline else mempty)
    <> statementSequence ss

-- Does this trivia list have a leading new line?
triviaHasLeadingLine :: [Trivia] -> Bool
triviaHasLeadingLine [] = False
triviaHasLeadingLine (Spaces _ : ts) = triviaHasLeadingLine ts
triviaHasLeadingLine (Tabs _ : ts) = triviaHasLeadingLine ts
triviaHasLeadingLine (Newlines _ _ : _) = True
triviaHasLeadingLine (OtherWhitespace _ : ts) = triviaHasLeadingLine ts
triviaHasLeadingLine (Comment _ : _) = False

-- Prints a single statement in a list of statements. If the statement has a parse error then this
-- function catches the parse error and prints the statement source instead of panicking. Always
-- prints a new line at the end of the statement.
statementListItem :: Recover Statement -> Document
statementListItem s =
  case toMaybe (recover s >>= statement) of
    Just t -> t <> hardline
    Nothing -> rawText (error "TODO: unimplemented")

-- Pretty prints a statement. Always inserts a semicolon after every statement.
statement :: Statement -> Panic Document

-- Pretty print an expression statement. Always print the semicolon! Even if the semicolon was
-- not included.
statement (ExpressionStatement e' t') = do
  e <- expression Standalone e'
  t <- recoverMaybe t'
  if noSemicolon e' then return $ e <> maybe mempty removeToken t
  else return $ e <> maybe (text ";") token t
  where
    noSemicolon (ConstantExpression _) = False
    noSemicolon (VariableExpression _) = False
    noSemicolon (FunctionExpression _ _) = False
    noSemicolon (ObjectExpression _ _ _ _) = False
    noSemicolon (UnaryExpression _ _ _) = False
    noSemicolon (ConditionalExpression _) = True
    noSemicolon (BlockExpression _ _) = True
    noSemicolon (LoopExpression _ _) = True
    noSemicolon (WrappedExpression _ (Ok e) Nothing _) = noSemicolon e
    noSemicolon (WrappedExpression _ _ _ _) = False
    noSemicolon (ExpressionExtra _ _) = False

-- Pretty print a binding statement. Always print the semicolon! Even if the semicolon was
-- not included.
statement (BindingStatement t1 p' Nothing t2' e' t3') = do
  bind <- recover p' >>= pattern
  t2 <- recover t2'
  e <- recover e'
  value <- expression AssignmentValue e
  t3 <- recoverMaybe t3'
  return $
    token t1
      <> text " "
      <> bind
      <> text " "
      <> token t2
      <> group ((if breaksOnNextLine e then ifFlat (text " ") else text " ") <> value)
      <> maybe (text ";") token t3
  where
    breaksOnNextLine (ExpressionExtra _ (Ok (BinaryExpressionExtra _ _))) = True
    breaksOnNextLine _ = False

-- Pretty print a return statement. Always print the semicolon! Even if the semicolon was
-- not included.
statement (ReturnStatement t1 e' t2') = do
  e <- recoverMaybe e' >>= sequence . fmap (expression KeywordArgument)
  t2 <- recoverMaybe t2'
  return $ token t1 <> maybe mempty (text " " <>) e <> maybe (text ";") token t2

-- To print an empty statement we remove the single semicolon token which comprised the
-- empty statement.
statement (EmptyStatement t) = return $ removeToken t

-- Pretty prints a block.
--
-- NOTE: Does not group the block! The caller needs to make sure they group their block.
ungroupedBlock :: Block -> Panic Document
ungroupedBlock (Block t1' [Ok (ExpressionStatement e' Nothing)] t2') = do
  t1 <- recover t1'
  e <- expression Standalone e'
  t2 <- recover t2'
  return $ token t1 <> indent (line <> e <> line) <> token t2
ungroupedBlock (Block t1' [Ok (ExpressionStatement e' (Just t2'))] t3') = do
  t1 <- recover t1'
  e <- expression Standalone e'
  t2 <- recover t2'
  t3 <- recover t3'
  return $ token t1 <> indent (line <> e <> removeToken t2 <> line) <> token t3
ungroupedBlock (Block t1' [] t2') = do
  t1 <- recover t1'
  t2 <- recover t2'
  return $ token t1 <> indent softline <> token t2
ungroupedBlock (Block t1' ss t2') = do
  t1 <- recover t1'
  t2 <- recover t2'
  return $ token t1 <> indent (hardline <> statementSequence ss) <> token t2

-- Pretty prints a block.
block :: Block -> Panic Document
block = fmap group . ungroupedBlock

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
  -- The expression was wrapped in parentheses. For example `(E)`.
  | Wrapped
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

-- The precedence of a binary operator.
binaryOperatorPrecedence :: BinaryOperator -> Precedence
binaryOperatorPrecedence Add = Additive
binaryOperatorPrecedence Subtract = Additive
binaryOperatorPrecedence Multiply = Multiplicative
binaryOperatorPrecedence Divide = Multiplicative
binaryOperatorPrecedence Remainder = Multiplicative
binaryOperatorPrecedence Exponent = Exponentiation
binaryOperatorPrecedence Equals = Equality
binaryOperatorPrecedence NotEquals = Equality
binaryOperatorPrecedence LessThan = Relational
binaryOperatorPrecedence LessThanOrEqual = Relational
binaryOperatorPrecedence GreaterThan = Relational
binaryOperatorPrecedence GreaterThanOrEqual = Relational
binaryOperatorPrecedence And = LogicalAnd
binaryOperatorPrecedence Or = LogicalOr

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
expression loc (ExpressionExtra l' (Ok (BinaryExpressionExtra (BinaryExpressionOperation op t r') []))) = do
  l <- expression (Operand precedence) l'
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
    -- Wrapped gets the same behavior as the case where we wrap the expression ourselves except we
    -- don’t add parentheses.
    Wrapped -> group (l <> text " " <> token t <> indent1 (line <> r))
    -- Don’t indent or anything at the top level.
    Standalone -> group (l <> text " " <> token t <> line <> r)
    -- In an assignment value indent but don’t wrap. Also, don’t group the entire thing. The caller
    -- is responsible for grouping.
    AssignmentValue -> indent (softline <> group (l <> text " " <> token t <> line <> r))
    -- In a keyword argument both indent _and_ wrap.
    KeywordArgument -> group $
      (ifBreak (text "(")
        <> indent (softline <> group (l <> text " " <> token t <> line <> r) <> softline)
        <> ifBreak (text ")"))
  where
    precedence = binaryOperatorPrecedence op

-- Transform binary expression back into a tree.
expression loc (ExpressionExtra l (Ok (BinaryExpressionExtra op1 (Ok op2 : ops)))) =
  expression loc
    (ExpressionExtra
      (ExpressionExtra l (Ok (BinaryExpressionExtra op1 [])))
      (Ok (BinaryExpressionExtra op2 ops)))
expression _ (ExpressionExtra _ (Ok (BinaryExpressionExtra _ (Recover _ _ _ : _)))) = empty
expression _ (ExpressionExtra _ (Ok (BinaryExpressionExtra _ (Fatal _ _ : _)))) = empty

-- Render block expressions.
expression _ (BlockExpression t b') = do
  b <- block b'
  return $ token t <> text " " <> b

-- Remove unnecessary parentheses and keep parentheses which are needed.
expression loc (WrappedExpression t1 e' Nothing t2') = do
  e <- recover e'
  t2 <- recover t2'
  if needsWrapping loc e then do
    wrapped <- expression Wrapped e
    return $ token t1 <> wrapped <> token t2
  else do
    wrapped <- expression loc e
    return $ removeToken t1 <> wrapped <> removeToken t2
  where
    needsWrapping (Operand p) (ExpressionExtra _ (Ok (BinaryExpressionExtra (BinaryExpressionOperation op _ _) _))) =
      p < binaryOperatorPrecedence op
    needsWrapping l (WrappedExpression _ (Ok e) Nothing _) = needsWrapping l e
    needsWrapping _ _ = False

-- Group a property expression and indent its property on a newline if the group breaks.
expression _ (ExpressionExtra e' (Ok (PropertyExpressionExtra t n'))) = do
  e <- expression (Operand Primary) e'
  n <- recover n'
  return $ group $ (e <> indent (softline <> token t <> name n))

-- Call expressions with a single argument never add a trailing comma. This was a pet-peeve of mine
-- (Caleb) in the JavaScript pretty printing library [Prettier][1]. One of the primary reasons for
-- trailing commas is to improve differences in the programmer’s source control manager (like git).
-- Adding a new line to a trailing comma list only changes one line. It does not also change the
-- line above it by adding a comma. It is also easy to copy/paste a new item in a trailing comma
-- list since you don’t need to worry about adding a new comma.
--
-- However, functions usually don’t have a variable number of arguments. Most of the time the number
-- of function arguments never changes so the convenience of a trailing comma list is not relevant.
-- Trailing commas in function calls with a single item do actively look worse (in my opinion),
-- though. Especially in JavaScript when you’d have an arrow function (`(x, y) => x + y`) and you’d
-- have to put a trailing comma after it.
--
-- This is my pretty printing framework now, so I get to call the shots.
--
-- [1]: https://prettier.io
expression _ (ExpressionExtra e' (Ok (CallExpressionExtra t1 (CommaList [] (Just arg')) t3'))) = do
  e <- expression (Operand Primary) e'
  arg <- recover arg' >>= expression Standalone
  t3 <- recover t3'
  return $ e <> group
    (token t1
      <> indent (softline <> arg <> softline)
      <> token t3)
expression _ (ExpressionExtra e' (Ok (CallExpressionExtra t1 (CommaList [(arg', t2')] Nothing) t3'))) = do
  e <- expression (Operand Primary) e'
  arg <- recover arg' >>= expression Standalone
  t2 <- recover t2'
  t3 <- recover t3'
  return $ e <> group
    (token t1
      <> indent (softline <> arg <> removeToken t2 <> softline)
      <> token t3)

-- For call expressions with more then one argument use the `commaList` helper function to print the
-- argument list. This will always add a trailing comma when the list is broken onto multiple lines.
expression _ (ExpressionExtra e' (Ok (CallExpressionExtra t1 args' t3'))) = do
  e <- expression (Operand Primary) e'
  args <- commaList (expression Standalone) args'
  t3 <- recover t3'
  return $ e <> group (token t1 <> args <> token t3)

-- Panic for all the other parse errors in expression extensions.
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
token (Token _ k ts1 ts2) =
  leading ts1 <> text (error "TODO: unimplemented") <> trailing ts2
  where
    leading ts = let (_, cs) = triviaToComments ts in comments True (reverse cs)
      where
        comments _ [] = mempty
        comments lastComment ((LineComment c, ls) : cs) =
          comments False cs
            <> forceBreak
            <> linePrefix
              (text "//" <> text (trimTrailingWhitespace c)
                <> (if ls > 1 && not (lastComment && noExtraLine k) then hardline else mempty))
        comments True ((BlockComment c _, 0) : cs) =
          comments True cs
            <> text "/*" <> text c <> text "*/ "
        comments lastComment ((BlockComment c _, ls) : cs) =
          comments False cs
            <> forceBreak
            <> linePrefix
              (text "/*" <> text c <> text "*/"
                <> (if ls > 1 && not (lastComment && noExtraLine k) then hardline else mempty))

    trailing [] = mempty
    trailing (Spaces _ : ts) = trailing ts
    trailing (Tabs _ : ts) = trailing ts
    trailing (Newlines _ _ : ts) = trailing ts
    trailing (OtherWhitespace _ : ts) = trailing ts
    trailing (Comment (LineComment c) : ts) = lineSuffix (text " //" <> text (trimTrailingWhitespace c)) <> trailing ts
    trailing (Comment (BlockComment c _) : ts) = text " /*" <> text c <> text "*/" <> trailing ts

    -- Should we trim the extra line after the last leading comment?
    noExtraLine (Glyph ParenRight) = True
    noExtraLine (Glyph BraceRight) = True
    noExtraLine (Glyph BracketRight) = True
    noExtraLine (Glyph GreaterThan_) = True
    noExtraLine _ = False

-- Converts a list of trivia to a list of comments and the number of new lines in between
-- each comment.
triviaToComments :: [Trivia] -> (Int, [(Comment, Int)])
triviaToComments [] = (0, [])
triviaToComments (Spaces _ : ts) = triviaToComments ts
triviaToComments (Tabs _ : ts) = triviaToComments ts
triviaToComments (Newlines _ n : ts) = let (ls, cs) = triviaToComments ts in (ls + n, cs)
triviaToComments (OtherWhitespace _ : ts) = triviaToComments ts
triviaToComments (Comment c : ts) = let (ls, cs) = triviaToComments ts in (0, (c, ls) : cs)

-- Does not print the token but does preserve the token’s trivia.
removeToken :: Token -> Document
removeToken (Token _ _ ts1 ts2) =
  let (_, cs) = triviaToComments (ts1 ++ ts2) in
    comments cs
  where
    comments [] = mempty
    comments ((LineComment c, ls) : cs) =
      forceBreak
        <> linePrefix (text "//" <> text (trimTrailingWhitespace c)
          <> (if ls > 1 && not (null cs) then hardline else mempty))
        <> comments cs
    comments ((BlockComment c _, ls) : cs) =
      forceBreak
        <> linePrefix (text "/*" <> text c <> text "*/"
          <> (if ls > 1 && not (null cs) then hardline else mempty))
        <> comments cs

-- Prints the trivia at the very end of a document.
endTrivia :: [Trivia] -> Document
endTrivia ts =
  let (ls, cs) = triviaToComments ts in
    (if ls > 0 && not (null cs) then hardline else mempty) <> comments cs
  where
    comments [] = mempty
    comments ((LineComment c, ls) : cs) =
      text "//" <> text (trimTrailingWhitespace c)
        <> (if ls > 1 && not (null cs) then hardline <> hardline else hardline)
        <> comments cs
    comments ((BlockComment c _, ls) : cs) =
      text "/*" <> text c <> text "*/"
        <> (if ls > 1 && not (null cs) then hardline <> hardline else hardline)
        <> comments cs

-- Trim whitespace from the end of some text.
trimTrailingWhitespace :: Text -> Text
trimTrailingWhitespace = Text.dropWhileEnd isSpace
