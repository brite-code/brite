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

import Brite.Syntax.CST (recoverStatementTokens)
import Brite.Syntax.PrinterAST
import Brite.Syntax.PrinterFramework
import Brite.Syntax.Tokens
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

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
printUnattachedComment c0 =
  (if unattachedCommentLeadingEmptyLine c0 then hardline else mempty) <>
    (case unattachedComment c0 of
      LineComment c -> text "//" <> text (Text.dropWhileEnd isSpace c)
      BlockComment c _ ->
        text "/*"
          <> text (Text.Lazy.toStrict (Text.Builder.toLazyText (removeTrailingSpaces c)))
          <> text "*/")
    <> hardline

-- Prints a list of leading attached comments. Attached line comments will always go to the end of
-- the line.
printLeadingAttachedComments :: [AttachedComment] -> Document
printLeadingAttachedComments cs = mconcat $ flip map cs $ \c0 ->
  case attachedComment c0 of
    LineComment c -> lineSuffix (text " //" <> text (Text.dropWhileEnd isSpace c))
    BlockComment c _ ->
      text "/*"
        <> text (Text.Lazy.toStrict (Text.Builder.toLazyText (removeTrailingSpaces c)))
        <> text "*/ "

-- Prints a list of trailing attached comments. Attached line comments will always go to the
-- end of the line.
printTrailingAttachedComments :: [AttachedComment] -> Document
printTrailingAttachedComments cs = mconcat $ flip map cs $ \c0 ->
  case attachedComment c0 of
    LineComment c -> lineSuffix (text " //" <> text (Text.dropWhileEnd isSpace c))
    BlockComment c _ ->
      text " /*"
        <> text (Text.Lazy.toStrict (Text.Builder.toLazyText (removeTrailingSpaces c)))
        <> text "*/"

-- Prints a list separated by commas. If the list is broken onto multiple lines we will always add a
-- trailing comma. The function for printing an individual comma list item returns a list of
-- trailing comments. Our comma list printer will print these _after_ the comma.
printCommaList :: (a -> (Document, [AttachedComment])) -> [MaybeComment a] -> Document
printCommaList f = loopStart
  where
    loopStart (Left (UnattachedComment True c) : as) = loop (Left (UnattachedComment False c) : as)
    loopStart as = loop as

    loop [] = mempty
    loop (Left c : as) = printUnattachedComment c <> loop as
    loop [Right a] =
      let (b, cs) = f a in
        b <> ifBreak (text ",") <> printTrailingAttachedComments cs <> softline
    loop (Right a : as) =
      let (b, cs) = f a in
        b <> text "," <> printTrailingAttachedComments cs <> line <> loop as

-- Prints a sequence of statements or comments.
printStatementSequence :: [MaybeComment Statement] -> Document
printStatementSequence ss0 = loopStart ss0
  where
    loopStart [] = mempty
    loopStart (Left (UnattachedComment True c) : ss) = loop (Left (UnattachedComment False c) : ss)
    loopStart (Right (Statement True cs1 cs2 s) : ss) = loop (Right (Statement False cs1 cs2 s) : ss)
    loopStart ss = loop ss

    loop [] = mempty
    loop (Left c : ss) = printUnattachedComment c <> loop ss
    loop (Right s : ss) = printStatement s <> loop ss

-- Prints a single statement.
printStatement :: Statement -> Document
printStatement s0' = build $ case statementNode s0 of
  -- For concrete statements we failed to convert them to the printer AST. Presumably because they
  -- contained a parse error. Print out the raw source code for concrete statements.
  ConcreteStatement s -> rawText (tokensTrimmedSource (recoverStatementTokens s))

  -- Print an expression statement and include a semicolon for the appropriate expressions. If the
  -- expression has attached trailing comments then print those _after_ the semicolon. We do this by
  -- moving around comments in `fixStatementComments`.
  ExpressionStatement x ->
    printExpression Top x
      <> (if withoutSemicolon (expressionNode x) then mempty else text ";")
    where
      withoutSemicolon (ConstantExpression _) = False
      withoutSemicolon (VariableExpression _) = False
      withoutSemicolon (FunctionExpression _) = False
      withoutSemicolon (CallExpression _ _) = False
      withoutSemicolon (ObjectExpression _ _) = False
      withoutSemicolon (PropertyExpression _ _ _) = False
      withoutSemicolon (UnaryExpression _ _) = False
      withoutSemicolon (BinaryExpression _ _ _ _) = False
      withoutSemicolon (ConditionalExpression _) = True
      withoutSemicolon (BlockExpression _) = True
      withoutSemicolon (LoopExpression _) = True
      withoutSemicolon (WrappedExpression _ _) = False

  ReturnStatement (Just (cs1, x)) | not (null cs1) || shouldBreakOntoNextLine x -> group $
    text "return "
      <> ifBreak (text "(")
      <> softline
      <> indent (mconcat (map printUnattachedComment cs2) <> printExpression Top x)
      <> softline
      <> ifBreak (text ")")
      <> text ";"
    where
      cs2 = case cs1 of
        UnattachedComment True c : cs -> UnattachedComment False c : cs
        _ -> cs1

  -- NOTE: It is ok to ignore the comments here because the above branch will match if we have
  -- some comments.
  ReturnStatement (Just (_, x)) ->
    text "return " <> printExpression Top x <> text ";"

  BreakStatement (Just (cs1, x)) | not (null cs1) || shouldBreakOntoNextLine x ->
    text "break "
      <> ifBreak (text "(")
      <> softline
      <> indent (mconcat (map printUnattachedComment cs2) <> printExpression Top x)
      <> softline
      <> ifBreak (text ")")
      <> text ";"
    where
      cs2 = case cs1 of
        UnattachedComment True c : cs -> UnattachedComment False c : cs
        _ -> cs1

  -- NOTE: It is ok to ignore the comments here because the above branch will match if we have
  -- some comments.
  BreakStatement (Just (_, x)) ->
    text "break " <> printExpression Top x <> text ";"

  ReturnStatement Nothing ->
    text "return;"

  BreakStatement Nothing ->
    text "break;"

  where
    s0 = fromMaybe s0' (fixStatementComments s0')

    build s1 =
      (if statementLeadingEmptyLine s0 then hardline else mempty)
        <> printLeadingAttachedComments (statementLeadingComments s0)
        <> s1
        <> printTrailingAttachedComments (statementTrailingComments s0)
        <> (case statementNode s0 of { ConcreteStatement _ -> mempty; _ -> hardline })

-- Prints a block, but the block is not wrapped in a group. That means it will only be flattened if
-- a parent group is wrapped in a block.
--
-- If the block is only a single expression statement (without comments) then we attempt to print
-- that expression statement on a single line.
printUngroupedBlock :: Block -> Document
printUngroupedBlock (Block []) = text "{}"
printUngroupedBlock (Block [Right s@(Statement { statementNode = ExpressionStatement x })]) =
  text "{"
    <> indent
      (line
        <> printLeadingAttachedComments (statementLeadingComments s)
        <> printExpression Top x
        <> printTrailingAttachedComments (statementTrailingComments s)
        <> line)
    <> text "}"
printUngroupedBlock (Block ss) =
  -- Statements in a statement sequence will always end with a new line.
  text "{" <> indent (line <> printStatementSequence ss) <> text "}"

-- Prints a block.
printBlock :: Block -> Document
printBlock = group . printUngroupedBlock

-- Prints an expression.
printExpression :: Precedence -> Expression -> Document
printExpression p0 x0' = build $ case expressionNode x0 of
  ConstantExpression (BooleanConstant True) -> text "true"
  ConstantExpression (BooleanConstant False) -> text "false"

  VariableExpression n -> text (identifierText n)

  -- Call expressions with a single argument never add a trailing comma. This was a pet-peeve of
  -- mine (Caleb) in the JavaScript pretty printing library [Prettier][1]. One of the primary
  -- reasons for trailing commas is to improve differences in the programmer’s source control
  -- manager (like git). Adding a new line to a trailing comma list only changes one line. It does
  -- not also change the line above it by adding a comma. It is also easy to copy/paste a new item
  -- in a trailing comma list since you don’t need to worry about adding a new comma.
  --
  -- However, functions usually don’t have a variable number of arguments. Most of the time the
  -- number of function arguments never changes so the convenience of a trailing comma list is not
  -- relevant. Trailing commas in function calls with a single item do actively look worse (in my
  -- opinion), though. Especially in JavaScript when you’d have an arrow function
  -- (`(x, y) => x + y`) and you’d have to put a trailing comma after it.
  --
  -- This is my pretty printing framework now, so I get to call the shots.
  --
  -- [1]: https://prettier.io
  CallExpression x1 xs ->
    if exactly (1 :: Int) xs then
      printExpression Primary x1
        <> group (text "("
            <> indent (softline <> printSingleArgStart xs)
            <> text ")")
    else
      printExpression Primary x1
        <> group (text "("
            <> indent (softline <> printCommaList printArg xs)
            <> text ")")
    where
      -- Prints an expression while also removing trailing comments to let our comma list handle the
      -- trailing comments.
      printArg x' =
        let (x, cs) = takeExpressionTrailingComments x' in
          (printExpression Top x, cs)

      -- Returns true if we have exactly `n` arguments.
      exactly n [] = n == 0
      exactly n (Left _ : args) = exactly n args
      exactly n (Right _ : _) | n == 0 = False
      exactly n (Right _ : args) = exactly (n - 1) args

      -- Print an argument list when we only have a single argument. Single argument lists never
      -- print trailing comments. See our justification above.
      printSingleArgStart (Left (UnattachedComment True c) : as) =
        printSingleArg (Left (UnattachedComment False c) : as)
      printSingleArgStart as = printSingleArg as

      -- Print our single argument without a trailing comment!
      printSingleArg [] = mempty
      printSingleArg (Left c : as) = printUnattachedComment c <> printSingleArg as
      printSingleArg (Right a : as) = printExpression Top a <> softline <> printSingleArg as

  -- Print a property statement which may have some unattached comments over the property.
  PropertyExpression e cs n ->
    printExpression Primary e <> group (indent
      (softline
        <> mconcat (map printUnattachedComment cs)
        <> text "." <> text (identifierText n)))

  UnaryExpression op' x -> op <> printExpression Unary x
    where
      op = case op' of
        Not -> text "!"
        Positive -> text "+"
        Negative -> text "-"

  BinaryExpression l op' cs r ->
    -- Group the binary expression if we were printed at a different precedence level than our own.
    -- This means operators of the same precedence will be put together in one group.
    (if p0 /= p1 then group else id)
      (printExpression p1 l <> text " " <> text op <>
        -- If we are wrapping this expression then when there is a new line we want to indent by a
        -- single space. That lines up our first line (which comes after a `(`) and future lines.
        (if wrap then indent1 else id)
          (line
            <> mconcat (map printUnattachedComment cs)
            <> printExpression p1 r))
    where
      op = case op' of
        Add -> "+"
        Subtract -> "-"
        Multiply -> "*"
        Divide -> "/"
        Remainder -> "%"
        Exponent -> "^"
        Equals -> "=="
        NotEquals -> "!="
        LessThan -> "<"
        LessThanOrEqual -> "<="
        GreaterThan -> ">"
        GreaterThanOrEqual -> ">="
        And -> "&&"
        Or -> "||"

  BlockExpression b -> text "do " <> printBlock b
  LoopExpression b -> text "loop " <> printBlock b

  where
    -- Attempt to fix up comments in our expression argument.
    x0 = fromMaybe x0' (fixExpressionComments x0')

    -- Finishes printing an expression node by printing leading/trailing attached comments and
    -- parentheses in case we need them.
    build x1 =
      (if wrap then text "(" else mempty)
        <> printLeadingAttachedComments (expressionLeadingComments x0)
        <> x1
        <> printTrailingAttachedComments (expressionTrailingComments x0)
        <> (if wrap then text ")" else mempty)

    -- Whether or not we should wrap this expression based on its precedence level.
    wrap = p0 < p1

    -- Get the actual precedence of our expression. Not the expected precedence our function
    -- was provided.
    p1 = case expressionNode x0 of
      ConstantExpression _ -> Primary
      VariableExpression _ -> Primary
      FunctionExpression _ -> Primary
      CallExpression _ _ -> Primary
      ObjectExpression _ _ -> Primary
      PropertyExpression _ _ _ -> Primary
      UnaryExpression _ _ -> Unary
      BinaryExpression _ Add _ _ -> Additive
      BinaryExpression _ Subtract _ _ -> Additive
      BinaryExpression _ Multiply _ _ -> Multiplicative
      BinaryExpression _ Divide _ _ -> Multiplicative
      BinaryExpression _ Remainder _ _ -> Multiplicative
      BinaryExpression _ Exponent _ _ -> Exponentiation
      BinaryExpression _ Equals _ _ -> Equality
      BinaryExpression _ NotEquals _ _ -> Equality
      BinaryExpression _ LessThan _ _ -> Relational
      BinaryExpression _ LessThanOrEqual _ _ -> Relational
      BinaryExpression _ GreaterThan _ _ -> Relational
      BinaryExpression _ GreaterThanOrEqual _ _ -> Relational
      BinaryExpression _ And _ _ -> LogicalAnd
      BinaryExpression _ Or _ _ -> LogicalOr
      ConditionalExpression _ -> Primary
      BlockExpression _ -> Primary
      LoopExpression _ -> Primary
      WrappedExpression _ _ -> Primary

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
  -- The highest level of precedence. Includes every expression. Terminology taken from set theory
  -- “top” and “bottom”.
  | Top
  deriving (Eq, Ord)

-- Expressions that, when they break, should break onto the next line. The following is an example
-- of an expression which should break onto a new line:
--
-- ```ite
-- let x =
--   a +
--   // Hello, world!
--   b;
-- ```
--
-- The following is an example of an expression which should not break onto a new line:
--
-- ```ite
-- let x = f(
--   // Hello, world!
-- );
-- ```
--
-- The general rule of thumb is that if you want to keep the expression aligned vertically when it
-- breaks you should put it on the next line.
shouldBreakOntoNextLine :: Expression -> Bool
shouldBreakOntoNextLine x = case expressionNode x of
  ConstantExpression _ -> False
  VariableExpression _ -> False
  FunctionExpression _ -> False
  CallExpression _ _ -> False
  ObjectExpression _ _ -> False
  PropertyExpression _ _ _ -> False
  UnaryExpression _ _ -> False
  BinaryExpression _ _ _ _ -> True
  ConditionalExpression _ -> False
  BlockExpression _ -> False
  LoopExpression _ -> False
  WrappedExpression _ _ -> False

-- Fixes the node leading and trailing comments are attached to. Returns `Nothing` if there was
-- nothing to fix.
fixStatementComments :: Statement -> Maybe Statement
fixStatementComments s0 = case statementNode s0 of
  ExpressionStatement x0 ->
    let (b1, x1) = maybe (False, x0) ((,) True) (fixExpressionComments x0) in
      if null (expressionTrailingComments x1) then
        if b1 then Just (s0 { statementNode = ExpressionStatement x1 }) else Nothing
      else Just $ s0
        { statementTrailingComments = expressionTrailingComments x1 ++ statementTrailingComments s0
        , statementNode = ExpressionStatement (x1 { expressionTrailingComments = [] })
        }

  BindingStatement p t x0 ->
    let (b1, x1) = maybe (False, x0) ((,) True) (fixExpressionComments x0) in
      if null (expressionTrailingComments x1) then
        if b1 then Just (s0 { statementNode = BindingStatement p t x1 }) else Nothing
      else Just $ s0
        { statementTrailingComments = expressionTrailingComments x1 ++ statementTrailingComments s0
        , statementNode = BindingStatement p t (x1 { expressionTrailingComments = [] })
        }

  ReturnStatement (Just (cs, x0)) ->
    let (b1, x1) = maybe (False, x0) ((,) True) (fixExpressionComments x0) in
      if null (expressionTrailingComments x1) then
        if b1 then Just (s0 { statementNode = ReturnStatement (Just (cs, x1)) }) else Nothing
      else Just $ s0
        { statementTrailingComments = expressionTrailingComments x1 ++ statementTrailingComments s0
        , statementNode = ReturnStatement (Just (cs, x1 { expressionTrailingComments = [] }))
        }

  BreakStatement (Just (cs, x0)) ->
    let (b1, x1) = maybe (False, x0) ((,) True) (fixExpressionComments x0) in
      if null (expressionTrailingComments x1) then
        if b1 then Just (s0 { statementNode = BreakStatement (Just (cs, x1)) }) else Nothing
      else Just $ s0
        { statementTrailingComments = expressionTrailingComments x1 ++ statementTrailingComments s0
        , statementNode = BreakStatement (Just (cs, x1 { expressionTrailingComments = [] }))
        }

  ReturnStatement Nothing -> Nothing
  BreakStatement Nothing -> Nothing
  FunctionDeclaration _ _ -> Nothing
  ConcreteStatement _ -> Nothing

-- Fixes the node leading and trailing comments are attached to. Returns `Nothing` if there was
-- nothing to fix.
fixExpressionComments :: Expression -> Maybe Expression
fixExpressionComments x0 = case expressionNode x0 of
  CallExpression a0 xs ->
    let (c1, a1) = maybe (False, a0) ((,) True) (fixExpressionComments a0) in
      if null (expressionLeadingComments a1) then
        if c1 then Just (x0 { expressionNode = CallExpression a1 xs }) else Nothing
      else Just $ x0
        { expressionLeadingComments = expressionLeadingComments x0 ++ expressionLeadingComments a1
        , expressionNode = CallExpression (a1 { expressionLeadingComments = [] }) xs
        }

  PropertyExpression a0 cs n ->
    let (c1, a1) = maybe (False, a0) ((,) True) (fixExpressionComments a0) in
      if null (expressionLeadingComments a1) then
        if c1 then Just (x0 { expressionNode = PropertyExpression a1 cs n }) else Nothing
      else Just $ x0
        { expressionLeadingComments = expressionLeadingComments x0 ++ expressionLeadingComments a1
        , expressionNode = PropertyExpression (a1 { expressionLeadingComments = [] }) cs n
        }

  UnaryExpression op a0 ->
    let (c1, a1) = maybe (False, a0) ((,) True) (fixExpressionComments a0) in
      if null (expressionLeadingComments a1) && null (expressionTrailingComments a1) then
        if c1 then Just (x0 { expressionNode = UnaryExpression op a1 }) else Nothing
      else Just $ x0
        { expressionLeadingComments = expressionLeadingComments x0 ++ expressionLeadingComments a1
        , expressionTrailingComments = expressionTrailingComments a1 ++ expressionTrailingComments x0
        , expressionNode = UnaryExpression op
            (a1 { expressionLeadingComments = [], expressionTrailingComments = [] })
        }

  BinaryExpression a0 op cs b0 ->
    let
      (c1, a1) = maybe (False, a0) ((,) True) (fixExpressionComments a0)
      (c2, b1) = maybe (False, b0) ((,) True) (fixExpressionComments b0)
    in
      if null (expressionLeadingComments a1) && null (expressionTrailingComments b1) then
        if c1 || c2 then (Just (x0 { expressionNode = BinaryExpression a1 op cs b1 })) else Nothing
      else Just $ x0
        { expressionLeadingComments = expressionLeadingComments x0 ++ expressionLeadingComments a1
        , expressionTrailingComments = expressionTrailingComments b1 ++ expressionTrailingComments x0
        , expressionNode = BinaryExpression
            (a1 { expressionLeadingComments = [] }) op cs (b1 { expressionTrailingComments = [] })
        }

  ConstantExpression _ -> Nothing
  VariableExpression _ -> Nothing
  FunctionExpression _ -> Nothing
  ObjectExpression _ _ -> Nothing
  ConditionalExpression _ -> Nothing
  BlockExpression _ -> Nothing
  LoopExpression _ -> Nothing
  WrappedExpression _ _ -> Nothing
