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
    printExpression x
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
        <> printExpression x
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
printExpression :: Expression -> Document
printExpression x0' = build $ case expressionNode x0 of
  ConstantExpression (BooleanConstant True) -> text "true"
  ConstantExpression (BooleanConstant False) -> text "false"

  VariableExpression n -> text (identifierText n)

  UnaryExpression op' x -> op <> printExpression x
    where
      op = case op' of
        Not -> text "!"
        Positive -> text "+"
        Negative -> text "-"

  BlockExpression b -> text "do " <> printBlock b
  LoopExpression b -> text "loop " <> printBlock b

  where
    x0 = fromMaybe x0' (fixExpressionComments x0')

    build x1 =
        printLeadingAttachedComments (expressionLeadingComments x0)
        <> x1
        <> printTrailingAttachedComments (expressionTrailingComments x0)

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

  ReturnStatement (Just x0) ->
    let (b1, x1) = maybe (False, x0) ((,) True) (fixExpressionComments x0) in
      if null (expressionTrailingComments x1) then
        if b1 then Just (s0 { statementNode = ReturnStatement (Just x1) }) else Nothing
      else Just $ s0
        { statementTrailingComments = expressionTrailingComments x1 ++ statementTrailingComments s0
        , statementNode = ReturnStatement (Just (x1 { expressionTrailingComments = [] }))
        }

  BreakStatement (Just x0) ->
    let (b1, x1) = maybe (False, x0) ((,) True) (fixExpressionComments x0) in
      if null (expressionTrailingComments x1) then
        if b1 then Just (s0 { statementNode = BreakStatement (Just x1) }) else Nothing
      else Just $ s0
        { statementTrailingComments = expressionTrailingComments x1 ++ statementTrailingComments s0
        , statementNode = BreakStatement (Just (x1 { expressionTrailingComments = [] }))
        }

  ReturnStatement Nothing -> Nothing
  BreakStatement Nothing -> Nothing
  FunctionDeclaration _ _ -> Nothing
  ConcreteStatement _ -> Nothing

-- Fixes the node leading and trailing comments are attached to. Returns `Nothing` if there was
-- nothing to fix.
fixExpressionComments :: Expression -> Maybe Expression
fixExpressionComments x0 = case expressionNode x0 of
  CallExpression _ _ -> error "TODO: Leading comments"

  PropertyExpression _ _ _ -> error "TODO: Leading comments"

  UnaryExpression op a0 ->
    let (b1, a1) = maybe (False, a0) ((,) True) (fixExpressionComments a0) in
      if null (expressionLeadingComments a1) && null (expressionTrailingComments a1) then
        if b1 then Just (x0 { expressionNode = UnaryExpression op a1 }) else Nothing
      else Just $ x0
        { expressionLeadingComments = expressionLeadingComments x0 ++ expressionLeadingComments a1
        , expressionTrailingComments = expressionTrailingComments a1 ++ expressionTrailingComments x0
        , expressionNode = UnaryExpression op
            (a1 { expressionLeadingComments = [], expressionTrailingComments = [] })
        }

  BinaryExpression _ _ _ _ -> error "TODO: Leading and trailing comments"

  ConstantExpression _ -> Nothing
  VariableExpression _ -> Nothing
  FunctionExpression _ -> Nothing
  ObjectExpression _ _ -> Nothing
  ConditionalExpression _ -> Nothing
  BlockExpression _ -> Nothing
  LoopExpression _ -> Nothing
  WrappedExpression _ _ -> Nothing
