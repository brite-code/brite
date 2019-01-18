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
-- trailing comments before and after the comma.
printCommaList :: (a -> (Document, [AttachedComment])) -> [CommaListItem a] -> Document
printCommaList f = loopStart
  where
    loopStart (Left (UnattachedComment True c) : as) = loop (Left (UnattachedComment False c) : as)
    loopStart as = loop as

    loop [] = mempty
    loop (Left c : as) = printUnattachedComment c <> loop as
    loop [Right (a, cs2)] =
      let (b, cs1) = f a in
        b <>
        ifBreakElse
          (text "," <> printTrailingAttachedComments cs1 <> printTrailingAttachedComments cs2 <> hardline)
          (printTrailingAttachedComments cs1 <> printTrailingAttachedComments cs2)
    loop (Right (a, cs2) : as) =
      let (b, cs1) = f a in
        b <>
        ifBreakElse
          (text "," <> printTrailingAttachedComments cs1 <> printTrailingAttachedComments cs2 <> hardline)
          (printTrailingAttachedComments cs1 <> text "," <> printTrailingAttachedComments cs2 <> text " ") <>
        loop as

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
printStatement s0 = build $ case statementNode s0 of
  -- For concrete statements we failed to convert them to the printer AST. Presumably because they
  -- contained a parse error. Print out the raw source code for concrete statements.
  ConcreteStatement s -> rawText (tokensTrimmedSource (recoverStatementTokens s))

  -- Print an expression statement and include a semicolon for the appropriate expressions. If the
  -- expression has attached trailing comments then print those _after_ the semicolon.
  ExpressionStatement x' ->
    let (x, cs) = takeExpressionTrailingComments x' in
      printExpression Top x
        <> (if withoutSemicolon (expressionNode x) then mempty else text ";")
        <> printTrailingAttachedComments cs
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

  -- Print expressions that should break on a new line and comments specifically written above
  -- the value.
  --
  -- NOTE: If the expression has trailing comments then print those _after_ the semicolon.
  BindingStatement p Nothing cs1 x1 | not (null cs1) || shouldBreakOntoNextLine x1 ->
    let (x, cs3) = takeExpressionTrailingComments x1 in
      -- NOTE: Adding another `group` here may break our carefully crafted
      -- `ifBreak`/`ifFlat` conditions.
      group $ text "let "
        <> printPattern p
        <> text " ="
        <> line
        <> indent
            (mconcat (map printUnattachedComment cs2)
              <> printExpression Top x)
        <> text ";"
        <> printTrailingAttachedComments cs3
      where
        cs2 = case cs1 of
          UnattachedComment True c : cs -> UnattachedComment False c : cs
          _ -> cs1

  -- IMPORTANT: It is ok to ignore the comments here because the above branch will match if we have
  -- some comments.
  --
  -- NOTE: If the expression has trailing comments then print those _after_ the semicolon.
  BindingStatement p Nothing _ x1 ->
    let (x, cs) = takeExpressionTrailingComments x1 in
      text "let "
        <> printPattern p
        <> text " = "
        <> printExpression Top x
        <> text ";"
        <> printTrailingAttachedComments cs

  -- NOTE: If the expression has trailing comments then print those _after_ the semicolon.
  ReturnStatement (Just (cs1, x1)) | not (null cs1) || shouldBreakOntoNextLine x1 ->
    let (x, cs3) = takeExpressionTrailingComments x1 in
      -- NOTE: Adding another `group` here may break our carefully crafted
      -- `ifBreak`/`ifFlat` conditions.
      group $ text "return "
        <> ifBreak (text "(")
        <> softline
        <> indent
            (mconcat (map printUnattachedComment cs2)
              <> printExpression Top x
              <> ifBreak (printTrailingAttachedComments cs3))
        <> softline
        <> ifBreak (text ")")
        <> text ";"
        <> ifFlat (printTrailingAttachedComments cs3)
    where
      cs2 = case cs1 of
        UnattachedComment True c : cs -> UnattachedComment False c : cs
        _ -> cs1

  -- IMPORTANT: It is ok to ignore the comments here because the above branch will match if we have
  -- some comments.
  --
  -- NOTE: If the expression has trailing comments then print those _after_ the semicolon.
  ReturnStatement (Just (_, x1)) ->
    let (x, cs) = takeExpressionTrailingComments x1 in
      text "return " <> printExpression Top x <> text ";" <> printTrailingAttachedComments cs

  -- NOTE: If the expression has trailing comments then print those _after_ the semicolon.
  BreakStatement (Just (cs1, x1)) | not (null cs1) || shouldBreakOntoNextLine x1 ->
    let (x, cs) = takeExpressionTrailingComments x1 in
      -- NOTE: Adding another `group` here may break our carefully crafted
      -- `ifBreak`/`ifFlat` conditions.
      group $ text "break "
        <> ifBreak (text "(")
        <> softline
        <> indent
            (mconcat (map printUnattachedComment cs2)
              <> printExpression Top x
              <> ifBreak (printTrailingAttachedComments cs))
        <> softline
        <> ifBreak (text ")")
        <> text ";"
        <> ifFlat (printTrailingAttachedComments cs)
    where
      cs2 = case cs1 of
        UnattachedComment True c : cs -> UnattachedComment False c : cs
        _ -> cs1

  -- IMPORTANT: It is ok to ignore the comments here because the above branch will match if we have
  -- some comments.
  --
  -- NOTE: If the expression has trailing comments then print those _after_ the semicolon.
  BreakStatement (Just (_, x1)) ->
    let (x, cs) = takeExpressionTrailingComments x1 in
      text "break " <> printExpression Top x <> text ";" <> printTrailingAttachedComments cs

  ReturnStatement Nothing ->
    text "return;"

  BreakStatement Nothing ->
    text "break;"

  where
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
      printSingleArg (Right (a, cs) : as) =
        printExpression Top a <> printTrailingAttachedComments cs <> softline <> printSingleArg as

  ObjectExpression ps Nothing ->
    group (text "{" <> indent (softline <> printCommaList printProperty ps) <> text "}")
    where
      -- The trailing comments of a punned object expression are printed by `printCommaList`.
      printProperty (ObjectExpressionPropertyPun cs1 cs2 n) =
        (printLeadingAttachedComments cs1 <> text (identifierText n), cs2)

      printProperty (ObjectExpressionProperty cs1 n cs2' x') =
        let
          -- The trailing comments of our expression are printed by `printCommaList` after
          -- the comma.
          (x, cs3) = takeExpressionTrailingComments x'
          -- Remove the leading empty line from our list of unattached comments.
          cs2 = case cs2' of
            UnattachedComment True c : cs -> UnattachedComment False c : cs
            cs -> cs
        in
          ( group $
              printLeadingAttachedComments cs1 <>
              text (identifierText n) <>
              text ":" <>
              -- If either we have some unattached comments or the expression should be broken onto
              -- a new line when printing then make sure when this property breaks we put the
              -- expression on a new line and we add indentation.
              (if not (null cs2) || shouldBreakOntoNextLine x then
                line <>
                indent (mconcat (map printUnattachedComment cs2) <> printExpression Top x)
              else
                text " " <> printExpression Top x)
          , cs3
          )

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

  ConditionalExpression c0 ->
    group (consequent c0)
    where
      consequent (ConditionalExpressionIf cs1 x b a) =
        text "if " <>
        (if not (null cs1) || shouldBreakOntoNextLine x then group $
          let
            cs2 = case cs1 of
              UnattachedComment True c : cs -> UnattachedComment False c : cs
              _ -> cs1
          in
            ifBreak (text "(") <>
            softline <>
            indent (mconcat (map printUnattachedComment cs2) <> printExpression Top x) <>
            softline <>
            ifBreak (text ")")
        else
          printExpression Top x) <>
        text " " <>
        printUngroupedBlock b <>
        maybe mempty alternate a

      alternate (ConditionalExpressionElse [] b) = text " else " <> printUngroupedBlock b
      alternate (ConditionalExpressionElse cs b) =
        hardline <> mconcat (map printUnattachedComment cs) <> text "else " <> printUngroupedBlock b

      alternate (ConditionalExpressionElseIf [] c) = text " else " <> consequent c
      alternate (ConditionalExpressionElseIf cs c) =
        hardline <> mconcat (map printUnattachedComment cs) <> text "else " <> consequent c

  BlockExpression b -> text "do " <> printBlock b
  LoopExpression b -> text "loop " <> printBlock b

  where
    -- Take the leading and trailing comments for our expression.
    (attachedLeadingComments, (x0, attachedTrailingComments)) =
      takeExpressionTrailingComments <$> takeExpressionLeadingComments x0'

    -- Finishes printing an expression node by printing leading/trailing attached comments and
    -- parentheses in case we need them.
    build x1 =
      (if wrap then text "(" else mempty)
        <> printLeadingAttachedComments attachedLeadingComments
        <> x1
        <> printTrailingAttachedComments attachedTrailingComments
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

-- Prints a pattern.
printPattern :: Pattern -> Document
printPattern x0 = build $ case patternNode x0 of
  ConstantPattern (BooleanConstant True) -> text "true"
  ConstantPattern (BooleanConstant False) -> text "false"

  VariablePattern n -> text (identifierText n)

  where
    -- Finishes printing an expression node by printing leading/trailing attached comments and
    -- parentheses in case we need them.
    build x1 =
      printLeadingAttachedComments (patternLeadingComments x0)
        <> x1
        <> printTrailingAttachedComments (patternTrailingComments x0)

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

-- Removes the expression trailing comments from our `Expression` and returns them. If our
-- expression ends in another expression (like unary expressions: `-E`) then we take the trailing
-- comments from that as well.
takeExpressionTrailingComments :: Expression -> (Expression, [AttachedComment])
takeExpressionTrailingComments x0 =
  case expressionNode x0 of
    ConstantExpression _ -> noTrailingExpression
    VariableExpression _ -> noTrailingExpression
    FunctionExpression _ -> noTrailingExpression
    CallExpression _ _ -> noTrailingExpression
    ObjectExpression _ _ -> noTrailingExpression
    PropertyExpression _ _ _ -> noTrailingExpression
    UnaryExpression op x1 -> trailingExpression (UnaryExpression op) x1
    BinaryExpression x1 op cs x2 -> trailingExpression (BinaryExpression x1 op cs) x2
    ConditionalExpression _ -> noTrailingExpression
    BlockExpression _ -> noTrailingExpression
    LoopExpression _ -> noTrailingExpression
    WrappedExpression _ _ -> noTrailingExpression
  where
    noTrailingExpression =
      if null (expressionTrailingComments x0) then (x0, [])
      else (x0 { expressionTrailingComments = [] }, expressionTrailingComments x0)

    trailingExpression f x1 =
      case takeExpressionTrailingComments x1 of
        (_, []) -> noTrailingExpression
        (x2, cs) ->
          if null (expressionTrailingComments x0) then (x0 { expressionNode = f x2 }, cs)
          else
            ( x0 { expressionTrailingComments = [], expressionNode = f x2 }
            , cs ++ expressionTrailingComments x0
            )

-- Removes the expression leading comments from our `Expression` and returns them. If our
-- expression begins in another expression (like property expressions: `E.p`) then we take the
-- leading comments from that as well.
takeExpressionLeadingComments :: Expression -> ([AttachedComment], Expression)
takeExpressionLeadingComments x0 =
  case expressionNode x0 of
    -- For call expressions and property expressions also move any trailing trivia to be
    -- leading trivia.
    CallExpression x1 xs -> leadingExpressionTakeTrailingToo x1 (\x -> CallExpression x xs)
    PropertyExpression x1 cs p -> leadingExpressionTakeTrailingToo x1 (\x -> PropertyExpression x cs p)
    -- While technically unary operations don’t have a leading expression we treat them as if they
    -- do for aesthetics.
    UnaryExpression op x1 -> leadingExpression x1 (UnaryExpression op)
    BinaryExpression x1 op cs x2 -> leadingExpression x1 (\x -> BinaryExpression x op cs x2)

    ConstantExpression _ -> noLeadingExpression
    VariableExpression _ -> noLeadingExpression
    FunctionExpression _ -> noLeadingExpression
    ObjectExpression _ _ -> noLeadingExpression
    ConditionalExpression _ -> noLeadingExpression
    BlockExpression _ -> noLeadingExpression
    LoopExpression _ -> noLeadingExpression
    WrappedExpression _ _ -> noLeadingExpression
  where
    noLeadingExpression =
      if null (expressionLeadingComments x0) then ([], x0)
      else (expressionLeadingComments x0, x0 { expressionLeadingComments = [] })

    leadingExpression x1 f =
      case takeExpressionLeadingComments x1 of
        ([], _) -> noLeadingExpression
        (cs, x2) ->
          ( expressionLeadingComments x0 ++ cs
          , x0 { expressionLeadingComments = [], expressionNode = f x2 }
          )

    leadingExpressionTakeTrailingToo x1 f =
      case takeExpressionLeadingComments x1 of
        ([], _) ->
          case takeExpressionTrailingComments x1 of
            (_, []) -> noLeadingExpression
            (x3, cs3) ->
              ( expressionLeadingComments x0 ++ cs3
              , x0 { expressionLeadingComments = [], expressionNode = f x3 }
              )
        (cs2, x2) ->
          case takeExpressionTrailingComments x2 of
            (_, []) ->
              ( expressionLeadingComments x0 ++ cs2
              , x0 { expressionLeadingComments = [], expressionNode = f x2 }
              )
            (x3, cs3) ->
              ( expressionLeadingComments x0 ++ cs2 ++ cs3
              , x0 { expressionLeadingComments = [], expressionNode = f x3 }
              )
