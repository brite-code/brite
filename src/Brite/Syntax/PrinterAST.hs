-- An Abstract Syntax Tree (AST) designed for consumption by the printer. A compiler programmer may
-- either manually construct printer AST nodes (since they do not include `Range`s) or you may
-- convert the CST into a printer AST.
--
-- After construction the printer AST is then consumed by the printer to pretty print a
-- Brite program.
--
-- The printer AST is different from the semantic AST in `Brite.Semantics.AST` in that the semantic
-- AST is designed for type checking and eventually compilation. The semantic AST contains `Range`s
-- which identify where in source code semantic AST nodes came from. This means semantic AST nodes
-- can only be constructed by the parser. They cannot be constructed manually be the programmer.
-- We also don’t include all of the comments in the semantic AST. Only the comments relevant
-- for documentation.
--
-- The printer AST, on the other hand, does not carry information about where nodes were defined in
-- source code and is designed to carries all comments.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brite.Syntax.PrinterAST
  ( Module(..)
  , UnattachedComment(..)
  , AttachedComment(..)
  , MaybeComment
  , Statement(..)
  , StatementNode(..)
  , Function(..)
  , FunctionParameter(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , takeExpressionTrailingComments
  , ExpressionNode(..)
  , ObjectExpressionProperty(..)
  , UnaryOperator(..)
  , BinaryOperator(..)
  , ConditionalExpressionIf(..)
  , ConditionalExpressionElse(..)
  , Pattern(..)
  , PatternNode(..)
  , ObjectPatternProperty(..)
  , Type(..)
  , TypeNode(..)
  , ObjectTypeProperty(..)
  , Quantifier(..)
  , QuantifierBoundKind(..)
  , convertModule
  ) where

import Brite.Syntax.CST (Recover(..), UnaryOperator(..), BinaryOperator(..), QuantifierBoundKind(..))
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Tokens
import Data.Foldable (foldrM, mapM_)
import Data.Maybe (maybeToList)

-- A Brite printer AST module.
newtype Module = Module
  { moduleStatements :: [MaybeComment Statement]
  }

-- A comment which is unattached to any source code. For example:
--
-- ```ite
-- // Hello, world!
-- let x = y;
-- ```
--
-- We consider the “Hello, world!” comment here to be unattached as it is on its own line. Separate
-- from any source code.
data UnattachedComment = UnattachedComment
  -- Does an empty line come before this unattached comment? We can easily tell when there are
  -- leading empty lines in the CST by looking at trivia. It is harder to tell if there are trailing
  -- empty lines.
  { unattachedCommentLeadingEmptyLine :: Bool
  -- The comment data for this unattached comment.
  , unattachedComment :: Comment
  }

-- A comment which is attached to some source code. For example:
--
-- ```ite
-- /* Hello, world! */ let x = y;
-- ```
--
-- We consider the “Hello, world!” comment here to be attached to the `let x = y;` statement as it
-- is on the same line as this statement.
--
-- Line comments can only be attached to source code at the very end of a line. After all, a line
-- comment must end in a new line, so in order for a line comment to be on the same line as some
-- code the line comment must be at the end of that line.
--
-- ```ite
-- let x = y; // Hello, world!
-- ```
newtype AttachedComment = AttachedComment
  -- The comment data for this attached comment.
  { attachedComment :: Comment
  }

-- Either an unattached comment or some other AST node.
type MaybeComment a = Either UnattachedComment a

data Statement = Statement
  -- Does an empty line come before this statement? We can easily tell when there are leading empty
  -- lines in the CST by looking at trivia. It is harder to tell if there are trailing empty lines.
  { statementLeadingEmptyLine :: Bool
  -- The leading comments that are attached to this statement.
  , statementLeadingComments :: [AttachedComment]
  -- The trailing comments that are attached to this statement.
  , statementTrailingComments :: [AttachedComment]
  -- The representation for this statement.
  , statementNode :: StatementNode
  }

data StatementNode
  -- `E;`
  = ExpressionStatement Expression

  -- `let P = E;`
  | BindingStatement Pattern (Maybe Type) Expression

  -- `return E;`
  | ReturnStatement (Maybe Expression)

  -- `break E;`
  | BreakStatement (Maybe Expression)

  -- NOTE: We don’t have an empty statement because the printer will never print empty statements.
  -- Including an empty statement in our AST means we might attach comments to the empty statement.
  -- Which we don’t want!

  -- `fun f() {}`
  | FunctionDeclaration Identifier Function

  -- If we find a error in the CST when converting it to a printer AST then we will bail out the
  -- conversion and put a `ConcreteStatement` node directly in our AST with a reference to the
  -- original source code. Our printer will print out this statement verbatim.
  | ConcreteStatement (Recover CST.Statement)

-- `fun() {}`
data Function = Function
  { functionQuantifiers :: [MaybeComment Quantifier]
  , functionParameters :: [MaybeComment FunctionParameter]
  , functionReturn :: Maybe Type
  , functionBody :: Block
  }

-- `P: T`
data FunctionParameter = FunctionParameter Pattern (Maybe Type)

-- A block of Brite statements.
newtype Block = Block
  { blockStatements :: [MaybeComment Statement]
  }

data Constant
  -- `true`, `false`
  = BooleanConstant Bool

data Expression = Expression
  -- The leading comments that are attached to this expression.
  { expressionLeadingComments :: [AttachedComment]
  -- The trailing comments that are attached to this expression.
  , expressionTrailingComments :: [AttachedComment]
  -- The representation for this expression.
  , expressionNode :: ExpressionNode
  }

-- Removes the expression trailing comments from our `Expression` and returns them.
takeExpressionTrailingComments :: Expression -> (Expression, [AttachedComment])
takeExpressionTrailingComments x@(Expression { expressionTrailingComments = [] }) = (x, [])
takeExpressionTrailingComments x = (x { expressionTrailingComments = [] }, expressionTrailingComments x)

data ExpressionNode
  -- `C`
  = ConstantExpression Constant

  -- `x`
  | VariableExpression Identifier

  -- `fun() {}`
  | FunctionExpression Function

  -- `f(E)`
  --
  -- The programmer may write comments between arguments.
  | CallExpression Expression [MaybeComment Expression]

  -- `{p: E}`
  --
  -- The programmer may write comments between properties.
  | ObjectExpression [MaybeComment ObjectExpressionProperty] (Maybe Expression)

  -- `E.p`
  --
  -- There may be unattached comments between the expression and the property. This allows us to
  -- print code like this:
  --
  -- ```ite
  -- myList
  --   // Do something.
  --   .map(fun() { ... })
  --   // Do something else.
  --   .filter(fun() { ... })
  -- ```
  | PropertyExpression Expression [UnattachedComment] Identifier

  -- `-E`
  | UnaryExpression UnaryOperator Expression

  -- `E + E`
  --
  -- There may be unattached comments between the binary operator and the right-hand-side
  -- expression. This allows us to print code like this:
  --
  -- ```ite
  -- a +
  -- // Here’s why we use “b”.
  -- b +
  -- // Here’s why we use “c”.
  -- c
  -- ```
  --
  -- NOTE: Logical operators “and” (`&&`) and “or” (`||`) are included in this AST node.
  | BinaryExpression Expression BinaryOperator [UnattachedComment] Expression

  -- `if E {} else {}`
  | ConditionalExpression ConditionalExpressionIf

  -- `do {}`
  | BlockExpression Block

  -- `loop {}`
  | LoopExpression Block

  -- `(E: T)`
  --
  -- NOTE: We never print unnecessary parentheses. Which is why our `WrappedExpression` AST node
  -- requires a type annotation instead of an optional type annotation. The printer will decide
  -- which nodes to wrap based on need and aesthetics.
  | WrappedExpression Expression Type

-- `p: E`
data ObjectExpressionProperty = ObjectExpressionProperty Identifier (Maybe Expression)

-- `if E { ... }`
data ConditionalExpressionIf =
  ConditionalExpressionIf Expression Block (Maybe ConditionalExpressionElse)

data ConditionalExpressionElse
  -- `else { ... }`
  --
  -- The programmer may write comments before `else` and `else if` which allows for code like:
  --
  -- ```ite
  -- // We check `x` because...
  -- if x {
  --   doSomething();
  -- }
  -- // Otherwise, do something else.
  -- else {
  --   doSomethingElse();
  -- }
  -- ```
  = ConditionalExpressionElse [UnattachedComment] Block
  -- `else if E { ... }`
  --
  -- The programmer may write comments before `else` and `else if` which allows for code like:
  --
  -- ```ite
  -- // We check `x` because...
  -- if x {
  --   doSomething();
  -- }
  -- // We check `y` because...
  -- else y {
  --   doSomethingElse();
  -- }
  -- ```
  | ConditionalExpressionElseIf [UnattachedComment] ConditionalExpressionIf

data Pattern = Pattern
  -- The leading comments that are attached to this pattern.
  { patternLeadingComments :: [AttachedComment]
  -- The trailing comments that are attached to this pattern.
  , patternTrailingComments :: [AttachedComment]
  -- The representation for this pattern.
  , patternNode :: PatternNode
  }

data PatternNode
  -- `C`
  = ConstantPattern Constant

  -- `x`
  | VariablePattern Identifier

  -- `_`
  | HolePattern

  -- `{p: P}`
  --
  -- The programmer may write comments between properties.
  | ObjectPattern [MaybeComment ObjectPatternProperty] (Maybe Pattern)

  -- NOTE: We never print unnecessary parentheses. Which is why we don’t have a `WrappedPattern` AST
  -- node. The printer will decide which nodes to wrap based on need and aesthetics.

-- `p: E`
data ObjectPatternProperty = ObjectPatternProperty Identifier (Maybe Pattern)

data Type = Type
  -- The leading comments that are attached to this type.
  { typeLeadingComments :: [AttachedComment]
  -- The trailing comments that are attached to this type.
  , typeTrailingComments :: [AttachedComment]
  -- The representation for this type.
  , typeNode :: TypeNode
  }

data TypeNode
  -- `x`
  = VariableType Identifier

  -- `!`
  | BottomType

  -- `fun() -> T`
  --
  -- The programmer may write comments between properties and parameters.
  | FunctionType [MaybeComment Quantifier] [MaybeComment Type] Type

  -- `{p: T}`
  --
  -- The programmer may write comments between properties.
  | ObjectType [MaybeComment ObjectTypeProperty] (Maybe Type)

  -- `<x> T`
  --
  -- The programmer may write comments between quantifiers.
  | QuantifiedType [MaybeComment Quantifier] Type

  -- NOTE: We never print unnecessary parentheses. Which is why we don’t have a `WrappedType` AST
  -- node. The printer will decide which nodes to wrap based on need and aesthetics.

-- `p: T`
data ObjectTypeProperty = ObjectTypeProperty Identifier Type

-- `x: T`
data Quantifier = Quantifier Identifier (Maybe (QuantifierBoundKind, Type))

-- Convert a CST module into a printer AST module.
convertModule :: CST.Module -> Module
convertModule (CST.Module ss t) =
  Module (convertStatementSequence (trivia 0 (endTokenTrivia t)) ss)
  where
    trivia _ [] = []
    trivia n (Spaces _ : ts) = trivia n ts
    trivia n (Tabs _ : ts) = trivia n ts
    trivia n (OtherWhitespace _ : ts) = trivia n ts
    trivia n (Newlines _ m : ts) = trivia (n + m) ts
    trivia n (Comment c : ts) = Left (UnattachedComment (n > 1) c) : trivia 0 ts

-- The monad we use for converting the CST into a printer AST.
newtype Conversion a = Conversion
  { runConversion :: ConversionState -> Maybe (a, ConversionState)
  }

instance Functor Conversion where
  fmap f x = Conversion $ \s1 -> do
    (a, s2) <- runConversion x s1
    return (f a, s2)

instance Applicative Conversion where
  pure a = Conversion (\s -> Just (a, s))
  x <*> y = Conversion $ \s1 -> do
    (f, s2) <- runConversion x s1
    (a, s3) <- runConversion y s2
    return (f a, s3)

instance Monad Conversion where
  x >>= f = Conversion $ \s1 -> do
    (a, s2) <- runConversion x s1
    runConversion (f a) s2

-- Panics the conversion discarding all current and future progress until the panic is caught.
panic :: Conversion a
panic = Conversion (\_ -> Nothing)

-- Panics if we have `Fatal` or `Recover`.
recover :: Recover a -> Conversion a
recover (Ok a) = return a
recover (Recover _ _ _) = panic
recover (Fatal _ _) = panic

-- Panics if we have `Fatal` or `Recover` and returns `Nothing` if the maybe was `Nothing`.
recoverMaybe :: Maybe (Recover a) -> Conversion (Maybe a)
recoverMaybe Nothing = return Nothing
recoverMaybe (Just (Ok a)) = return (Just a)
recoverMaybe (Just (Recover _ _ _)) = panic
recoverMaybe (Just (Fatal _ _)) = panic

data ConversionState = ConversionState
  -- A buffer for unattached comments at the current conversion position. Once we find a place where
  -- we can have unattached comments this list will be emptied.
  { conversionUnattachedComments :: [UnattachedComment]
  -- A list of attached leading comments at the current conversion position. Every time a new token
  -- is added we add our leading comments list to our trailing comments list. This list will only
  -- ever hold the attached leading comments for the last token added to state.
  , conversionAttachedLeadingComments :: [AttachedComment]
  -- A list of attached trailing comments at the current conversion position.
  , conversionAttachedTrailingComments :: [AttachedComment]
  -- Is there a leading empty line before the last token to be added to state but also under the
  -- first comment above that token?
  --
  -- ```ite
  -- // Hello, world!
  --
  -- let x = y;
  -- ```
  --
  -- That is we will have a leading empty line for the `x` statement above, but not for the `x`
  -- statement below.
  --
  -- ```ite
  --
  -- // Hello, world!
  -- let x = y;
  -- ```
  , conversionLeadingEmptyLine :: Bool
  }

-- Create an initial conversion state.
initialConversionState :: ConversionState
initialConversionState = ConversionState
  { conversionUnattachedComments = []
  , conversionAttachedLeadingComments = []
  , conversionAttachedTrailingComments = []
  , conversionLeadingEmptyLine = False
  }

-- Takes the unattached comments out of our conversion state.
takeConversionUnattachedComments :: Conversion [UnattachedComment]
takeConversionUnattachedComments = Conversion $ \s ->
  Just (conversionUnattachedComments s, s { conversionUnattachedComments = [] })

-- Takes the attached leading comments out of our conversion state.
takeConversionAttachedLeadingComments :: Conversion [AttachedComment]
takeConversionAttachedLeadingComments = Conversion $ \s ->
  Just (conversionAttachedLeadingComments s, s { conversionAttachedLeadingComments = [] })

-- Takes the attached trailing comments out of our conversion state.
takeConversionAttachedTrailingComments :: Conversion [AttachedComment]
takeConversionAttachedTrailingComments = Conversion $ \s ->
  Just (conversionAttachedTrailingComments s, s { conversionAttachedTrailingComments = [] })

-- Consumes a token and adds its trivia to state. Remember that we expect tokens to be added to
-- state in reverse of the order in which they were written in source code! As such comments are
-- added to the _beginning_ of our comment lists in state.
token :: Token -> Conversion ()
token t = Conversion (\s -> Just ((), tokenUpdateState t s))

-- Updates the conversion state with out token. Separate from `token` so that we can call it outside
-- of the `Conversion` monad.
tokenUpdateState :: Token -> ConversionState -> ConversionState
tokenUpdateState t s =
  let
    -- Build a reversed list of attached trailing comments using the token’s trailing trivia.
    attachedTrailingComments =
      foldr
        (\trivia cs ->
          case trivia of
            Spaces _ -> cs
            Tabs _ -> cs
            Newlines _ _ -> cs
            Comment c -> AttachedComment c : cs
            OtherWhitespace _ -> cs)
        (conversionAttachedLeadingComments s ++ conversionAttachedTrailingComments s)
        (tokenTrailingTrivia t)

    -- Get new attached comments and unattached comments by iterating in reverse through the token’s
    -- leading trivia.
    (_, initialLeadingLines, _, attachedLeadingComments, unattachedComments) =
      foldr
        (\trivia (a, n1, n, cs1, cs2) ->
          case trivia of
            -- Skip whitespace.
            Spaces _ -> (a, n1, n, cs1, cs2)
            Tabs _ -> (a, n1, n, cs1, cs2)
            OtherWhitespace _ -> (a, n1, n, cs1, cs2)

            -- If we have more than one newline between our latest unattached comment and whatever
            -- is above it then we have an empty leading line.
            Newlines _ m ->
              (False, (m +) <$> n1, n + m, cs1, case cs2 of
                UnattachedComment False c : cs | n + m > 1 -> UnattachedComment True c : cs
                cs -> cs)

            -- Attach comments if we are on the same line as our token. Finalize `n1` by
            -- transitioning from `Right` to `Left` at the first unattached comment we see.
            Comment c | a -> (True, n1, 0, AttachedComment c : cs1, cs2)
            Comment c -> (False, either Left Left n1, 0, cs1, UnattachedComment False c : cs2))
        (True, Right 0, 0, [], conversionUnattachedComments s)
        -- All tokens with leading trivia are the first token on their line. The previous new line
        -- was consumed by the trailing trivia of the last token on the last line. We model that
        -- missing new line here by adding a `Newlines` trivia.
        (if not (null (tokenLeadingTrivia t)) then Newlines LF 1 : tokenLeadingTrivia t
        else tokenLeadingTrivia t)
  in
    -- Construct the new state.
    ConversionState
      { conversionUnattachedComments = unattachedComments
      , conversionAttachedLeadingComments = attachedLeadingComments
      , conversionAttachedTrailingComments = attachedTrailingComments
      , conversionLeadingEmptyLine = either id id initialLeadingLines > 1
      }

-- Consumes a token and adds its trivia to state. All the trivia for this token (even the trailing
-- trivia) will be considered leading trivia.
leadingToken :: Token -> Conversion ()
leadingToken t = token $ t
  { tokenTrailingTrivia = []
  -- Move the trailing trivia to the leading trivia field.
  , tokenLeadingTrivia = tokenLeadingTrivia t ++ tokenTrailingTrivia t
  }

-- Convert every item in a comma list and capture the comments between items in a comma list.
-- Returns a list of the converted items.
convertCommaList :: (a -> Conversion b) -> CST.CommaList a -> Conversion [MaybeComment b]
convertCommaList f (CST.CommaList as an) = do
  -- Take the unattached comments before and after we try converting the last item in the comma
  -- list. If there is no last item then `cs2` will always be empty. If there is a list item then
  -- `cs2` might have some values.
  cs1 <- takeConversionUnattachedComments
  bn <- recoverMaybe an >>= mapM f
  cs2 <- takeConversionUnattachedComments
  -- Iterate in reverse through all items in the comma list.
  foldrM
    -- Convert each item and attempt take unattached comments for each item.
    (\(a, t) bs -> do
      recover t >>= token
      b <- recover a >>= f
      cs <- takeConversionUnattachedComments
      return (map Left cs ++ (Right b : bs)))
    -- Add up out initial list before we even start iterating through the main items.
    (map Left cs2 ++ maybeToList (Right <$> bn) ++ map Left cs1)
    as

-- Converts a sequence of CST statements into a sequence of AST statements.
convertStatementSequence :: [MaybeComment Statement] -> [Recover CST.Statement] -> [MaybeComment Statement]
convertStatementSequence =
  -- Use `foldr` to iterate in reverse through all our statements.
  foldr $ \s0 ss ->
    let
      loop s1 =
        case s1 of
          -- Add all the comments from an empty statement to the statement list, but don’t add the
          -- empty statement itself. We never print an empty statement. They are mostly only for
          -- error recovery.
          Ok (CST.EmptyStatement t) ->
            let
              ConversionState
                { conversionUnattachedComments = cs1
                , conversionAttachedLeadingComments = cs2
                , conversionAttachedTrailingComments = cs3
                , conversionLeadingEmptyLine = l
                } =
                  tokenUpdateState t initialConversionState

              cs4 = case map (UnattachedComment False . attachedComment) (cs2 ++ cs3) of
                [] -> []
                (UnattachedComment _ c) : cs | l -> UnattachedComment True c : cs
                cs -> cs
            in
              map Left cs1 ++ map Left cs4 ++ ss

          -- For all other statements attempt to convert the statement by
          -- calling `convertStatement`.
          Ok s2 ->
            case runConversion (convertStatement s2) initialConversionState of
              -- If the statement contains some parse error and we throw away all our conversion
              -- work then add our CST statement to the list. Our printer will print out the raw
              -- version of this statement instead of a pretty printed version.
              Nothing ->
                let leadingLine = triviaHasLeadingLine (tokenLeadingTrivia (CST.statementFirstToken s2)) in
                  Right (Statement leadingLine [] [] (ConcreteStatement (Ok s2))) : ss

              Just (s3, state) ->
                let
                  ConversionState
                    { conversionUnattachedComments = cs1
                    , conversionAttachedLeadingComments = cs2
                    , conversionAttachedTrailingComments = cs3
                    , conversionLeadingEmptyLine = l
                    } = state
                in
                  map Left cs1 ++ (Right (Statement l cs2 cs3 s3) : ss)

          -- If we have an error recovery statement then add a concrete, fatal, statement to our
          -- list but also recurse with an `Ok` version of our recovered statement.
          Recover ts e s2 ->
            let s3 = Statement (tokensHaveLeadingLine ts) [] [] (ConcreteStatement (Fatal ts e)) in
              Right s3 : loop (Ok s2)

          -- If we have a fatal statement then add a concrete, fatal, statement to our list.
          Fatal ts e ->
            let s2 = Statement (tokensHaveLeadingLine ts) [] [] (ConcreteStatement (Fatal ts e)) in
              Right s2 : ss
    in
      loop s0
  where
    -- Does this list of tokens have a leading line?
    tokensHaveLeadingLine [] = False
    tokensHaveLeadingLine (t : _) = triviaHasLeadingLine (tokenLeadingTrivia t)

    -- Does this list of trivia have a leading line?
    triviaHasLeadingLine [] = False
    triviaHasLeadingLine (Spaces _ : ts) = triviaHasLeadingLine ts
    triviaHasLeadingLine (Tabs _ : ts) = triviaHasLeadingLine ts
    triviaHasLeadingLine (Newlines _ _ : _) = True
    triviaHasLeadingLine (Comment _ : _) = False
    triviaHasLeadingLine (OtherWhitespace _ : ts) = triviaHasLeadingLine ts

-- Convert a CST statement into an AST statement node. `convertStatementSequence` will do the final
-- conversion from a statement node into a statement. We don’t do the final conversion to a
-- statement in this function because we want to let `convertStatementSequence` handle errors
-- and comments.
convertStatement :: CST.Statement -> Conversion StatementNode
convertStatement s0 = case s0 of
  CST.ExpressionStatement x t -> do
    recoverMaybe t >>= mapM_ token
    ExpressionStatement <$> convertExpression x

  -- Empty statements should be handled by `convertStatementSequence`!
  CST.EmptyStatement _ -> panic

-- Convert a CST block into an AST block.
convertBlock :: CST.Block -> Conversion Block
convertBlock (CST.Block t1 ss' t2) = do
  recover t2 >>= token
  cs <- takeConversionUnattachedComments
  let ss = convertStatementSequence (map Left cs) ss'
  recover t1 >>= token
  return (Block ss)

-- Convert a CST expression into an AST expression.
--
-- IMPORTANT: Remember to add tokens in reverse of the order they were declared in!!!
convertExpression :: CST.Expression -> Conversion Expression
convertExpression x0 = case x0 of
  CST.ConstantExpression (CST.BooleanConstant b t) -> build $ do
    token t
    return (ConstantExpression (BooleanConstant b))

  CST.VariableExpression (CST.Name n t) -> build $ do
    token t
    return (VariableExpression n)

  CST.UnaryExpression op t x' -> build $ do
    x <- recover x' >>= convertExpression
    leadingToken t
    return (UnaryExpression op x)

  CST.BlockExpression t b' -> build $ do
    b <- convertBlock b'
    leadingToken t
    return (BlockExpression b)

  CST.LoopExpression t b' -> build $ do
    b <- convertBlock b'
    leadingToken t
    return (LoopExpression b)

  CST.WrappedExpression t1 x' Nothing t2 -> do
    recover t2 >>= token
    x <- recover x' >>= convertExpression
    token t1
    cs1 <- takeConversionAttachedLeadingComments
    cs2 <- takeConversionAttachedTrailingComments
    return (x { expressionLeadingComments = cs1 ++ cs2 ++ expressionLeadingComments x })

  CST.ExpressionExtra x1' (Ok (CST.BinaryExpressionExtra y' ys')) -> do
    -- Iterate through all our operations in reverse. We use the same trick as `Data.Monoid.Endo` or
    -- `DList` to build the expression in the right way. The `build` function takes an `Expression`
    -- and returns an `Expression`. We may then add binary expression wrappers however we see fit.
    make <-
      foldrM
        (\y make ->
          case y of
            Ok (CST.BinaryExpressionOperation op t x2') -> do
              -- Convert the expression and then the operator token.
              x2 <- recover x2' >>= convertExpression
              token t
              -- Take all the comments in our state. The unattached comments we will put in the slot
              -- right before the right-hand-side expression.
              cs1 <- takeConversionUnattachedComments
              -- The attached comments we will attach as leading comments to our right-hand-side
              -- expression.
              cs2 <- takeConversionAttachedLeadingComments
              cs3 <- takeConversionAttachedTrailingComments
              -- Add a function which will create the desired binary expression.
              return (make . (\x1 ->
                Expression [] [] (BinaryExpression x1 op cs1
                  (x2 { expressionLeadingComments = cs2 ++ cs3 ++ expressionLeadingComments x2 }))))

            -- Panic if there was some parse error.
            Recover _ _ _ -> panic
            Fatal _ _ -> panic)
        id (Ok y' : ys')
    -- Convert our left-most expression last!
    x1 <- convertExpression x1'
    -- Call our build function with the left-most expression and return it.
    return (make x1)

  CST.ExpressionExtra x1' (Ok (CST.PropertyExpressionExtra t1 n')) -> build $ do
    CST.Name n t2 <- recover n'
    token t2
    token t1
    -- Capture unattached comments that come before the property access.
    cs <- takeConversionUnattachedComments
    x1 <- convertExpression x1'
    return (PropertyExpression x1 cs n)

  CST.ExpressionExtra x1' (Ok (CST.CallExpressionExtra t1 xs' t2)) -> build $ do
    recover t2 >>= token
    xs <- convertCommaList convertExpression xs'
    token t1
    x1 <- convertExpression x1'
    return (CallExpression x1 xs)

  CST.ExpressionExtra _ (Recover _ _ _) -> panic
  CST.ExpressionExtra _ (Fatal _ _) -> panic

  where
    build mx = do
      x <- mx
      cs1 <- takeConversionAttachedLeadingComments
      cs2 <- takeConversionAttachedTrailingComments
      return (Expression cs1 cs2 x)
