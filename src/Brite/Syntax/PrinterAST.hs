-- The printer AST is used to provide a more abstract representation of the user’s code to the
-- printer instead of the pedantic CST. The printer AST is also very useful for code generators.

module Brite.Syntax.PrinterAST
  ( Identifier
  , Module(..)
  , Statement(..)
  , StatementNode(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  , UnaryOperator(..)
  , BinaryOperator(..)
  , Pattern(..)
  , PatternNode(..)
  , LeadingCommentKind(..)
  ) where

import Brite.Syntax.CST (Recover(..), UnaryOperator(..), BinaryOperator(..))
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Tokens
import Control.Applicative
import Data.Foldable (mapM_)
import Data.Maybe (maybeToList)

-- A module in a Brite program.
newtype Module = Module { moduleStatements :: [Statement] }

data Statement = Statement
  -- The comments surrounding a statement.
  { statementComments :: Comments
  -- The representation for our statement.
  , statementNode :: StatementNode
  }

data StatementNode
  -- `E;`
  = ExpressionStatement Expression
  -- `let x = E;`
  | BindingStatement Pattern Expression
  -- `return E;`
  | ReturnStatement (Maybe Expression)
  -- `break E;`
  | BreakStatement (Maybe Expression)
  -- `;`
  | EmptyStatement

  -- TODO: | Declaration

  -- Bail out and use a statement from the Concrete Syntax Tree.
  | ConcreteStatement CST.Statement

-- `{ ... }`
newtype Block = Block [Statement]

data Constant
  -- `true`, `false`
  = BooleanConstant Bool

data Expression = Expression
  -- The comments surrounding an expression.
  { expressionComments :: Comments
  -- The representation for our expression.
  , expressionNode :: ExpressionNode
  }

data ExpressionNode
  -- `C`
  = ConstantExpression Constant
  -- `x`
  | VariableExpression Identifier

  -- TODO: | FunctionExpression
  -- TODO: | ObjectExpression
  -- TODO: | VariantExpression

  -- `-E`
  | UnaryExpression UnaryOperator Expression
  -- `E + E`
  | BinaryExpression Expression BinaryOperator Expression

  -- TODO: | ConditionalExpression
  -- TODO: | MatchExpression

  -- `do {}`
  | BlockExpression Block
  -- `(E)`
  | WrappedExpression Expression
  -- `E.p`
  | PropertyExpression Expression Identifier
  -- `f(E)`
  | CallExpression Expression [Expression]

data Pattern = Pattern
  -- The comments surrounding a pattern.
  { patternComments :: Comments
  -- The representation for our pattern.
  , patternNode :: PatternNode
  }

data PatternNode
  -- `x`
  = VariablePattern Identifier

-- Comments surrounding a non-empty node on the leading and trailing sides.
data Comments = Comments
  -- Comments that come before a node. Each leading comment has a kind which we also specify.
  { leadingComments :: [(Comment, LeadingCommentKind)]
  -- Comments that come at the end of a node _on the same line_.
  , trailingComments :: [Comment]
  }

-- The kind of a leading comment.
data LeadingCommentKind
  -- The leading comment is on the line above the code.
  --
  -- ```
  -- // Hello, world!
  -- E;
  -- ```
  = LeadingCommentOnLineAbove
  -- The leading comment is on a line above an empty line above the code.
  --
  -- ```
  -- // Hello, world!
  --
  -- E;
  -- ```
  | LeadingCommentOnLineAboveEmptyLine
  -- The leading comment is on the same line as the code. This is only a valid leading comment kind
  -- for block comments.
  --
  -- ```
  -- /* Hello, world! */ E;
  -- ```
  | LeadingCommentOnSameLine

-- Returns the leading comment kind based on how many lines come after a comment.
leadingCommentKind :: Int -> LeadingCommentKind
leadingCommentKind 0 = LeadingCommentOnSameLine
leadingCommentKind 1 = LeadingCommentOnLineAbove
leadingCommentKind _ = LeadingCommentOnLineAboveEmptyLine

-- Monad for our CST to printer AST conversion. Basically a `Maybe` and `State` monad put together.
-- We could use the monad transformer `MaybeT (StateT ConversionState) a`, but I (Caleb) prefer to
-- hand-roll monads for now.
newtype Conversion a = Conversion
  { runConversion :: ConversionState -> Maybe (ConversionState, a)
  }

instance Functor Conversion where
  fmap f (Conversion g) = Conversion (fmap (fmap f) . g)

instance Applicative Conversion where
  pure a = Conversion (\s -> pure (s, a))
  Conversion cf <*> Conversion ca = Conversion $ \s1 -> do
    (s2, f) <- cf s1
    (s3, a) <- ca s2
    return (s3, f a)

instance Alternative Conversion where
  empty = Conversion (\_ -> empty)
  Conversion ca <|> Conversion cb = Conversion (\s1 -> ca s1 <|> cb s1)

instance Monad Conversion where
  Conversion ca >>= f = Conversion $ \s1 -> do
    (s2, a) <- ca s1
    runConversion (f a) s2

-- Only recovers if the result is ok.
recover :: Recover a -> Conversion a
recover (Ok a) = pure a
recover (Recover _ _ _) = empty
recover (Fatal _ _) = empty

-- Only recovers if the result is ok.
recoverMaybe :: Maybe (Recover a) -> Conversion (Maybe a)
recoverMaybe Nothing = pure Nothing
recoverMaybe (Just (Ok a)) = pure (Just a)
recoverMaybe (Just (Recover _ _ _)) = empty
recoverMaybe (Just (Fatal _ _)) = empty

-- Get the comments from some leading trivia. We track the number of new lines which come after each
-- comment to determine the `LeadingCommentKind` for each comment.
triviaComments :: [Trivia] -> (Int, [(Comment, Int)])
triviaComments = flip foldr (0, []) $ \t (ls, cs) ->
  case t of
    Spaces _ -> (ls, cs)
    Tabs _ -> (ls, cs)
    Newlines _ n -> (ls + n, cs)
    OtherWhitespace _ -> (ls, cs)
    Comment c -> (0, (c, ls) : cs)

-- The state of our conversion.
data ConversionState = ConversionState
  -- The first token we found while converting. The first token is significant because its leading
  -- comments should be part of the parent node.
  { conversionFirstToken :: Maybe Token
  -- The tokens we found as part of a conversion in reverse order.
  , conversionTokensReversed :: [Token]
  }

-- Consumes a token and adds it to state.
token :: Token -> Conversion ()
token t = Conversion (\s -> Just (update s, ()))
  where
    update (ConversionState Nothing ts) = ConversionState (Just t) ts
    update (ConversionState t1@(Just _) ts) = ConversionState t1 (t : ts)

-- Given a conversion we return the leading and trailing comments of that conversion.
--
-- This function may be a little tricky to understand.
node :: Conversion a -> Conversion (Comments, a)
node (Conversion ca) = Conversion $ \(ConversionState t1' ts1) -> do
  (ConversionState t2 ts2', a) <- ca (ConversionState Nothing [])
  let
    -- Split the first token into a token with no trailing trivia and a token with no
    -- leading trivia.
    (t1a, t1b) = case t1' of
      Nothing -> (Nothing, Nothing)
      Just t -> (Just t { tokenTrailingTrivia = [] }, Just t { tokenLeadingTrivia = [] })

    -- Remove the trailing trivia from the last token of the conversion we just executed.
    (trailing, ts2) = case ts2' of
      [] -> ([], [])
      t : ts -> (tokenTrailingTrivia t, (t { tokenTrailingTrivia = [] }) : ts)

    -- Iterate in reverse through all the tokens which have comments we want in our leading comments
    -- list.
    --
    -- This is an interesting use of Haskell syntax to do just that. `tokenComments` is a `foldl`.
    -- We could write `tokenComments (0, []) (ts2 ++ maybeToList t2 ++ ts1)` but `++` is inefficient
    -- compared to running the fold multiple times, so that’s what we do. We just use infix syntax
    -- because it looks better.
    (_, leading) = (0, [])
      `tokenComments` ts2
      `tokenComments` maybeToList t2
      `tokenComments` ts1
      `tokenComments` maybeToList t1b

    -- Construct the comments object from our leading and trailing trivia.
    comments =
      Comments
        (map (\(c, ls) -> (c, leadingCommentKind ls)) leading)
        (map fst (snd (triviaComments (0, []) trailing)))

  -- The new state holds just the first token we saw coming into this function.
  return (ConversionState t1a [], (comments, a))
  where
    -- Iterates in reverse through some tokens and adds all the comments from each trivia in
    -- addition to the number of lines which comes after the comment to a list.
    --
    -- `foldl` iterates from the start of a list, but the list we are iterating over is in reverse,
    -- so in fact we are iterating through all our tokens in reverse.
    tokenComments = foldl $ \(ls1, cs1) t ->
      -- Remember, we are iterating in reverse so that means `ls1` will be the number of lines
      -- before the first comment in the _next_ token’s leading trivia.
      let (_, cs2) = triviaComments (ls1, cs1) (tokenTrailingTrivia t) in
        triviaComments (0, cs2) (tokenLeadingTrivia t)

    -- Iterates in reverse through some trivia and adds each comment in addition to the number of
    -- lines which comes after the comment to a list.
    triviaComments = foldr $ \t (ls, cs) ->
      case t of
        Spaces _ -> (ls, cs)
        Tabs _ -> (ls, cs)
        Newlines _ n -> (ls + n, cs)
        OtherWhitespace _ -> (ls, cs)
        Comment c -> (0, (c, ls) : cs)

-- moduleFromCST :: CST.Module -> Module

-- Converts a CST statement to a printer statement. May fail if the CST contains syntax errors.
statementFromCST :: CST.Statement -> Conversion Statement
statementFromCST x = (uncurry Statement <$>) . node $
  case x of
    CST.ExpressionStatement e' t -> do
      e <- expressionFromCST e'
      recoverMaybe t >>= mapM_ token
      return (ExpressionStatement e)

    CST.BindingStatement t1 p' Nothing t2 e' t3 -> do
      token t1
      p <- recover p' >>= patternFromCST
      recover t2 >>= token
      e <- recover e' >>= expressionFromCST
      recoverMaybe t3 >>= mapM_ token
      return (BindingStatement p e)

    CST.ReturnStatement t1 e' t2 -> do
      token t1
      e <- recoverMaybe e' >>= mapM expressionFromCST
      recoverMaybe t2 >>= mapM_ token
      return (ReturnStatement e)

    CST.BreakStatement t1 e' t2 -> do
      token t1
      e <- recoverMaybe e' >>= mapM expressionFromCST
      recoverMaybe t2 >>= mapM_ token
      return (BreakStatement e)

    CST.EmptyStatement t -> do
      token t
      return EmptyStatement

-- Converts a CST expression to a printer AST expression. May fail if the CST contains
-- syntax errors.
expressionFromCST :: CST.Expression -> Conversion Expression
expressionFromCST x = (uncurry Expression <$>) . node $
  case x of
    CST.VariableExpression (CST.Name ident t) -> do
      token t
      return (VariableExpression ident)

    CST.UnaryExpression op t e' -> do
      token t
      e <- recover e' >>= expressionFromCST
      return (UnaryExpression op e)

    CST.BinaryExpression l' extra -> do
      l <- recover l' >>= expressionFromCST
      CST.BinaryExpressionExtra op t r' <- recover extra
      token t
      r <- recover r' >>= expressionFromCST
      return (BinaryExpression l op r)

    -- TODO: CST.BlockExpression

    CST.WrappedExpression t1 e' Nothing t2 -> do
      token t1
      e <- recover e' >>= expressionFromCST
      recover t2 >>= token
      return (WrappedExpression e)

    CST.ExpressionExtra e' extra' -> do
      e <- expressionFromCST e'
      extra <- recover extra'
      case extra of
        CST.PropertyExpressionExtra t1 p -> do
          token t1
          CST.Name ident t2 <- recover p
          token t2
          return (PropertyExpression e ident)

        -- TODO: CST.CallExpressionExtra

-- Converts a CST expression to a printer AST pattern. May fail if the CST contains
-- syntax errors.
patternFromCST :: CST.Pattern -> Conversion Pattern
patternFromCST x = (uncurry Pattern <$>) . node $
  case x of
    CST.VariablePattern (CST.Name ident t) -> do
      token t
      return (VariablePattern ident)
