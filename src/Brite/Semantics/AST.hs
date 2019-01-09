-- The Abstract Syntax Tree (AST) is a rather simple transformation of the Concrete Syntax Tree
-- (CST). The AST removes all the pedantic parts of the CST leaving in all the semantically relevant
-- parts. For instance, the AST removes all comments and `Recover` errors opting to express syntax
-- errors as `ErrorExpression` wrappers.

module Brite.Semantics.AST
  ( Position(..)
  , Range(..)
  , Identifier
  , identifierText
  , Name(..)
  , Module(..)
  , Statement(..)
  , StatementNode(..)
  , Declaration(..)
  , Function(..)
  , FunctionParameter(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  , ObjectExpressionProperty(..)
  , UnaryOperator(..)
  , BinaryOperator(..)
  , LogicalOperator(..)
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
  , convertExpression
  ) where

import Brite.Diagnostics
import Brite.Syntax.CST (Recover(..), UnaryOperator(..), QuantifierBoundKind(..))
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Tokens (Position(..), Range(..), rangeBetween, Identifier, identifierText, Token(..), EndToken(..))
import Control.Applicative
import Control.Monad.Writer
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt(..))

data Name = Name
  -- The range covered by a name in a document.
  { nameRange :: Range
  -- The identifier which creates the name.
  , nameIdentifier :: Identifier
  }

-- A Brite module.
newtype Module = Module
  { moduleStatements :: [Statement]
  }

data Statement = Statement
  -- The range occupied by a statement.
  { statementRange :: Range
  -- The representation of a statement.
  , statementNode :: StatementNode
  }

data StatementNode
  -- `E;`
  = ExpressionStatement Expression

  -- `let P = E;`
  | BindingStatement Pattern (Maybe Type) Expression

  -- `return E;`
  | ReturnStatement (Maybe Expression)

  -- `break;`
  | BreakStatement (Maybe Expression)

  -- Some declaration which is not order dependent unlike all statements.
  | Declaration Declaration

  -- Marks the position of some error in the AST. We may or may not have been able to recover from
  -- the error. If we recovered then the AST node will be `Just` otherwise it will be `Nothing`.
  --
  -- We will panic at runtime if someone attempts to execute this node.
  | ErrorStatement Diagnostic (Maybe StatementNode)

-- NOTE: Eventually we will also add a `TypeDeclaration`.
data Declaration
  -- `fun f() {}`
  --
  -- If we failed to parse a name for the function then the name will be `Left` instead of `Right`.
  -- We do this so we don’t have to throw away our entire function declaration just because of a
  -- missing name.
  = FunctionDeclaration (Either Diagnostic Name) Function

-- `fun() {}`
data Function = Function
  { functionQuantifiers :: [Quantifier]
  , functionParameters :: [FunctionParameter]
  , functionReturn :: Maybe Type
  , functionBody :: Block
  }

-- `P: T`
data FunctionParameter = FunctionParameter Pattern (Maybe Type)

-- `{ ... }`
newtype Block = Block
  { blockStatements :: [Statement]
  }

data Constant
  -- `true`, `false`
  = BooleanConstant Bool

data Expression = Expression
  -- The range covered by an expression in a document.
  { expressionRange :: Range
  -- The representation of an expression.
  , expressionNode :: ExpressionNode
  }

data ExpressionNode
  -- `C`
  = ConstantExpression Constant

  -- `x`
  | VariableExpression Identifier

  -- `fun() {}`
  | FunctionExpression Function

  -- `f(E)`
  | CallExpression Expression [Expression]

  -- `{p: E}`
  | ObjectExpression [ObjectExpressionProperty] (Maybe Expression)

  -- `E.p`
  | PropertyExpression Expression Name

  -- TODO: | VariantExpression

  -- `-E`
  | UnaryExpression UnaryOperator Expression

  -- `E + E`
  --
  -- Logical operators like `&&` and `||` are moved to the `LogicalExpression` variant.
  | BinaryExpression Expression BinaryOperator Expression

  -- `E && E`
  --
  -- We have different variants for `BinaryExpression` and `LogicalExpression` in our AST because
  -- when a logical expression executes we might not evaluate the second expression. This is called
  -- “short-circuiting”. For instance, `false && E` will not evaluate `E` since we know that the
  -- only solution is `false`.
  | LogicalExpression Expression LogicalOperator Expression

  -- `if E {} else {}`
  | ConditionalExpression ConditionalExpressionIf

  -- TODO: | MatchExpression

  -- `do {}`
  | BlockExpression Block

  -- `loop {}`
  | LoopExpression Block

  -- `(E)`, `(E: T)`
  | WrappedExpression Expression (Maybe Type)

  -- Marks the position of some error in the AST. We may or may not have been able to recover from
  -- the error. If we recovered then the AST node will be `Just` otherwise it will be `Nothing`.
  --
  -- We will panic at runtime if someone attempts to execute this node.
  | ErrorExpression Diagnostic (Maybe ExpressionNode)

-- `p: E`
data ObjectExpressionProperty = ObjectExpressionProperty Name (Maybe Expression)

data BinaryOperator
  -- `+`
  = Add
  -- `-`
  | Subtract
  -- `*`
  | Multiply
  -- `/`
  | Divide
  -- `%`
  | Remainder
  -- `^`
  | Exponent
  -- `==`
  | Equals
  -- `!=`
  | NotEquals
  -- `<`
  | LessThan
  -- `<=`
  | LessThanOrEqual
  -- `>`
  | GreaterThan
  -- `>=`
  | GreaterThanOrEqual

data LogicalOperator
  -- `&&`
  = And
  -- `||`
  | Or

-- `if E { ... }`
data ConditionalExpressionIf =
  ConditionalExpressionIf Expression Block (Maybe ConditionalExpressionElse)

data ConditionalExpressionElse
  -- `else { ... }`
  = ConditionalExpressionElse Block
  -- `else if E { ... }`
  | ConditionalExpressionElseIf ConditionalExpressionIf

data Pattern = Pattern
  -- The range covered by a pattern in a document.
  { patternRange :: Range
  -- The representation of a pattern.
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
  | ObjectPattern [ObjectPatternProperty] (Maybe Pattern)

  -- TODO: | VariantPattern

  -- `(P)`
  | WrappedPattern Pattern

  -- Marks the position of some error in the AST. We may or may not have been able to recover from
  -- the error. If we recovered then the AST node will be `Just` otherwise it will be `Nothing`.
  --
  -- We will panic at runtime if someone attempts to execute this node.
  | ErrorPattern Diagnostic (Maybe PatternNode)

-- `p: P`
data ObjectPatternProperty = ObjectPatternProperty Name (Maybe Pattern)

data Type = Type
  -- The range covered by the type in a document.
  { typeRange :: Range
  -- The representation of a type.
  , typeNode :: TypeNode
  }

data TypeNode
  -- `x`
  = VariableType Identifier

  -- `!`
  | BottomType

  -- `fun() -> T`
  --
  -- NOTE: We can parse quantifiers within a function type `fun<T>(T) -> T` but in our AST we
  -- represent this as a quantified type wrapping a function type.
  | FunctionType [Type] Type

  -- `{p: T}`
  | ObjectType [ObjectTypeProperty] (Maybe Type)

  -- TODO: | VariantType

  -- `<x> T`
  | QuantifiedType [Quantifier] Type

  -- `(T)`
  | WrappedType Type

  -- Marks the position of some error in the AST. We may or may not have been able to recover from
  -- the error. If we recovered then the AST node will be `Just` otherwise it will be `Nothing`.
  --
  -- We will panic at runtime if someone attempts to execute this node.
  | ErrorType Diagnostic (Maybe TypeNode)

-- `p: T`
data ObjectTypeProperty = ObjectTypeProperty Name (Maybe Type)

-- `x: T`
data Quantifier = Quantifier Name (Maybe (QuantifierBoundKind, Type))

-- Gets a range from a list of tokens. Returns `Nothing` if there are no tokens.
tokensRange :: [Token] -> Maybe Range
tokensRange [] = Nothing
tokensRange ts = Just (rangeBetween (tokenRange (head ts)) (tokenRange (last ts)))

-- Takes a `Recover Token` and writes the error if we encountered a parse error.
recoverToken :: Recover Token -> Writer (Alt Maybe Diagnostic) ()
recoverToken (Ok _) = return ()
recoverToken (Fatal _ e) = tell (pure e)
recoverToken (Recover _ e _) = tell (pure e)

-- Takes a `Recover Token` and get the range for that token. If the `Recover` is `Fatal` then there
-- may not be a range. Also writes a diagnostic if there was a parse error.
recoverTokenRange :: Recover Token -> Writer (Alt Maybe Diagnostic) (Maybe Range)
recoverTokenRange (Ok t) = return (Just (tokenRange t))
recoverTokenRange (Fatal ts e) = tell (pure e) *> return (tokensRange ts)
recoverTokenRange (Recover ts e t) = do
  tell (pure e)
  return $ Just $ case tokensRange ts of
    Nothing -> tokenRange t
    Just r -> rangeBetween r (tokenRange t)

-- The range between two ranges when one of them is optional.
rangeBetweenMaybe :: Range -> Maybe Range -> Range
rangeBetweenMaybe r Nothing = r
rangeBetweenMaybe r1 (Just r2) = rangeBetween r1 r2

-- Converts a comma list to a plain list using the provided conversion function. Using the writer
-- monad we record the first syntax error we find in the comma list.
convertCommaList :: (Recover a -> b) -> CST.CommaList a -> Writer (Alt Maybe Diagnostic) [b]
convertCommaList f (CST.CommaList as an) = do
  bs <- foldlM (\bs (a, t) -> recoverToken t *> return (f a : bs)) [] as
  -- Add the last element in the comma list and reverse.
  case an of
    Nothing -> return (reverse bs)
    Just a -> return (reverse (f a : bs))

-- Converts a CST module into an AST module.
convertModule :: CST.Module -> Module
convertModule (CST.Module ss (EndToken {})) = Module (convertStatementSequence ss)

-- Convert a sequence of recover CST statements into a list of AST statements. If any of the CST
-- statements are recovered parse errors then we insert two statements. One a fatal `ErrorStatement`
-- and the other the recovered statement.
convertStatementSequence :: [Recover CST.Statement] -> [Statement]
convertStatementSequence =
  foldr
    (\s' ss ->
      case s' of
        Ok s -> convertStatement s : ss
        Recover ts e s -> fatalErrorStatement ts e : convertStatement s : ss
        Fatal ts e -> fatalErrorStatement ts e : ss)
    []

-- Takes a statement and makes it an error statement. If the statement is already an error
-- statement then we replace the current error with our new one.
--
-- The range of our error statement will be the same as the statement we are annotating.
errorStatement :: Diagnostic -> Statement -> Statement
errorStatement e (Statement r (ErrorStatement _ x)) = Statement r (ErrorStatement e x)
errorStatement e (Statement r x) = Statement r (ErrorStatement e (Just x))

-- Create a statement from a fatal error. If the token list is not empty then the range will be
-- between the first and last token range. Otherwise the range will be take from the provided error.
fatalErrorStatement :: [Token] -> Diagnostic -> Statement
fatalErrorStatement ts e =
  Statement
    (fromMaybe (diagnosticRange e) (tokensRange ts))
    (ErrorStatement e Nothing)

-- Converts a CST statement into an AST statement.
convertStatement :: CST.Statement -> Statement
convertStatement s0 = case s0 of
  -- Convert a CST expression statement into an AST expression statement by converting the
  -- expression and checking if the optional semicolon has errors.
  CST.ExpressionStatement x' t -> build $ do
    let x = convertExpression x'
    r <- join <$> mapM recoverTokenRange t
    return $ Statement
      (rangeBetweenMaybe (expressionRange x) r)
      (ExpressionStatement x)

  where
    -- A small utility for adding the first error to the statement we find if any.
    build s1 =
      let (s, e) = runWriter s1 in
        maybe s (flip errorStatement s) (getAlt e)

-- Converts a CST block into an AST block.
convertBlock :: CST.Block -> Writer (Alt Maybe Diagnostic) (Maybe Range, Block)
convertBlock (CST.Block t1 ss' t2) = do
  r1 <- recoverTokenRange t1
  let ss = convertStatementSequence ss'
  r2 <- recoverTokenRange t2
  let
    -- Attempt to construct the range covered by the block. There might not be any tokens in the
    -- block in which case the resulting range is `Nothing`.
    start' = r1 <|> (if not (null ss) then Just (statementRange (head ss)) else Nothing)
    end' = r2 <|> (if not (null ss) then Just (statementRange (last ss)) else Nothing)
    range = case (start', end') of
      (Nothing, Nothing) -> Nothing
      (Just start, Nothing) -> Just start
      (Nothing, Just end) -> Just end
      (Just start, Just end) -> Just (rangeBetween start end)

  return (range, Block ss)

-- Takes an expression and makes it an error expression. If the expression is already an error
-- expression then we replace the current error with our new one.
--
-- The range of our error expression will be the same as the expression we are annotating.
errorExpression :: Diagnostic -> Expression -> Expression
errorExpression e (Expression r (ErrorExpression _ x)) = Expression r (ErrorExpression e x)
errorExpression e (Expression r x) = Expression r (ErrorExpression e (Just x))

-- Create an expression from a fatal error. If the token list is not empty then the range will be
-- between the first and last token range. Otherwise the range will be take from the provided error.
fatalErrorExpression :: [Token] -> Diagnostic -> Expression
fatalErrorExpression ts e =
  Expression
    (fromMaybe (diagnosticRange e) (tokensRange ts))
    (ErrorExpression e Nothing)

-- Converts a CST expression into an AST expression.
convertExpression :: CST.Expression -> Expression
convertExpression x0 = case x0 of
  -- Boolean constants are easy since they are a single token.
  CST.ConstantExpression (CST.BooleanConstant b t) ->
    Expression (tokenRange t) (ConstantExpression (BooleanConstant b))

  -- Variable expressions are easy since they are a single token.
  CST.VariableExpression (CST.Name n t) ->
    Expression (tokenRange t) (VariableExpression n)

  -- Convert the unary expression’s operand and set the range to be between the unary operator and
  -- the operand.
  CST.UnaryExpression op t x' ->
    let x = convertRecoverExpression x' in
      Expression
        (rangeBetween (tokenRange t) (expressionRange x))
        (UnaryExpression op x)

  -- Convert a conditional expression CST which may recursively have `if`, `else if`, and
  -- `else` conditions.
  CST.ConditionalExpression c0 -> build $ do
    (r, c) <- consequent c0
    return $ Expression r (ConditionalExpression c)
    where
      consequent (CST.ConditionalExpressionIf t1 x' b' a') = do
        let x = convertRecoverExpression x'
        (r2, b) <- convertBlock b'
        case a' of
          Nothing ->
            return (rangeBetweenMaybe (tokenRange t1) r2, ConditionalExpressionIf x b Nothing)
          Just (Ok a'') -> do
            (r3, a) <- alternate a''
            return (rangeBetween (tokenRange t1) r3, ConditionalExpressionIf x b (Just a))
          Just (Recover _ e a'') -> do
            tell (pure e)
            (r3, a) <- alternate a''
            return (rangeBetween (tokenRange t1) r3, ConditionalExpressionIf x b (Just a))
          Just (Fatal _ e) -> do
            tell (pure e)
            return (rangeBetweenMaybe (tokenRange t1) r2, ConditionalExpressionIf x b Nothing)

      alternate (CST.ConditionalExpressionElse t1 b') = do
        (r2, b) <- convertBlock b'
        return (rangeBetweenMaybe (tokenRange t1) r2, ConditionalExpressionElse b)

      alternate (CST.ConditionalExpressionElseIf t1 c') = do
        (r2, c) <- consequent c'
        return (rangeBetween (tokenRange t1) r2, ConditionalExpressionElseIf c)

  -- Convert a block CST expression to a block AST expression.
  CST.BlockExpression t b' -> build $ do
    (r, b) <- convertBlock b'
    return $ Expression
      (rangeBetweenMaybe (tokenRange t) r)
      (BlockExpression b)

  -- Convert a loop CST expression to a block AST expression.
  CST.LoopExpression t b' -> build $ do
    (r, b) <- convertBlock b'
    return $ Expression
      (rangeBetweenMaybe (tokenRange t) r)
      (LoopExpression b)

  CST.WrappedExpression t1 x' Nothing t2 -> build $ do
    let x = convertRecoverExpression x'
    -- NOTE: If there is an error in `t2` then that error will be attached to the
    -- `WrappedExpression` and will be thrown first at runtime instead of an error attached to `x`
    -- which technically came first in the source code.
    --
    -- This is acceptable since the error thrown at runtime is indicative not of source order but
    -- instead of execution order. Technically, the wrapped expression executes first.
    r2 <- recoverTokenRange t2
    return $ Expression
      (rangeBetween (tokenRange t1) (fromMaybe (expressionRange x) r2))
      (WrappedExpression x Nothing)

  -- Convert all the binary expression operations in the CST’s list representation of a binary
  -- expression into our AST representation. We’ll also need to convert some binary expressions into
  -- logical expressions.
  CST.ExpressionExtra x' (Ok (CST.BinaryExpressionExtra y' ys')) ->
    let x = convertExpression x' in
      foldl
        (\l y ->
          case y of
            -- NOTE: If `Token {}` is ever wrapped in `Recover` then we’ll need to handle the error.
            Ok (CST.BinaryExpressionOperation op (Token {}) r') ->
              let r = convertRecoverExpression r' in
                make (rangeBetween (expressionRange l) (expressionRange r)) l op r

            -- If the binary operation was recovered after skipping some tokens then create our
            -- binary expression but make sure it is annotated with the error.
            Recover _ e (CST.BinaryExpressionOperation op (Token {}) r') ->
              let r = convertRecoverExpression r' in
                errorExpression e (make (rangeBetween (expressionRange l) (expressionRange r)) l op r)

            -- If we failed to parse a binary expression operation then annotate our left-hand side
            -- expression with the error.
            Fatal _ e -> errorExpression e l)
        x (Ok y' : ys')
    where
      make r x CST.Add y = Expression r (BinaryExpression x Add y)
      make r x CST.Subtract y = Expression r (BinaryExpression x Subtract y)
      make r x CST.Multiply y = Expression r (BinaryExpression x Multiply y)
      make r x CST.Divide y = Expression r (BinaryExpression x Divide y)
      make r x CST.Remainder y = Expression r (BinaryExpression x Remainder y)
      make r x CST.Exponent y = Expression r (BinaryExpression x Exponent y)
      make r x CST.Equals y = Expression r (BinaryExpression x Equals y)
      make r x CST.NotEquals y = Expression r (BinaryExpression x NotEquals y)
      make r x CST.LessThan y = Expression r (BinaryExpression x LessThan y)
      make r x CST.LessThanOrEqual y = Expression r (BinaryExpression x LessThanOrEqual y)
      make r x CST.GreaterThan y = Expression r (BinaryExpression x GreaterThan y)
      make r x CST.GreaterThanOrEqual y = Expression r (BinaryExpression x GreaterThanOrEqual y)
      make r x CST.And y = Expression r (LogicalExpression x And y)
      make r x CST.Or y = Expression r (LogicalExpression x Or y)

  -- If we have a completely ok property expression then convert to an AST property expression.
  --
  -- NOTE: If `Token {}` is ever wrapped in `Recover` then we’ll need to handle the error.
  CST.ExpressionExtra x' (Ok (CST.PropertyExpressionExtra (Token {}) (Ok (CST.Name n t)))) ->
    let x = convertExpression x' in
      Expression
        (rangeBetween (expressionRange x) (tokenRange t))
        (PropertyExpression x (Name (tokenRange t) n))

  -- Redirect property name recovery to `Ok` after attaching an error.
  CST.ExpressionExtra x (Ok (CST.PropertyExpressionExtra t (Recover _ e n))) ->
    errorExpression e (convertExpression (CST.ExpressionExtra x (Ok (CST.PropertyExpressionExtra t (Ok n)))))

  -- Treat a property expression with a fatal property name the same as a fatal `ExpressionExtra`.
  CST.ExpressionExtra x (Ok (CST.PropertyExpressionExtra (Token {}) (Fatal _ e))) ->
    errorExpression e (convertExpression x)

  -- Convert a CST call expression to an AST call expression..
  CST.ExpressionExtra x' (Ok (CST.CallExpressionExtra t1 xs' t2)) -> build $ do
    let x = convertExpression x'
    xs <- convertCommaList convertRecoverExpression xs'
    r2 <- recoverTokenRange t2
    return $ Expression
      (rangeBetween
        (expressionRange x)
        -- For the range end use the closing parentheses token, if that doesn’t exist use the last
        -- argument range, if there are no arguments use the opening parentheses token.
        (fromMaybe
          (tokenRange t1)
          (r2 <|> (if not (null xs) then Just (expressionRange (last xs)) else Nothing))))
      (CallExpression x xs)

  -- Add the error from our error recovery and redirect with `Ok`.
  CST.ExpressionExtra x (Recover _ e extra) ->
    errorExpression e (convertExpression (CST.ExpressionExtra x (Ok extra)))

  -- NOTE: The range of the error does not extend the expression range. This expression will be
  -- treated by the type system without any extra extensions so extending the range would break
  -- that intuition.
  --
  -- For example, if we have `f(x 😈)` then we will have a fatal `ExpressionExtra`. We want to
  -- assign a type and in errors point to `x` and not `x 😈`.
  CST.ExpressionExtra x (Fatal _ e) -> errorExpression e (convertExpression x)

  where
    -- A small utility for adding the first error to the expression we find if any.
    build x1 =
      let (x, e) = runWriter x1 in
        maybe x (flip errorExpression x) (getAlt e)

-- Converts a CST expression wrapped in `Recover` into an AST expression.
convertRecoverExpression :: Recover CST.Expression -> Expression
convertRecoverExpression (Ok x) = convertExpression x
convertRecoverExpression (Recover _ e x) = errorExpression e (convertExpression x)
convertRecoverExpression (Fatal ts e) = fatalErrorExpression ts e
