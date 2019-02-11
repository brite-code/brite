-- The Abstract Syntax Tree (AST) is a rather simple transformation of the Concrete Syntax Tree
-- (CST). The AST removes all the pedantic parts of the CST leaving in all the semantically relevant
-- parts. For instance, the AST removes all comments and `Recover` errors opting to express syntax
-- errors as `ErrorExpression` wrappers.
--
-- The CST to AST conversion is lossy. You lose a lot of the details that the CST provides for the
-- advantage of only keeping semantically relevant information. The CST warps the form of the syntax
-- tree for the sake of preserving all syntactic information in a parser with error recovery. The
-- CST node for infix expressions is a particularly good example of this.

module Brite.Semantics.AST
  ( Name(..)
  , Module(..)
  , Statement(..)
  , StatementNode(..)
  , Function(..)
  , FunctionParameter(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  , ObjectExpressionProperty(..)
  , PrefixOperator(..)
  , InfixOperator(..)
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
  , Flexibility(..)
  , convertModule
  , convertExpression
  , convertRecoverExpression
  , convertType
  , convertRecoverType
  ) where

import Brite.Diagnostic
import Brite.Syntax.CST (Recover(..), PrefixOperator(..), Flexibility(..))
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Identifier
import Brite.Syntax.Number
import Brite.Syntax.Range
import Brite.Syntax.Token
import Control.Applicative
import Control.Monad.Writer
import Data.Foldable (foldrM, mapM_)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt(..), Dual(..))
import Data.Sequence ((<|))

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

  -- `;`
  | EmptyStatement

  -- Marks the position of some error in the AST. We may or may not have been able to recover from
  -- the error. If we recovered then the AST node will be `Just` otherwise it will be `Nothing`.
  --
  -- We will panic at runtime if someone attempts to execute this node.
  | ErrorStatement Diagnostic (Maybe StatementNode)

-- `{ ... }`
data Block = Block
  { blockRange :: Range
  , blockStatements :: [Statement]
  }

-- `fun() {}`
data Function = Function
  { functionName :: Maybe Name
  , functionQuantifiers :: [Quantifier]
  , functionParameters :: [FunctionParameter]
  , functionReturn :: Maybe Type
  , functionBody :: Block
  }

-- `P: T`
data FunctionParameter = FunctionParameter Pattern (Maybe Type)

-- Gets the range of the function parameter.
functionParameterRange :: FunctionParameter -> Range
functionParameterRange (FunctionParameter p Nothing) = patternRange p
functionParameterRange (FunctionParameter p (Just t)) = rangeBetween (patternRange p) (typeRange t)

data Constant
  -- `void`
  = VoidConstant
  -- `true`, `false`
  | BooleanConstant Bool
  -- `42`
  | IntegerConstant IntegerBase Integer
  -- `3.1415`
  | FloatConstant Double

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

  -- `-E`
  | PrefixExpression PrefixOperator Expression

  -- `E + E`
  --
  -- Logical operators like `&&` and `||` are moved to the `LogicalExpression` variant.
  | InfixExpression Expression InfixOperator Expression

  -- `E && E`
  --
  -- We have different variants for `InfixExpression` and `LogicalExpression` in our AST because
  -- when a logical expression executes we might not evaluate the second expression. This is called
  -- “short-circuiting”. For instance, `false && E` will not evaluate `E` since we know that the
  -- only solution is `false`.
  | LogicalExpression Expression LogicalOperator Expression

  -- `if E {} else {}`
  | ConditionalExpression ConditionalExpressionIf

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

-- Gets the range of an object expression property.
objectExpressionPropertyRange :: ObjectExpressionProperty -> Range
objectExpressionPropertyRange (ObjectExpressionProperty (Name r _) Nothing) = r
objectExpressionPropertyRange (ObjectExpressionProperty (Name r _) (Just x)) =
  rangeBetween r (expressionRange x)

data InfixOperator
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

  -- `(P)`
  | WrappedPattern Pattern

  -- Marks the position of some error in the AST. We may or may not have been able to recover from
  -- the error. If we recovered then the AST node will be `Just` otherwise it will be `Nothing`.
  --
  -- We will panic at runtime if someone attempts to execute this node.
  | ErrorPattern Diagnostic (Maybe PatternNode)

-- `p: P`
data ObjectPatternProperty = ObjectPatternProperty Name (Maybe Pattern)

-- Gets the range of an object pattern property.
objectPatternPropertyRange :: ObjectPatternProperty -> Range
objectPatternPropertyRange (ObjectPatternProperty (Name r _) Nothing) = r
objectPatternPropertyRange (ObjectPatternProperty (Name r _) (Just x)) =
  rangeBetween r (patternRange x)

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

  -- `_`
  | TopType

  -- `void`
  | VoidType

  -- `fun() -> T`
  | FunctionType [Quantifier] [Type] Type

  -- `{p: T}`
  | ObjectType [ObjectTypeProperty] (Maybe Type)

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
data ObjectTypeProperty = ObjectTypeProperty Name Type

-- Gets the range of an object pattern property.
objectTypePropertyRange :: ObjectTypeProperty -> Range
objectTypePropertyRange (ObjectTypeProperty (Name r _) x) = rangeBetween r (typeRange x)

-- `x: T`
data Quantifier
  = UniversalQuantifier Name (Maybe (Flexibility, Type))
  | ExistentialQuantifier Name

-- Gets the range of a quantifier.
quantifierRange :: Quantifier -> Range
quantifierRange (UniversalQuantifier (Name r _) Nothing) = r
quantifierRange (UniversalQuantifier (Name r _) (Just (_, t))) = rangeBetween r (typeRange t)
quantifierRange (ExistentialQuantifier (Name r _)) = r

-- The monad we use while converting CST nodes.
type Conversion a = Writer (Alt Maybe Diagnostic) a

-- Gets a range from a list of tokens. Returns `Nothing` if there are no tokens.
tokensRange :: [Token] -> Maybe Range
tokensRange [] = Nothing
tokensRange ts = Just (rangeBetween (tokenRange (head ts)) (tokenRange (last ts)))

-- Takes a `Recover Token` and writes the error if we encountered a parse error.
recoverToken :: Recover Token -> Conversion ()
recoverToken (Ok _) = return ()
recoverToken (Fatal _ e) = tell (pure e)
recoverToken (Recover _ e _) = tell (pure e)

-- Takes a `Recover Token` and get the range for that token. If the `Recover` is `Fatal` then there
-- may not be a range. Also writes a diagnostic if there was a parse error.
recoverTokenRange :: Recover Token -> Conversion (Maybe Range)
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
--
-- This function asks for a callback which takes a `Recover`ed comma list item and turns that into
-- a new value which we will return. See `convertRecoverCommaList` if you’d like for the comma list
-- converter to handle error recovery.
convertCommaList :: (Recover a -> b) -> CST.CommaList a -> Conversion [b]
convertCommaList f (CST.CommaList as an) = mapWriter (fmap getDual) $ do
  -- Start the new comma list with the last element.
  let
    bs0 = case an of
      Nothing -> []
      Just a -> [f a]
  -- Traverse through all of the comma list items.
  --
  -- TODO: Test that we report the first error from a comma list.
  foldrM
    (\(a, t) bs -> mapWriter (fmap Dual) $
      recoverToken t *> return (f a : bs))
    bs0
    as

-- Converts a comma list to a plain list using the provided conversion function. Using the writer
-- monad we record the first syntax error we find in the comma list.
--
-- This function asks for a callback which does not take a `Recover`ed comma list item even though
-- internally every item in a comma list is wrapped in `Recover`. This converter will handle error
-- recovery for you. It will skip items with fatal errors and report errors from recovered items. If
-- you’d like to handle error recovery yourself see `convertCommaList`.
convertRecoverCommaList :: (a -> b) -> CST.CommaList a -> Conversion [b]
convertRecoverCommaList f (CST.CommaList as an) = mapWriter (fmap getDual) $ do
  -- Start the new comma list with the last element.
  bs0 <-
    case an of
      Nothing -> return []
      Just (Ok a) -> return [f a]
      Just (Recover _ e a) -> tell (pure (pure e)) *> return [f a]
      Just (Fatal _ e) -> tell (pure (pure e)) *> return []
  -- Traverse through the comma list items.
  foldrM
    (\(a', t) bs -> mapWriter (fmap Dual) $
      case a' of
        Ok a -> recoverToken t *> return (f a : bs)
        -- NOTE: Technically calling `recoverToken` here is a noop since adding the `Recover` and
        -- `Fatal` error means all other errors will be ignored since `Alt Maybe` uses the first
        -- error written.
        Recover _ e a -> tell (Alt (Just e)) *> recoverToken t *> return (f a : bs)
        Fatal _ e -> tell (Alt (Just e)) *> recoverToken t *> return bs)
    bs0
    as

-- Converts a CST module into an AST module.
convertModule :: CST.Module -> Module
convertModule (CST.Module ss (EndToken {})) = Module (convertStatementSequence ss)

-- Converts a CST name into an AST name.
convertName :: CST.Name -> Name
convertName (CST.Name n t) = Name (tokenRange t) n

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

  -- Convert from a CST binding statement into an AST binding statement. Reporting syntax errors
  -- from any tokens which may have them.
  CST.BindingStatement t1 p' t' t2 e' t3 -> build $ do
    let p = convertRecoverPattern p'
    let t = convertRecoverTypeAnnotation <$> t'
    recoverToken t2
    let e = convertRecoverExpression e'
    r3 <- join <$> mapM recoverTokenRange t3
    return $ Statement
      (rangeBetween (tokenRange t1) (fromMaybe (expressionRange e) r3))
      (BindingStatement p t e)

  -- Convert the return statement CST into an AST.
  CST.ReturnStatement t1 x' t2 -> build $ do
    let x = convertRecoverExpression <$> x'
    mapM_ recoverToken t2
    return $ Statement
      (rangeBetweenMaybe (tokenRange t1) (expressionRange <$> x))
      (ReturnStatement x)

  -- Convert the break statement CST into an AST.
  CST.BreakStatement t1 x' t2 -> build $ do
    let x = convertRecoverExpression <$> x'
    mapM_ recoverToken t2
    return $ Statement
      (rangeBetweenMaybe (tokenRange t1) (expressionRange <$> x))
      (BreakStatement x)

  -- Convert the empty statement CST into an AST. Easy because the empty statement only has
  -- one token.
  CST.EmptyStatement t -> Statement (tokenRange t) EmptyStatement

  where
    -- A small utility for adding the first error to the statement we find if any.
    build s1 =
      let (s, e) = runWriter s1 in
        maybe s (flip errorStatement s) (getAlt e)

-- Converts a CST block into an AST block.
--
-- It’s possible to parse a block with no tokens with sufficient syntax errors. So a starting
-- position must be provided.
convertBlock :: Position -> CST.Block -> Conversion Block
convertBlock prevStart (CST.Block t1 ss' t2) = do
  r1 <- recoverTokenRange t1
  let ss = convertStatementSequence ss'
  r2 <- recoverTokenRange t2
  let
    -- Attempt to construct the range covered by the block. There might not be any tokens in the
    -- block in which case the resulting range is `Nothing`.
    start' = r1 <|> (if not (null ss) then Just (statementRange (head ss)) else Nothing)
    end' = r2 <|> (if not (null ss) then Just (statementRange (last ss)) else Nothing)
    range = case (start', end') of
      (Nothing, Nothing) -> Range prevStart prevStart
      (Just start, Nothing) -> start
      (Nothing, Just end) -> end
      (Just start, Just end) -> rangeBetween start end

  return (Block range ss)

-- Converts a CST function into an AST function. Also returns a range between the provided starting
-- range and the end range of the function.
convertFunction :: CST.Function -> Conversion (Range, Function)
convertFunction (CST.Function t1 n' qs' t3 ps' t4 ret' b') = do
  n <- case n' of
    Nothing -> return Nothing
    Just (Ok n) -> return (Just (convertName n))
    Just (Recover _ e n) -> tell (pure e) *> return (Just (convertName n))
    Just (Fatal _ e) -> tell (pure e) *> return Nothing
  qs <- convertOptionalQuantifierList FunctionQuantifierContext qs'
  r3 <- recoverTokenRange t3
  ps <- convertCommaList convertFunctionParameter ps'
  r4 <- recoverTokenRange t4
  let ret = convertFunctionReturn <$> ret'
  let
    -- Get the last position _before_ the block.
    p =
      rangeEnd (fromMaybe (tokenRange t1) ((typeRange <$> ret) <|> r4 <|>
        (functionParameterRange <$> lastMaybe ps) <|> r3 <|> (quantifierRange <$> lastMaybe qs) <|> (nameRange <$> n)))
  b <- convertBlock p b'
  return $
    ( rangeBetween (tokenRange t1) (blockRange b)
    , Function n qs ps ret b
    )
  where
    convertFunctionParameter (Ok (CST.FunctionParameter p t)) =
      FunctionParameter (convertPattern p) (convertRecoverTypeAnnotation <$> t)
    convertFunctionParameter (Recover _ e (CST.FunctionParameter p t)) =
      FunctionParameter (errorPattern e (convertPattern p)) (convertRecoverTypeAnnotation <$> t)
    convertFunctionParameter (Fatal ts e) =
      FunctionParameter (fatalErrorPattern ts e) Nothing

    convertFunctionReturn (Ok (CST.FunctionReturn (Token {}) t)) =
      convertRecoverType t
    convertFunctionReturn (Recover _ e (CST.FunctionReturn (Token {}) t)) =
      errorType e (convertRecoverType t)
    convertFunctionReturn (Fatal ts e) =
      fatalErrorType ts e

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
  -- Convert a void constant.
  CST.ConstantExpression (CST.VoidConstant t) ->
    Expression (tokenRange t) (ConstantExpression VoidConstant)

  -- Convert a boolean constant.
  CST.ConstantExpression (CST.BooleanConstant b t) ->
    Expression (tokenRange t) (ConstantExpression (BooleanConstant b))

  -- Convert an invalid number token into an error expression.
  CST.ConstantExpression (CST.NumberConstant (InvalidNumberToken err _) t) ->
    Expression (tokenRange t) (ErrorExpression err Nothing)

  -- Convert the integer constants of various bases to an AST integer constant. The base doesn’t
  -- matter for our program’s semantics so we forget it.
  CST.ConstantExpression (CST.NumberConstant (NumberToken (DecimalInteger _ value)) t) ->
    Expression (tokenRange t) (ConstantExpression (IntegerConstant Decimal value))
  CST.ConstantExpression (CST.NumberConstant (NumberToken (BinaryInteger _ _ value)) t) ->
    Expression (tokenRange t) (ConstantExpression (IntegerConstant Binary value))
  CST.ConstantExpression (CST.NumberConstant (NumberToken (HexadecimalInteger _ _ value)) t) ->
    Expression (tokenRange t) (ConstantExpression (IntegerConstant Hexadecimal value))

  -- Convert a float constant to an AST constant.
  CST.ConstantExpression (CST.NumberConstant (NumberToken (DecimalFloat _ value)) t) ->
    Expression (tokenRange t) (ConstantExpression (FloatConstant value))

  -- Variable expressions are easy since they are a single token.
  CST.VariableExpression (CST.Name n t) ->
    Expression (tokenRange t) (VariableExpression n)

  -- Convert our function expression CST to an AST node. Most of the work is done
  -- by `convertFunction`.
  CST.FunctionExpression f' -> build $ do
    (r, f) <- convertFunction f'
    return $ Expression r (FunctionExpression f)

  -- Convert a CST object expression into an AST object expression. Any syntax error within the
  -- object expression will be used to wrap the object expression.
  CST.ObjectExpression t1 ps' ext' t2 -> build $ do
    ps <- convertRecoverCommaList convertProperty ps'
    ext <- convertExtension ext'
    r2 <- recoverTokenRange t2
    return $ Expression
      (rangeBetweenMaybe
        (tokenRange t1)
        (r2 <|> (expressionRange <$> ext) <|> (objectExpressionPropertyRange <$> lastMaybe ps)))
      (ObjectExpression ps ext)
    where
      convertProperty (CST.ObjectExpressionProperty n v) =
        ObjectExpressionProperty (convertName n) (convertPropertyValue <$> v)

      convertPropertyValue (Ok (CST.ObjectExpressionPropertyValue (Token {}) v)) =
        convertRecoverExpression v
      convertPropertyValue (Recover _ e (CST.ObjectExpressionPropertyValue (Token {}) v)) =
        errorExpression e (convertRecoverExpression v)
      convertPropertyValue (Fatal ts e) =
        fatalErrorExpression ts e

      convertExtension Nothing =
        return Nothing
      convertExtension (Just (Ok (CST.ObjectExpressionExtension (Token {}) v))) =
        return (Just (convertRecoverExpression v))
      convertExtension (Just (Recover _ e (CST.ObjectExpressionExtension (Token {}) v))) =
        tell (pure e) *> return (Just (convertRecoverExpression v))
      convertExtension (Just (Fatal _ e)) =
        tell (pure e) *> return Nothing

  -- Convert the prefix expression’s operand and set the range to be between the prefix operator and
  -- the operand.
  CST.PrefixExpression op t x' ->
    let x = convertRecoverExpression x' in
      Expression
        (rangeBetween (tokenRange t) (expressionRange x))
        (PrefixExpression op x)

  -- Convert a conditional expression CST which may recursively have `if`, `else if`, and
  -- `else` conditions.
  CST.ConditionalExpression c0 -> build $ do
    (r, c) <- consequent c0
    return $ Expression r (ConditionalExpression c)
    where
      consequent (CST.ConditionalExpressionIf t1 x' b' a') = do
        let x = convertRecoverExpression x'
        b <- convertBlock (rangeEnd (expressionRange x)) b'
        case a' of
          Nothing ->
            return (rangeBetween (tokenRange t1) (blockRange b), ConditionalExpressionIf x b Nothing)
          Just (Ok a'') -> do
            (r3, a) <- alternate a''
            return (rangeBetween (tokenRange t1) r3, ConditionalExpressionIf x b (Just a))
          Just (Recover _ e a'') -> do
            tell (pure e)
            (r3, a) <- alternate a''
            return (rangeBetween (tokenRange t1) r3, ConditionalExpressionIf x b (Just a))
          Just (Fatal _ e) -> do
            tell (pure e)
            return (rangeBetween (tokenRange t1) (blockRange b), ConditionalExpressionIf x b Nothing)

      alternate (CST.ConditionalExpressionElse t1 b') = do
        b <- convertBlock (rangeEnd (tokenRange t1)) b'
        return (rangeBetween (tokenRange t1) (blockRange b), ConditionalExpressionElse b)

      alternate (CST.ConditionalExpressionElseIf t1 c') = do
        (r2, c) <- consequent c'
        return (rangeBetween (tokenRange t1) r2, ConditionalExpressionElseIf c)

  -- Convert a block CST expression to a block AST expression.
  CST.BlockExpression t b' -> build $ do
    b <- convertBlock (rangeEnd (tokenRange t)) b'
    return $ Expression
      (rangeBetween (tokenRange t) (blockRange b))
      (BlockExpression b)

  -- Convert a loop CST expression to a block AST expression.
  CST.LoopExpression t b' -> build $ do
    b <- convertBlock (rangeEnd (tokenRange t)) b'
    return $ Expression
      (rangeBetween (tokenRange t) (blockRange b))
      (LoopExpression b)

  CST.WrappedExpression t1 x' t' t2 -> build $ do
    let x = convertRecoverExpression x'
    let t = convertRecoverTypeAnnotation <$> t'
    -- NOTE: If there is an error in `t2` then that error will be attached to the
    -- `WrappedExpression` and will be thrown first at runtime instead of an error attached to `x`
    -- which technically came first in the source code.
    --
    -- This is acceptable since the error thrown at runtime is indicative not of source order but
    -- instead of execution order. Technically, the wrapped expression executes first.
    r2 <- recoverTokenRange t2
    return $ Expression
      (rangeBetween (tokenRange t1) (fromMaybe (expressionRange x) r2))
      (WrappedExpression x t)

  -- Convert all the infix expression operations in the CST’s list representation of a infix
  -- expression into our AST representation. We’ll also need to convert some infix expressions into
  -- logical expressions.
  CST.ExpressionExtra x' (Ok (CST.InfixExpressionExtra y' ys')) ->
    let x = convertExpression x' in
      foldl
        (\l y ->
          case y of
            -- NOTE: If `Token {}` is ever wrapped in `Recover` then we’ll need to handle the error.
            Ok (CST.InfixExpressionOperation op (Token {}) r') ->
              let r = convertRecoverExpression r' in
                make (rangeBetween (expressionRange l) (expressionRange r)) l op r

            -- If the infix operation was recovered after skipping some tokens then create our
            -- infix expression but make sure it is annotated with the error.
            Recover _ e (CST.InfixExpressionOperation op (Token {}) r') ->
              let r = convertRecoverExpression r' in
                errorExpression e (make (rangeBetween (expressionRange l) (expressionRange r)) l op r)

            -- If we failed to parse a infix expression operation then annotate our left-hand side
            -- expression with the error.
            Fatal _ e -> errorExpression e l)
        x (Ok y' <| ys')
    where
      make r x CST.Add y = Expression r (InfixExpression x Add y)
      make r x CST.Subtract y = Expression r (InfixExpression x Subtract y)
      make r x CST.Multiply y = Expression r (InfixExpression x Multiply y)
      make r x CST.Divide y = Expression r (InfixExpression x Divide y)
      make r x CST.Remainder y = Expression r (InfixExpression x Remainder y)
      make r x CST.Exponent y = Expression r (InfixExpression x Exponent y)
      make r x CST.Equals y = Expression r (InfixExpression x Equals y)
      make r x CST.NotEquals y = Expression r (InfixExpression x NotEquals y)
      make r x CST.LessThan y = Expression r (InfixExpression x LessThan y)
      make r x CST.LessThanOrEqual y = Expression r (InfixExpression x LessThanOrEqual y)
      make r x CST.GreaterThan y = Expression r (InfixExpression x GreaterThan y)
      make r x CST.GreaterThanOrEqual y = Expression r (InfixExpression x GreaterThanOrEqual y)
      make r x CST.And y = Expression r (LogicalExpression x And y)
      make r x CST.Or y = Expression r (LogicalExpression x Or y)

  -- If we have a completely ok property expression then convert to an AST property expression.
  --
  -- NOTE: If `Token {}` is ever wrapped in `Recover` then we’ll need to handle the error.
  CST.ExpressionExtra x' (Ok (CST.PropertyExpressionExtra (Token {}) (Ok n))) ->
    let x = convertExpression x' in
      Expression
        (rangeBetween (expressionRange x) (tokenRange (CST.nameToken n)))
        (PropertyExpression x (convertName n))

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
          (r2 <|> (expressionRange <$> lastMaybe xs))))
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

-- Takes a pattern and makes it an error pattern. If the pattern is already an error
-- pattern then we replace the current error with our new one.
--
-- The range of our error pattern will be the same as the pattern we are annotating.
errorPattern :: Diagnostic -> Pattern -> Pattern
errorPattern e (Pattern r (ErrorPattern _ x)) = Pattern r (ErrorPattern e x)
errorPattern e (Pattern r x) = Pattern r (ErrorPattern e (Just x))

-- Create a pattern from a fatal error. If the token list is not empty then the range will be
-- between the first and last token range. Otherwise the range will be take from the provided error.
fatalErrorPattern :: [Token] -> Diagnostic -> Pattern
fatalErrorPattern ts e =
  Pattern
    (fromMaybe (diagnosticRange e) (tokensRange ts))
    (ErrorPattern e Nothing)

-- Converts a CST pattern into an AST pattern.
convertPattern :: CST.Pattern -> Pattern
convertPattern x0 = case x0 of
  -- Convert a void constant.
  CST.ConstantPattern (CST.VoidConstant t) ->
    Pattern (tokenRange t) (ConstantPattern VoidConstant)

  -- Convert a boolean constant.
  CST.ConstantPattern (CST.BooleanConstant b t) ->
    Pattern (tokenRange t) (ConstantPattern (BooleanConstant b))

  -- Convert an invalid number token into an error expression.
  CST.ConstantPattern (CST.NumberConstant (InvalidNumberToken err _) t) ->
    Pattern (tokenRange t) (ErrorPattern err Nothing)

  -- Convert the integer constants of various bases to an AST integer constant. The base doesn’t
  -- matter for our program’s semantics so we forget it.
  CST.ConstantPattern (CST.NumberConstant (NumberToken (DecimalInteger _ value)) t) ->
    Pattern (tokenRange t) (ConstantPattern (IntegerConstant Decimal value))
  CST.ConstantPattern (CST.NumberConstant (NumberToken (BinaryInteger _ _ value)) t) ->
    Pattern (tokenRange t) (ConstantPattern (IntegerConstant Binary value))
  CST.ConstantPattern (CST.NumberConstant (NumberToken (HexadecimalInteger _ _ value)) t) ->
    Pattern (tokenRange t) (ConstantPattern (IntegerConstant Hexadecimal value))

  -- Convert a float constant to an AST constant.
  CST.ConstantPattern (CST.NumberConstant (NumberToken (DecimalFloat _ value)) t) ->
    Pattern (tokenRange t) (ConstantPattern (FloatConstant value))

  -- Variable patterns are easy since they are a single token.
  CST.VariablePattern (CST.Name n t) ->
    Pattern (tokenRange t) (VariablePattern n)

  -- Hole patterns are easy since they are a single token.
  CST.HolePattern t ->
    Pattern (tokenRange t) HolePattern

  -- Convert a CST object pattern into an AST object pattern. Any syntax error within the
  -- object pattern will be used to wrap the object pattern.
  CST.ObjectPattern t1 ps' ext' t2 -> build $ do
    ps <- convertRecoverCommaList convertProperty ps'
    ext <- convertExtension ext'
    r2 <- recoverTokenRange t2
    return $ Pattern
      (rangeBetweenMaybe
        (tokenRange t1)
        (r2 <|> (patternRange <$> ext) <|> (objectPatternPropertyRange <$> lastMaybe ps)))
      (ObjectPattern ps ext)
    where
      convertProperty (CST.ObjectPatternProperty n v) =
        ObjectPatternProperty (convertName n) (convertPropertyValue <$> v)

      convertPropertyValue (Ok (CST.ObjectPatternPropertyValue (Token {}) v)) =
        convertRecoverPattern v
      convertPropertyValue (Recover _ e (CST.ObjectPatternPropertyValue (Token {}) v)) =
        errorPattern e (convertRecoverPattern v)
      convertPropertyValue (Fatal ts e) =
        fatalErrorPattern ts e

      convertExtension Nothing =
        return Nothing
      convertExtension (Just (Ok (CST.ObjectPatternExtension (Token {}) v))) =
        return (Just (convertRecoverPattern v))
      convertExtension (Just (Recover _ e (CST.ObjectPatternExtension (Token {}) v))) =
        tell (pure e) *> return (Just (convertRecoverPattern v))
      convertExtension (Just (Ok (CST.ObjectPatternExtensionHole t))) =
        return (Just (Pattern (tokenRange t) HolePattern))
      convertExtension (Just (Recover _ e (CST.ObjectPatternExtensionHole t))) =
        tell (pure e) *> return (Just (Pattern (tokenRange t) HolePattern))
      convertExtension (Just (Fatal _ e)) =
        tell (pure e) *> return Nothing

  CST.WrappedPattern t1 x' t2 -> build $ do
    let x = convertRecoverPattern x'
    -- NOTE: If there is an error in `t2` then that error will be attached to the
    -- `WrappedPattern` and will be thrown first at runtime instead of an error attached to `x`
    -- which technically came first in the source code.
    --
    -- This is acceptable since the error thrown at runtime is indicative not of source order but
    -- instead of execution order. Technically, the wrapped pattern executes first.
    r2 <- recoverTokenRange t2
    return $ Pattern
      (rangeBetween (tokenRange t1) (fromMaybe (patternRange x) r2))
      (WrappedPattern x)

  where
    -- A small utility for adding the first error to the pattern we find if any.
    build x1 =
      let (x, e) = runWriter x1 in
        maybe x (flip errorPattern x) (getAlt e)

-- Converts a CST pattern wrapped in `Recover` into an AST pattern.
convertRecoverPattern :: Recover CST.Pattern -> Pattern
convertRecoverPattern (Ok x) = convertPattern x
convertRecoverPattern (Recover _ e x) = errorPattern e (convertPattern x)
convertRecoverPattern (Fatal ts e) = fatalErrorPattern ts e

-- Takes a type and makes it an error type. If the type is already an error
-- type then we replace the current error with our new one.
--
-- The range of our error type will be the same as the type we are annotating.
errorType :: Diagnostic -> Type -> Type
errorType e (Type r (ErrorType _ x)) = Type r (ErrorType e x)
errorType e (Type r x) = Type r (ErrorType e (Just x))

-- Create a type from a fatal error. If the token list is not empty then the range will be
-- between the first and last token range. Otherwise the range will be take from the provided error.
fatalErrorType :: [Token] -> Diagnostic -> Type
fatalErrorType ts e =
  Type
    (fromMaybe (diagnosticRange e) (tokensRange ts))
    (ErrorType e Nothing)

-- Converts a CST type into an AST type.
convertType :: CST.Type -> Type
convertType x0 = case x0 of
  -- Variable types are easy since they are a single token.
  CST.VariableType (CST.Name n t) ->
    Type (tokenRange t) (VariableType n)

  -- Bottom types are easy since they are a single token.
  CST.BottomType t ->
    Type (tokenRange t) BottomType

  -- Top types are easy since they are a single token.
  CST.TopType t ->
    Type (tokenRange t) TopType

  -- Void types are easy since they are a single token.
  CST.VoidType t ->
    Type (tokenRange t) VoidType

  -- Converts a CST function type to an AST function type. Pretty involved because there are a lot
  -- of moving parts in a function type’s CST.
  CST.FunctionType t1 qs' t2 ps' t3 t4 x' -> build $ do
    qs <- convertOptionalQuantifierList FunctionQuantifierContext qs'
    recoverToken t2
    ps <- convertCommaList convertRecoverType ps'
    recoverToken t3
    recoverToken t4
    let x = convertRecoverType x'
    return $ Type
      (rangeBetween (tokenRange t1) (typeRange x))
      (FunctionType qs ps x)

  -- Convert a CST object type into an AST object type. Any syntax error within the
  -- object type will be used to wrap the object type.
  CST.ObjectType t1 ps' ext' t2 -> build $ do
    ps <- convertRecoverCommaList convertProperty ps'
    ext <- convertExtension ext'
    r2 <- recoverTokenRange t2
    return $ Type
      (rangeBetweenMaybe
        (tokenRange t1)
        (r2 <|> (typeRange <$> ext) <|> (objectTypePropertyRange <$> lastMaybe ps)))
      (ObjectType ps ext)
    where
      convertProperty (CST.ObjectTypeProperty n t3 v) =
        ObjectTypeProperty (convertName n)
          (build (recoverToken t3 *> return (convertRecoverType v)))

      convertExtension Nothing =
        return Nothing
      convertExtension (Just (Ok (CST.ObjectTypeExtension (Token {}) v))) =
        return (Just (convertRecoverType v))
      convertExtension (Just (Recover _ e (CST.ObjectTypeExtension (Token {}) v))) =
        tell (pure e) *> return (Just (convertRecoverType v))
      convertExtension (Just (Ok (CST.ObjectTypeExtensionHole t))) =
        return (Just (Type (tokenRange t) TopType))
      convertExtension (Just (Recover _ e (CST.ObjectTypeExtensionHole t))) =
        tell (pure e) *> return (Just (Type (tokenRange t) TopType))
      convertExtension (Just (Fatal _ e)) =
        tell (pure e) *> return Nothing

  -- Convert a CST quantified type to an AST quantified type.
  CST.QuantifiedType qs' t' -> build $ do
    (r, qs) <- convertQuantifierList TypeQuantifierContext qs'
    let t = convertRecoverType t'
    return $ Type
      (rangeBetween r (typeRange t))
      (QuantifiedType qs t)

  CST.WrappedType t1 x' t2 -> build $ do
    let x = convertRecoverType x'
    -- NOTE: If there is an error in `t2` then that error will be attached to the
    -- `WrappedType` and will be thrown first at runtime instead of an error attached to `x`
    -- which technically came first in the source code.
    --
    -- This is acceptable since the error thrown at runtime is indicative not of source order but
    -- instead of execution order. Technically, the wrapped type executes first.
    r2 <- recoverTokenRange t2
    return $ Type
      (rangeBetween (tokenRange t1) (fromMaybe (typeRange x) r2))
      (WrappedType x)

  where
    -- A small utility for adding the first error to the type we find if any.
    build x1 =
      let (x, e) = runWriter x1 in
        maybe x (flip errorType x) (getAlt e)

-- Converts a CST type wrapped in `Recover` into an AST type.
convertRecoverType :: Recover CST.Type -> Type
convertRecoverType (Ok x) = convertType x
convertRecoverType (Recover _ e x) = errorType e (convertType x)
convertRecoverType (Fatal ts e) = fatalErrorType ts e

-- Converts a CST type annotation wrapped in `Recover` into an AST type.
convertRecoverTypeAnnotation :: Recover CST.TypeAnnotation -> Type
convertRecoverTypeAnnotation (Ok (CST.TypeAnnotation (Token {}) t)) = convertRecoverType t
convertRecoverTypeAnnotation (Recover _ e (CST.TypeAnnotation _ t)) = errorType e (convertRecoverType t)
convertRecoverTypeAnnotation (Fatal ts e) = fatalErrorType ts e

data QuantifierContext = TypeQuantifierContext | FunctionQuantifierContext

-- Converts a CST list of quantifiers to a list of AST quantifiers. Also returns the range of the
-- CST quantifier list. Remember that the CST quantifier list is always wrapped in `<>`.
convertQuantifierList :: QuantifierContext -> CST.QuantifierList -> Conversion (Range, [Quantifier])
convertQuantifierList kind (CST.QuantifierList t1 qs' t2) = do
  qs <- convertRecoverCommaList convertQuantifier qs'
  r2 <- recoverTokenRange t2
  return
    ( rangeBetweenMaybe (tokenRange t1) (r2 <|> (quantifierRange <$> lastMaybe qs))
    , qs
    )
  where
    -- An unbound quantifier means different things depending on our context. If we are in a type
    -- context then we have an existential quantifier. If we are in a function context then we have
    -- a universal quantifier.
    convertQuantifier (CST.Quantifier n Nothing) =
      case kind of
        TypeQuantifierContext -> ExistentialQuantifier (convertName n)
        FunctionQuantifierContext -> UniversalQuantifier (convertName n) Nothing

    convertQuantifier (CST.Quantifier n (Just (Ok (CST.QuantifierBound k (Token {}) x)))) =
      UniversalQuantifier (convertName n) (Just (k, convertRecoverType x))
    convertQuantifier (CST.Quantifier n (Just (Recover _ e (CST.QuantifierBound k (Token {}) x)))) =
      UniversalQuantifier (convertName n) (Just (k, errorType e (convertRecoverType x)))
    convertQuantifier (CST.Quantifier n (Just (Fatal ts e))) =
      UniversalQuantifier (convertName n) (Just (Flexible, fatalErrorType ts e))

-- Convert an optional list of quantifiers from the CST to the AST.
convertOptionalQuantifierList :: QuantifierContext -> Maybe (Recover CST.QuantifierList) -> Conversion [Quantifier]
convertOptionalQuantifierList _ Nothing = return []
convertOptionalQuantifierList kind (Just (Ok qs)) = snd <$> convertQuantifierList kind qs
convertOptionalQuantifierList kind (Just (Recover _ e qs)) = tell (pure e) *> (snd <$> convertQuantifierList kind qs)
convertOptionalQuantifierList _ (Just (Fatal _ e)) = tell (pure e) *> return []

-- Returns `Nothing` if the list is empty and the last item if the list is non-empty.
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe as = Just (last as)
