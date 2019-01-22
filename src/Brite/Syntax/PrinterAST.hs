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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brite.Syntax.PrinterAST
  ( Module(..)
  , UnattachedComment(..)
  , AttachedComment(..)
  , MaybeComment
  , CommaListItem
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
  , ConditionalExpressionIf(..)
  , ConditionalExpressionElse(..)
  , Pattern(..)
  , PatternNode(..)
  , ObjectPatternProperty(..)
  , Type(..)
  , TypeNode(..)
  , ObjectTypeProperty(..)
  , Quantifier(..)
  , QuantifierBoundFlexibility(..)
  , convertModule
  ) where

import Brite.Syntax.CST (Recover(..), PrefixOperator(..), InfixOperator(..), QuantifierBoundFlexibility(..))
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Tokens
import Control.Applicative ((<|>))
import Data.Foldable (foldlM, foldrM)
import Data.Maybe (fromMaybe)

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
  deriving (Show)

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
  deriving (Show)

-- Either an unattached comment or some other AST node.
type MaybeComment a = Either UnattachedComment a

-- A comma list item is either an unattached comment or an item with some attached comments. From
-- the item’s comma.
type CommaListItem a = MaybeComment (a, [AttachedComment])

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
  | BindingStatement Pattern (Maybe Type) [UnattachedComment] Expression

  -- `return E;`
  --
  -- We capture the unattached comments between the return keyword and the returned expression.
  -- (Usually from a wrapped expression.) This allows us to print:
  --
  -- ```ite
  -- return (
  --   // Hello, world!
  --   x
  -- );
  -- ```
  | ReturnStatement (Maybe ([UnattachedComment], Expression))

  -- `break E;`
  --
  -- We capture the unattached comments between the break keyword and the statement expression.
  -- (Usually from a wrapped expression.) This allows us to print:
  --
  -- ```ite
  -- break (
  --   // Hello, world!
  --   x
  -- );
  -- ```
  | BreakStatement (Maybe ([UnattachedComment], Expression))

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
  { functionQuantifiers :: [CommaListItem Quantifier]
  , functionParameters :: [CommaListItem FunctionParameter]
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
  | CallExpression Expression [CommaListItem Expression]

  -- `{p: E}`
  --
  -- The programmer may write comments between properties.
  | ObjectExpression [CommaListItem ObjectExpressionProperty] (Maybe ([UnattachedComment], Expression))

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
  | PrefixExpression PrefixOperator Expression

  -- `E + E`
  --
  -- There may be unattached comments between the infix operator and the right-hand-side
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
  | InfixExpression Expression InfixOperator [UnattachedComment] Expression

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

data ObjectExpressionProperty
  -- `{p: E}`
  = ObjectExpressionProperty [AttachedComment] Identifier [UnattachedComment] Expression
  -- `{p}`
  | ObjectExpressionPropertyPun [AttachedComment] [AttachedComment] Identifier

-- `if E { ... }`
data ConditionalExpressionIf =
  ConditionalExpressionIf [UnattachedComment] Expression Block (Maybe ConditionalExpressionElse)

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
  | ObjectPattern [CommaListItem ObjectPatternProperty] (Maybe ([UnattachedComment], Pattern))

  -- NOTE: We never print unnecessary parentheses. Which is why we don’t have a `WrappedPattern` AST
  -- node. The printer will decide which nodes to wrap based on need and aesthetics.

data ObjectPatternProperty
  -- `{p: E}`
  = ObjectPatternProperty [AttachedComment] Identifier Pattern
  -- `{p}`
  | ObjectPatternPropertyPun [AttachedComment] [AttachedComment] Identifier

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
  | FunctionType [CommaListItem Quantifier] [CommaListItem Type] Type

  -- `{p: T}`
  --
  -- The programmer may write comments between properties.
  | ObjectType [CommaListItem ObjectTypeProperty] (Maybe ([UnattachedComment], Type))

  -- `<x> T`
  --
  -- The programmer may write comments between quantifiers.
  | QuantifiedType [CommaListItem Quantifier] Type

  -- NOTE: We never print unnecessary parentheses. Which is why we don’t have a `WrappedType` AST
  -- node. The printer will decide which nodes to wrap based on need and aesthetics.

-- `p: T`
data ObjectTypeProperty = ObjectTypeProperty [AttachedComment] Identifier Type

data Quantifier
  -- `T`
  = QuantifierUnbound [AttachedComment] [AttachedComment] Identifier
  -- `T: U`
  | Quantifier [AttachedComment] Identifier QuantifierBoundFlexibility Type

-- Convert a CST module into a printer AST module.
--
-- IMPORTANT: The CST to AST converter should be pretty straightforward and rules based. All of our
-- heuristics about what a pretty printed module looks like should go into `Brite.Syntax.Printer`.
-- This is important because conversion is not the only way to generate a printer AST. The
-- programmer may also manually construct a printer AST if they wish to programmatically generate
-- some code. Any printing heuristics we implement in our conversion code here will not apply for
-- manually constructed printer AST nodes.
convertModule :: CST.Module -> Module
convertModule (CST.Module ss t) =
  Module (convertStatementSequence (trivia 1 (endTokenTrivia t)) ss)
  where
    trivia _ [] = []
    trivia n (Spaces _ : ts) = trivia n ts
    trivia n (Tabs _ : ts) = trivia n ts
    trivia n (OtherWhitespace _ : ts) = trivia n ts
    trivia n (Newlines _ m : ts) = trivia (n + m) ts
    trivia n (Comment c : ts) = Left (UnattachedComment (n > 1) c) : trivia 0 ts

-- Responsible for converting a CST into a printer AST. We use an applicative structure because to
-- correctly determine the placement of comments we need to be able to look forwards and backwards
-- in the CST to see previous and next tokens.
data Conversion a where
  -- Lifts a value into the conversion execution context.
  PureConversion :: a -> Conversion a
  -- Maps a value in the conversion execution context.
  MapConversion :: (a -> b) -> Conversion a -> Conversion b
  -- Sequences two conversions together.
  ApplyConversion :: Conversion (a -> b) -> Conversion a -> Conversion b
  -- Adds a token to the current conversion group.
  TokenConversion :: Token -> Conversion ()
  -- Creates a new conversion group which will have attached comments on either side.
  GroupConversion :: ([AttachedComment] -> [AttachedComment] -> a -> b) -> Conversion a -> Conversion b
  -- Captures all the unattached comments which come after this point.
  CommentsConversion :: Conversion [UnattachedComment]

instance Functor Conversion where
  fmap = MapConversion

instance Applicative Conversion where
  pure = PureConversion
  (<*>) = ApplyConversion

-- Adds a token to the current conversion group.
token :: Token -> Conversion ()
token = TokenConversion

-- Creates a new conversion group which will have attached comments on either side.
group :: ([AttachedComment] -> [AttachedComment] -> a -> b) -> Conversion a -> Conversion b
group = GroupConversion

-- Captures all the unattached comments which come after this point.
comments :: Conversion [UnattachedComment]
comments = CommentsConversion

data ConversionState = ConversionState
  -- All the unattached comments in the current position. Once we find a place for these comments
  -- the list will be emptied.
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
  deriving (Show)

initialConversionState :: ConversionState
initialConversionState = ConversionState [] [] [] False

-- Runs a conversion execution context and returns the resulting value along with its state.
runConversion :: Conversion a -> (a, ConversionState)
runConversion c0 =
  let
    (a, s1) = loop Nothing (Just Nothing) initialConversionState c0
    -- Recreate the group conversion behavior where we force the trivia of the last token in a group
    -- to actually be the last trivia in the group.
    s2 = case lastToken c0 of
      Just (Just t) | not (null (tokenTrailingTrivia t)) -> s1
        { conversionAttachedTrailingComments =
            conversionAttachedTrailingComments s1 ++ simpleTrailingTrivia [] (tokenTrailingTrivia t)
        }
      _ -> s1
  in
    (a, s2)
  where
    -- Loops through the conversion and produces a value along with leading and trailing trivia
    -- around that value.
    --
    -- The first and second parameters represent the “previous conversion” and “next conversion”
    -- respectively. The main reason conversion is an applicative instead of a monad is that we want
    -- to know what comes before and after our conversion while we process it.
    --
    -- * `Nothing` means no conversion comes before or after this conversion.
    -- * `Just Nothing` means a group comes before or after this conversion.
    -- * `Just (Just Token)` means a token comes before or after this conversion.
    loop :: Maybe (Maybe Token) -> Maybe (Maybe Token) -> ConversionState -> Conversion a -> (a, ConversionState)

    -- Pure conversions are lift their value and don’t add tokens.
    loop _ _ s (PureConversion a) = (a, s)

    -- Transform the value of a conversion with a function for map conversions.
    loop t1 t2 s0 (MapConversion f c) =
      let (a, s1) = loop t1 t2 s0 c in
        (f a, s1)

    -- An application conversion converts both of its arguments. We give the first conversion the
    -- first token in the second conversion and the second conversion the last token in the
    -- first conversion.
    --
    -- We also “push outwards” the leading and trailing trivia for each conversion. The first group
    -- will capture the trivia.
    loop t1 t2 s0 (ApplyConversion c1 c2) =
      let
        (a, s1) = loop (lastToken c1 <|> t1) t2 s0 c2
        (f, s2) = loop t1 (firstToken c2 <|> t2) s1 c1
      in
        (f a, s2)

    -- A token will add its leading and trailing trivia to state, _unless_ the next conversion is a
    -- group conversion. If the next conversion is a group conversion we won’t add trailing trivia.
    -- This is because the a group will eat our trailing trivia. So we don’t want to add that
    -- trivia twice.
    loop _ (Just Nothing) s0 (TokenConversion t) =
      let s1 = leadingTrivia (tokenLeadingTrivia t) (trailingTrivia [] s0) in
        ((), s1)
    loop _ _ s0 (TokenConversion t) =
      let s1 = leadingTrivia (tokenLeadingTrivia t) (trailingTrivia (tokenTrailingTrivia t) s0) in
        ((), s1)

    -- A group conversion will capture the leading and trailing trivia of its conversion. If the
    -- last conversion was an ungrouped token then we will add that token’s trailing trivia to
    -- our own.
    loop t1 _ s0 (GroupConversion f c) =
      let
        (a, s1) = loop Nothing (Just Nothing) s0 c
        -- If there was a token which came before us then add its trailing trivia to our
        -- leading trivia.
        s2 = case t1 of
          Just (Just t) -> s1
            { conversionAttachedLeadingComments =
                simpleTrailingTrivia (conversionAttachedLeadingComments s1) (tokenTrailingTrivia t)
            }
          _ -> s1
        -- Force the trailing trivia for the last token in our group to be the trailing trivia for
        -- our entire group. Otherwise another group might snatch it up.
        s3 = case lastToken c of
          Just (Just t) | not (null (tokenTrailingTrivia t)) -> s2
            { conversionAttachedTrailingComments =
                conversionAttachedTrailingComments s2 ++ simpleTrailingTrivia [] (tokenTrailingTrivia t)
            }
          _ -> s2
      in
        ( f (conversionAttachedLeadingComments s3) (conversionAttachedTrailingComments s3) a
        , s3 { conversionAttachedLeadingComments = [], conversionAttachedTrailingComments = [] }
        )

    -- Capture all the unattached comments when we see one of these bois.
    loop _ _ s CommentsConversion =
      ( conversionUnattachedComments s
      , s { conversionUnattachedComments = [], conversionLeadingEmptyLine = False }
      )

    -- Gets the first token in the conversion.
    --
    -- * `Nothing` means the conversion is empty and has no tokens.
    -- * `Just (Just t)` is the first token in the conversion.
    -- * `Just Nothing` means the conversion ends in a group. We can’t see the tokens in a group.
    firstToken :: Conversion a -> Maybe (Maybe Token)
    firstToken (PureConversion _) = Nothing
    firstToken (MapConversion _ c) = firstToken c
    firstToken (ApplyConversion c1 c2) = firstToken c1 <|> firstToken c2
    firstToken (TokenConversion t) = Just (Just t)
    firstToken (GroupConversion _ _) = Just Nothing
    firstToken CommentsConversion = Nothing

    -- Gets the last token in the conversion.
    --
    -- * `Nothing` means the conversion is empty and has no tokens.
    -- * `Just (Just t)` is the last token in the conversion.
    -- * `Just Nothing` means the conversion ends in a group. We can’t see the tokens in a group.
    lastToken :: Conversion a -> Maybe (Maybe Token)
    lastToken (PureConversion _) = Nothing
    lastToken (MapConversion _ c) = lastToken c
    lastToken (ApplyConversion c1 c2) = lastToken c2 <|> lastToken c1
    lastToken (TokenConversion t) = Just (Just t)
    lastToken (GroupConversion _ _) = Just Nothing
    lastToken CommentsConversion = Nothing

    -- Produces a conversion state based on a token’s leading trivia. Comments which have a new line
    -- between themselves and
    leadingTrivia :: [Trivia] -> ConversionState -> ConversionState
    leadingTrivia ts s0 =
      let
        (leadingLines, unattachedComments, attachedComments) = foldl
          (\(n, cs1, cs2) t ->
            case t of
              Spaces _ -> (n, cs1, cs2)
              Tabs _ -> (n, cs1, cs2)
              OtherWhitespace _ -> (n, cs1, cs2)
              Comment c -> (n, cs1, AttachedComment c : cs2)
              Newlines _ m | null cs2 -> (n + m, cs1, [])
              Newlines _ m ->
                let
                  cs3 =
                    UnattachedComment (n > 1) (attachedComment (head cs2)) :
                      (map (UnattachedComment False . attachedComment) (tail cs2) ++ cs1)
                in
                  (m, cs3, []))
          (1, [], [])
          ts

        -- If there are currently some unattached comments we don’t want to override
        -- `conversionLeadingEmptyLine`. If the first of the existing comments does not have a
        -- leading empty line but we do then we also want to add that.
        (leadingEmptyLine, currentUnattachedComments) = case conversionUnattachedComments s0 of
          UnattachedComment False c : cs | leadingLines > 1 ->
            (conversionLeadingEmptyLine s0, UnattachedComment True c : cs)
          cs@(_ : _) -> (conversionLeadingEmptyLine s0, cs)
          [] -> (leadingLines > 1, [])
      in
        s0
          { conversionUnattachedComments = foldl (flip (:)) currentUnattachedComments unattachedComments
          , conversionAttachedLeadingComments = foldl (flip (:)) (conversionAttachedLeadingComments s0) attachedComments
          , conversionLeadingEmptyLine = leadingEmptyLine
          }

    -- Produces a conversion state based on a token’s trailing trivia. It is assumed that all
    -- comments in trailing trivia are attached trailing comments since there may only be one new
    -- line at the very end of trailing trivia.
    --
    -- If we pass `True` then the trailing trivia will be added to attached _leading_ comments
    -- instead of the attached trailing comments.
    trailingTrivia :: [Trivia] -> ConversionState -> ConversionState
    trailingTrivia ts s0 = s0
      { conversionAttachedLeadingComments = []
      , conversionAttachedTrailingComments = simpleTrailingTrivia
          (conversionAttachedLeadingComments s0 ++ conversionAttachedTrailingComments s0) ts
      }

    -- Adds trivia as attached comments to the provided list.
    simpleTrailingTrivia :: [AttachedComment] -> [Trivia] -> [AttachedComment]
    simpleTrailingTrivia = foldr $ \t cs ->
      case t of
        Spaces _ -> cs
        Tabs _ -> cs
        OtherWhitespace _ -> cs
        Comment c -> AttachedComment c : cs
        Newlines _ _ -> cs

-- The `Maybe` monad but with a scarier name.
newtype Panic a = Panic { toMaybe :: Maybe a }
  deriving (Functor, Applicative, Monad)

-- Throws away all computation progress.
panic :: Panic a
panic = Panic Nothing

-- Panics if we have `Fatal` or `Recover`.
recover :: Recover a -> Panic a
recover (Ok a) = return a
recover (Recover _ _ _) = panic
recover (Fatal _ _) = panic

-- Panics if we have `Fatal` or `Recover` and returns `Nothing` if the maybe was `Nothing`.
recoverMaybe :: Maybe (Recover a) -> Panic (Maybe a)
recoverMaybe Nothing = return Nothing
recoverMaybe (Just (Ok a)) = return (Just a)
recoverMaybe (Just (Recover _ _ _)) = panic
recoverMaybe (Just (Fatal _ _)) = panic

-- Lift’s an applicative inside a maybe to an applicative wrapping a maybe.
liftMaybe :: Applicative t => Maybe (t a) -> t (Maybe a)
liftMaybe = maybe (pure Nothing) (fmap Just)

-- Convert every item in a comma list and capture the comments between items in a comma list.
-- Returns a list of the converted items.
--
-- We wrap each item in the comma list in a conversion group to capture attached comments from the
-- trailing comma. So we will also need a a function for that conversion group.
convertCommaList :: (a -> Panic (Conversion b)) -> CST.CommaList a -> Panic (Conversion [CommaListItem b])
convertCommaList f (CST.CommaList as an) = do
  -- At the end of the list we get all of the unattached comments...
  let bs0 = map Left <$> comments
  -- If we have a last item without a trailing comma then convert it!
  bn <- recoverMaybe an >>= mapM f
  let
    bs1 = case bn of
      Nothing -> bs0
      Just b -> item <$> comments <*> (flip (,) [] <$> b) <*> bs0
  -- Convert each item and take unattached comments for each item.
  foldrM
    (\(a, t') bs -> do
      b <- recover a >>= f
      t <- recover t'
      return (item <$> comments <*> ((,) <$> b <*> group comma (token t)) <*> bs))
    bs1
    as
  where
    item cs b bs = map Left cs ++ (Right b : bs)
    comma cs1 cs2 () = cs1 ++ cs2

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
              ((), state) = runConversion (token t)

              ConversionState
                { conversionUnattachedComments = cs1
                , conversionAttachedLeadingComments = cs2
                , conversionAttachedTrailingComments = cs3
                , conversionLeadingEmptyLine = l
                } = state

              cs4 = map (UnattachedComment False . attachedComment) (cs2 ++ cs3)
            in
              map Left cs1 ++ map Left cs4 ++ case ss of
                Left (UnattachedComment False c) : ss' | l -> Left (UnattachedComment True c) : ss'
                Right (Statement False a b c) : ss' | l -> Right (Statement True a b c) : ss'
                _ -> ss

          -- For all other statements attempt to convert the statement by
          -- calling `convertStatement`.
          Ok s2 ->
            case toMaybe (runConversion <$> convertStatement s2) of
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
convertStatement :: CST.Statement -> Panic (Conversion StatementNode)
convertStatement s0 = case s0 of
  CST.ExpressionStatement x' t' -> do
    x <- convertExpression x'
    t <- recoverMaybe t'
    return ((ExpressionStatement <$> x) `semicolon` t)

  CST.BindingStatement t1 p' a' t2' x' t3' -> do
    p <- recover p' >>= convertPattern
    a <- liftMaybe <$> (recoverMaybe a' >>= mapM convertTypeAnnotation)
    t2 <- recover t2'
    x <- recover x' >>= convertExpression
    t3 <- recoverMaybe t3'
    return ((BindingStatement <$> (token t1 *> p) <*> a <*> (token t2 *> comments) <*> x) `semicolon` t3)

  CST.ReturnStatement t1 x' t2' -> do
    x <- liftMaybe <$> (recoverMaybe x' >>= mapM (fmap (((,) <$> comments) <*>) . convertExpression))
    t2 <- recoverMaybe t2'
    return (token t1 *> (ReturnStatement <$> x) `semicolon` t2)

  CST.BreakStatement t1 x' t2' -> do
    x <- liftMaybe <$> (recoverMaybe x' >>= mapM (fmap (((,) <$> comments) <*>) . convertExpression))
    t2 <- recoverMaybe t2'
    return (token t1 *> (BreakStatement <$> x) `semicolon` t2)

  -- Empty statements should be handled by `convertStatementSequence`!
  CST.EmptyStatement _ -> panic

  CST.FunctionDeclaration t1 n' f' -> do
    CST.Name n t2 <- recover n'
    f <- convertFunction f'
    return (FunctionDeclaration n <$> (token t1 *> token t2 *> f))

  where
    semicolon x Nothing = x
    semicolon x (Just t) = x <* token t

-- Convert a CST function to an AST function.
convertFunction :: CST.Function -> Panic (Conversion Function)
convertFunction (CST.Function qs' t1' ps' t2' r' b') = do
  qs <- fromMaybe (pure []) <$> (recoverMaybe qs' >>= mapM convertQuantifierList)
  t1 <- recover t1'
  ps <- convertCommaList convertFunctionParameter ps'
  t2 <- recover t2'
  r <- liftMaybe <$> (recoverMaybe r' >>= mapM convertFunctionReturn)
  b <- convertBlock b'
  return (Function <$> qs <*> (token t1 *> ps <* token t2) <*> r <*> b)
  where
    convertFunctionParameter (CST.FunctionParameter p' a') = do
      p <- convertPattern p'
      a <- liftMaybe <$> (recoverMaybe a' >>= mapM convertTypeAnnotation)
      return (FunctionParameter <$> p <*> a)

    convertFunctionReturn (CST.FunctionReturn t3 t') = do
      t <- recover t' >>= convertType
      return (group wrap (token t3 *> t))
      where
        wrap [] [] x = x
        wrap cs [] x = x { typeLeadingComments = cs ++ typeLeadingComments x }
        wrap [] cs x = x { typeTrailingComments = typeTrailingComments x ++ cs }
        wrap cs1 cs2 x = x
          { typeLeadingComments = cs1 ++ typeLeadingComments x
          , typeTrailingComments = typeTrailingComments x ++ cs2
          }

-- Convert a CST block into an AST block.
convertBlock :: CST.Block -> Panic (Conversion Block)
convertBlock (CST.Block t1' ss t2') = do
  t1 <- recover t1'
  t2 <- recover t2'
  let block = (\cs -> Block (convertStatementSequence (map Left cs) ss)) <$> comments
  return (token t1 *> block <* token t2)

-- Convert a CST expression into an AST expression.
convertExpression :: CST.Expression -> Panic (Conversion Expression)
convertExpression x0 = case x0 of
  CST.ConstantExpression (CST.BooleanConstant b t) ->
    return (group Expression (token t *> pure (ConstantExpression (BooleanConstant b))))

  CST.VariableExpression (CST.Name n t) ->
    return (group Expression (token t *> pure (VariableExpression n)))

  CST.FunctionExpression t f' -> do
    f <- convertFunction f'
    return (group Expression (FunctionExpression <$> (token t *> f)))

  CST.ObjectExpression t1 ps' ext' t2' -> do
    ps <- convertCommaList property ps'
    ext <- liftMaybe <$> (recoverMaybe ext' >>= mapM extension)
    t2 <- recover t2'
    return (group Expression (ObjectExpression <$> (token t1 *> ps) <*> (ext <* token t2)))
    where
      property (CST.ObjectExpressionProperty (CST.Name n t) Nothing) =
        return (group ObjectExpressionPropertyPun (token t *> pure n))

      property (CST.ObjectExpressionProperty (CST.Name n t3) (Just v')) = do
        (CST.ObjectExpressionPropertyValue t4 x') <- recover v'
        x <- recover x' >>= convertExpression
        return (group wrap ((,) <$> (token t3 *> token t4 *> comments) <*> x))
        where
          wrap [] [] (cs, x) = ObjectExpressionProperty [] n cs x
          wrap cs1 [] (cs2, x) = ObjectExpressionProperty cs1 n cs2 x
          wrap cs1 cs2 (cs3, x) =
            wrap cs1 [] (cs3, x { expressionTrailingComments = expressionTrailingComments x ++ cs2 })

      extension (CST.ObjectExpressionExtension t' x') = do
        -- Cheat and make all of an expression bar’s trivia leading instead of trailing. This way
        -- when we print a comment on the same line as a bar it will be treated as an
        -- unattached comment.
        let t = t' { tokenLeadingTrivia = tokenLeadingTrivia t' ++ tokenTrailingTrivia t', tokenTrailingTrivia = [] }
        x <- recover x' >>= convertExpression
        return ((,) <$> comments <*> group wrap (token t *> x))
        where
          wrap [] [] x = x
          wrap cs [] x = x { expressionLeadingComments = cs ++ expressionLeadingComments x }
          wrap cs1 cs2 x = x { expressionLeadingComments = cs1 ++ cs2 ++ expressionLeadingComments x }

  CST.PrefixExpression op t x' -> do
    x <- recover x' >>= convertExpression
    return (group Expression (token t *> (PrefixExpression op <$> x)))

  CST.ConditionalExpression c0' -> do
    c0 <- consequent c0'
    return (group Expression (ConditionalExpression <$> c0))
    where
      consequent (CST.ConditionalExpressionIf t x' b' a') = do
        x <- recover x' >>= convertExpression
        b <- convertBlock b'
        a <- liftMaybe <$> (recoverMaybe a' >>= mapM alternate)
        return (ConditionalExpressionIf <$> (token t *> comments) <*> x <*> b <*> a)

      alternate (CST.ConditionalExpressionElse t b') = do
        b <- convertBlock b'
        return (ConditionalExpressionElse <$> comments <*> (token t *> b))

      alternate (CST.ConditionalExpressionElseIf t c') = do
        c <- consequent c'
        return (ConditionalExpressionElseIf <$> comments <*> (token t *> c))

  CST.BlockExpression t b' -> do
    b <- convertBlock b'
    return (group Expression (token t *> (BlockExpression <$> b)))

  CST.LoopExpression t b' -> do
    b <- convertBlock b'
    return (group Expression (token t *> (LoopExpression <$> b)))

  CST.WrappedExpression t1 x' Nothing t2' -> do
    x <- recover x' >>= convertExpression
    t2 <- recover t2'
    return (group wrap (token t1 *> x <* token t2))
    where
      wrap [] [] x = x
      wrap cs [] x = x { expressionLeadingComments = cs ++ expressionLeadingComments x }
      wrap [] cs x = x { expressionTrailingComments = expressionTrailingComments x ++ cs }
      wrap cs1 cs2 x = x
        { expressionLeadingComments = cs1 ++ expressionLeadingComments x
        , expressionTrailingComments = expressionTrailingComments x ++ cs2
        }

  CST.WrappedExpression t1 x' (Just a') t2' -> do
    x <- recover x' >>= convertExpression
    a <- recover a' >>= convertTypeAnnotation
    t2 <- recover t2'
    return (group Expression (WrappedExpression <$> (token t1 *> x) <*> (a <* token t2)))

  CST.ExpressionExtra x1' (Ok (CST.InfixExpressionExtra y' ys')) -> do
    -- Convert our left-most expression.
    x1 <- convertExpression x1'
    -- Iterate through all our operations and use them to create infix expressions...
    x3 <-
      foldlM
        (\x y ->
          case y of
            Ok (CST.InfixExpressionOperation op t x2') -> do
              -- Convert the expression and then the operator token.
              x2 <- recover x2' >>= convertExpression
              -- Convert our operation into a infix expression.
              return (group Expression (flip InfixExpression op <$> x <*> comments <*> (token t *> x2)))

            -- Panic if there was some parse error.
            Recover _ _ _ -> panic
            Fatal _ _ -> panic)
        x1
        (Ok y' : ys')
    -- We’re done!
    return x3

  CST.ExpressionExtra x' (Ok (CST.PropertyExpressionExtra t1 n')) -> do
    x <- convertExpression x'
    CST.Name n t2 <- recover n'
    return (group Expression (PropertyExpression <$> x <*> comments <*> (token t1 *> token t2 *> pure n)))

  CST.ExpressionExtra x1' (Ok (CST.CallExpressionExtra t1 xs' t2')) -> do
    x1 <- convertExpression x1'
    xs <- convertCommaList convertExpression xs'
    t2 <- recover t2'
    return (group Expression (CallExpression <$> x1 <*> (token t1 *> xs <* token t2)))

  CST.ExpressionExtra _ (Recover _ _ _) -> panic
  CST.ExpressionExtra _ (Fatal _ _) -> panic

-- Convert a CST pattern into an AST pattern.
convertPattern :: CST.Pattern -> Panic (Conversion Pattern)
convertPattern x0 = case x0 of
  CST.ConstantPattern (CST.BooleanConstant b t) ->
    return (group Pattern (token t *> pure (ConstantPattern (BooleanConstant b))))

  CST.VariablePattern (CST.Name n t) ->
    return (group Pattern (token t *> pure (VariablePattern n)))

  CST.HolePattern t ->
    return (group Pattern (token t *> pure HolePattern))

  CST.ObjectPattern t1 ps' ext' t2' -> do
    ps <- convertCommaList property ps'
    ext <- liftMaybe <$> (recoverMaybe ext' >>= mapM extension)
    t2 <- recover t2'
    return (group Pattern (ObjectPattern <$> (token t1 *> ps) <*> (ext <* token t2)))
    where
      property (CST.ObjectPatternProperty (CST.Name n t) Nothing) =
        return (group ObjectPatternPropertyPun (token t *> pure n))

      -- NOTE: Unlike object expression properties we don’t collect unattached comments between the
      -- colon and the property value. Instead we let those comments float up to the comma list
      -- root. We capture unattached comments for expression properties mostly because some
      -- expressions (like infix expressions) are printed on the next line below the property colon
      -- and we’d like to put a comment on the line above such expressions.
      property (CST.ObjectPatternProperty (CST.Name n t3) (Just v')) = do
        (CST.ObjectPatternPropertyValue t4 x') <- recover v'
        x <- recover x' >>= convertPattern
        return (group wrap (token t3 *> token t4 *> x))
        where
          wrap [] [] x = ObjectPatternProperty [] n x
          wrap cs [] x = ObjectPatternProperty cs n x
          wrap cs1 cs2 x =
            wrap cs1 [] (x { patternTrailingComments = patternTrailingComments x ++ cs2 })

      extension (CST.ObjectPatternExtension t' x') = do
        -- Cheat and make all of an expression bar’s trivia leading instead of trailing. This way
        -- when we print a comment on the same line as a bar it will be treated as an
        -- unattached comment.
        let t = t' { tokenLeadingTrivia = tokenLeadingTrivia t' ++ tokenTrailingTrivia t', tokenTrailingTrivia = [] }
        x <- recover x' >>= convertPattern
        return ((,) <$> comments <*> group wrap (token t *> x))
        where
          wrap [] [] x = x
          wrap cs [] x = x { patternLeadingComments = cs ++ patternLeadingComments x }
          wrap cs1 cs2 x = x { patternLeadingComments = cs1 ++ cs2 ++ patternLeadingComments x }

  CST.WrappedPattern t1 x' t2' -> do
    x <- recover x' >>= convertPattern
    t2 <- recover t2'
    return (group wrap (token t1 *> x <* token t2))
    where
      wrap [] [] x = x
      wrap cs [] x = x { patternLeadingComments = cs ++ patternLeadingComments x }
      wrap [] cs x = x { patternTrailingComments = patternTrailingComments x ++ cs }
      wrap cs1 cs2 x = x
        { patternLeadingComments = cs1 ++ patternLeadingComments x
        , patternTrailingComments = patternTrailingComments x ++ cs2
        }

-- Convert a CST type into an AST type.
convertType :: CST.Type -> Panic (Conversion Type)
convertType x0 = case x0 of
  CST.VariableType (CST.Name n t) ->
    return (group Type (token t *> pure (VariableType n)))

  CST.BottomType t ->
    return (group Type (token t *> pure BottomType))

  CST.FunctionType t1 qs' t2' ps' t3' t4' r' -> do
    qs <- fromMaybe (pure []) <$> (recoverMaybe qs' >>= mapM convertQuantifierList)
    t2 <- recover t2'
    ps <- convertCommaList convertType ps'
    t3 <- recover t3'
    t4 <- recover t4'
    r <- recover r' >>= convertType
    return (group Type (FunctionType <$> (token t1 *> qs) <*> (token t2 *> ps <* token t3) <*> (token t4 *> r)))

  CST.ObjectType t1 ps' ext' t2' -> do
    ps <- convertCommaList property ps'
    ext <- liftMaybe <$> (recoverMaybe ext' >>= mapM extension)
    t2 <- recover t2'
    return (group Type (ObjectType <$> (token t1 *> ps) <*> (ext <* token t2)))
    where
      -- NOTE: Unlike object expression properties we don’t collect unattached comments between the
      -- colon and the property value. Instead we let those comments float up to the comma list
      -- root. We capture unattached comments for expression properties mostly because some
      -- expressions (like infix expressions) are printed on the next line below the property colon
      -- and we’d like to put a comment on the line above such expressions.
      property (CST.ObjectTypeProperty (CST.Name n t3) t4' x') = do
        t4 <- recover t4'
        x <- recover x' >>= convertType
        return (group wrap (token t3 *> token t4 *> x))
        where
          wrap [] [] x = ObjectTypeProperty [] n x
          wrap cs [] x = ObjectTypeProperty cs n x
          wrap cs1 cs2 x =
            wrap cs1 [] (x { typeTrailingComments = typeTrailingComments x ++ cs2 })

      extension (CST.ObjectTypeExtension t' x') = do
        -- Cheat and make all of an expression bar’s trivia leading instead of trailing. This way
        -- when we print a comment on the same line as a bar it will be treated as an
        -- unattached comment.
        let t = t' { tokenLeadingTrivia = tokenLeadingTrivia t' ++ tokenTrailingTrivia t', tokenTrailingTrivia = [] }
        x <- recover x' >>= convertType
        return ((,) <$> comments <*> group wrap (token t *> x))
        where
          wrap [] [] x = x
          wrap cs [] x = x { typeLeadingComments = cs ++ typeLeadingComments x }
          wrap cs1 cs2 x = x { typeLeadingComments = cs1 ++ cs2 ++ typeLeadingComments x }

  CST.QuantifiedType qs' t' -> do
    qs <- convertQuantifierList qs'
    t <- recover t' >>= convertType
    return (group Type (QuantifiedType <$> qs <*> t))

  CST.WrappedType t1 x' t2' -> do
    x <- recover x' >>= convertType
    t2 <- recover t2'
    return (group wrap (token t1 *> x <* token t2))
    where
      wrap [] [] x = x
      wrap cs [] x = x { typeLeadingComments = cs ++ typeLeadingComments x }
      wrap [] cs x = x { typeTrailingComments = typeTrailingComments x ++ cs }
      wrap cs1 cs2 x = x
        { typeLeadingComments = cs1 ++ typeLeadingComments x
        , typeTrailingComments = typeTrailingComments x ++ cs2
        }

convertQuantifierList :: CST.QuantifierList -> Panic (Conversion [CommaListItem Quantifier])
convertQuantifierList (CST.QuantifierList t1 qs' t2') = do
  qs <- convertCommaList convertQuantifier qs'
  t2 <- recover t2'
  return (token t1 *> qs <* token t2)
  where
    convertQuantifier (CST.Quantifier (CST.Name n t) Nothing) = do
      return (group QuantifierUnbound (token t *> pure n))

    convertQuantifier (CST.Quantifier (CST.Name n t3) (Just b')) = do
      CST.QuantifierBound k t4 t' <- recover b'
      t <- recover t' >>= convertType
      return (group wrap ((,) k <$> (token t3 *> token t4 *> t)))
      where
        wrap [] [] (k, x) = Quantifier [] n k x
        wrap cs [] (k, x) = Quantifier cs n k x
        wrap cs1 cs2 (k, x) =
          wrap cs1 [] (k, x { typeTrailingComments = typeTrailingComments x ++ cs2 })

-- Converts a type annotation into a type.
convertTypeAnnotation :: CST.TypeAnnotation -> Panic (Conversion Type)
convertTypeAnnotation (CST.TypeAnnotation t a') = do
  a <- recover a' >>= convertType
  return (group wrap (token t *> a))
  where
    wrap [] [] x = x
    wrap cs [] x = x { typeLeadingComments = cs ++ typeLeadingComments x }
    wrap [] cs x = x { typeTrailingComments = typeTrailingComments x ++ cs }
    wrap cs1 cs2 x = x
      { typeLeadingComments = cs1 ++ typeLeadingComments x
      , typeTrailingComments = typeTrailingComments x ++ cs2
      }
