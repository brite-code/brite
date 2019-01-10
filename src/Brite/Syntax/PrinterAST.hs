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

module Brite.Syntax.PrinterAST
  ( Module(..)
  , UnattachedComment(..)
  , AttachedComment(..)
  , Statement(..)
  , StatementNode(..)
  , Function(..)
  , FunctionParameter(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
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
  ) where

import Brite.Syntax.CST (Recover(..), UnaryOperator(..), BinaryOperator(..), QuantifierBoundKind(..))
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Tokens

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
  -- Does an empty line come before this unattached comment?
  { unattachedCommentLeadingEmptyLine :: Bool
  -- Does an empty line come after this unattached comment?
  , unattachedCommentTrailingEmptyLine :: Bool
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
  -- Does an empty line come before this statement?
  { statementLeadingEmptyLine :: Bool
  -- Does an empty line come after this statement?
  , statementTrailingEmptyLine :: Bool
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
  | ConcreteStatement CST.Statement

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

data ExpressionNode
  -- `C`
  = ConstantExpression Constant

  -- `x`
  | VariableExpression Identifier

  -- `fun() {}`
  | FunctionExpression

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
