-- The Abstract Value Tree (AVT) is a type-checked version of the AST. We use the AVT for
-- compilation and for serving IDE requests like hover-types.

module Brite.Semantics.AVT
  ( Statement(..)
  , StatementNode(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  , Pattern(..)
  , PatternNode(..)
  ) where

import Brite.Diagnostic
import Brite.Semantics.Type (Polytype)
import Brite.Syntax.Identifier
import Brite.Syntax.Range

newtype Statement = Statement
  -- The representation of this statement.
  { statementNode :: StatementNode
  }

data StatementNode
  -- `E;`
  = ExpressionStatement Expression

  -- `let P = E;`
  | BindingStatement Pattern Expression

-- `{ ... }`
newtype Block = Block
  { blockStatements :: [Statement]
  }

data Constant
  -- `void`
  = VoidConstant
  -- `true`, `false`
  | BooleanConstant Bool

data Expression = Expression
  -- The range of source code covered by this expression.
  { expressionRange :: Range
  -- The representation of this expression.
  , expressionNode :: ExpressionNode
  }

data ExpressionNode
  -- `C`
  = ConstantExpression Constant

  -- `x`
  | VariableExpression Identifier

  -- `fun() {}`
  | FunctionExpression Pattern Block

  -- `f(E)`
  | CallExpression Expression [Expression]

  -- `if E {} else {}`
  | ConditionalExpression Expression Block Block

  -- `do {}`
  | BlockExpression Block

  -- `(E: T)`
  --
  -- NOTE: We only remember wrapped expressions with a type annotation in our AVT. We don’t remember
  -- plain ol’ wrapped expressions with no type annotation.
  | WrappedExpression Expression Polytype

  -- Marks the position of some error in the AVT. We may or may not have been able to recover from
  -- the error. If we recovered then the AVT node will be `Just` otherwise it will be `Nothing`.
  --
  -- We will panic at runtime if someone attempts to execute this node.
  | ErrorExpression Diagnostic (Maybe ExpressionNode)

data Pattern = Pattern
  -- The range of source code covered by this pattern.
  { patternRange :: Range
  -- The representation of this pattern.
  , patternNode :: PatternNode
  }

data PatternNode
  -- `x`
  = VariablePattern Identifier
