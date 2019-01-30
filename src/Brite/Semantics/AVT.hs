-- The Abstract Value Tree (AVT) is a type-checked version of the AST. We use the AVT for
-- compilation and for serving IDE requests like hover-types.

module Brite.Semantics.AVT
  ( Block(..)
  , Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  , Pattern(..)
  , PatternNode(..)
  ) where

import Brite.Diagnostic
import Brite.Semantics.Type (Polytype)
import Brite.Syntax.Tokens (Range(..), Identifier)

data Block

data Constant
  -- `true`, `false`
  = BooleanConstant Bool

data Expression = Expression
  -- The range of source code covered by this expression.
  { expressionRange :: Range
  -- The type of this expression.
  , expressionType :: Polytype
  -- The representation of this expression.
  , expressionNode :: ExpressionNode
  }

data ExpressionNode
  -- `C`
  = ConstantExpression Constant

  -- `x`
  | VariableExpression Identifier

  -- Marks the position of some error in the AVT. We may or may not have been able to recover from
  -- the error. If we recovered then the AVT node will be `Just` otherwise it will be `Nothing`.
  --
  -- We will panic at runtime if someone attempts to execute this node.
  | ErrorExpression Diagnostic (Maybe ExpressionNode)

data Pattern = Pattern
  -- The range of source code covered by this pattern.
  { patternRange :: Range
  -- The type of the value bound by this pattern.
  , patternType :: Polytype
  -- The representation of this pattern.
  , patternNode :: PatternNode
  }

data PatternNode
  -- `x`
  = VariablePattern Identifier
