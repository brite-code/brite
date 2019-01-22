-- The Abstract Value Tree (AVT) is a type-checked version of the AST. We use the AVT for
-- compilation and for serving IDE requests like hover-types.

module Brite.Semantics.AVT
  ( Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  ) where

import Brite.Semantics.AST (Constant(..))
import Brite.Semantics.Type (Polytype)
import Brite.Syntax.Tokens (Range(..))

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
