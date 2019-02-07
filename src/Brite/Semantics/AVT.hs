-- The Abstract Value Tree (AVT) is a type-checked version of the AST. We use the AVT for
-- compilation and for serving IDE requests like hover-types.

module Brite.Semantics.AVT
  ( Name(..)
  , Statement(..)
  , StatementNode(..)
  , Block(..)
  , Constant(..)
  , Expression(..)
  , ExpressionNode(..)
  , Pattern(..)
  , PatternNode(..)
  , expressionSnippet
  ) where

import Brite.Diagnostic
import Brite.Semantics.AST (Name(..))
import Brite.Semantics.Type (Polytype)
import Brite.Semantics.TypePrinter (objectPropertyList)
import Brite.Syntax.Identifier
import Brite.Syntax.Number (IntegerBase(..))
import Brite.Syntax.Range
import Brite.Syntax.Snippet
import Data.Map.Strict (Map)

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
  -- `42`
  | IntegerConstant IntegerBase Integer

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

  -- `{p: E}`
  | ObjectExpression (Map Identifier [(Range, Expression)]) (Maybe Expression)

  -- `E.p`
  | PropertyExpression Expression Name

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

-- Gets a snippet for a constant.
constantSnippet :: Constant -> ConstantSnippet
constantSnippet VoidConstant = VoidConstantSnippet
constantSnippet (BooleanConstant value) = BooleanConstantSnippet value
constantSnippet (IntegerConstant base value) = IntegerConstantSnippet base value

-- Gets a snippet for an expression.
expressionSnippet :: Expression -> ExpressionSnippet
expressionSnippet expression = case expressionNode expression of
  ConstantExpression constant -> ConstantExpressionSnippet (constantSnippet constant)
  VariableExpression name -> VariableExpressionSnippet name
  FunctionExpression parameter _ -> FunctionExpressionSnippet (patternSnippet parameter)

  CallExpression callee arguments ->
    CallExpressionSnippet (expressionSnippet callee) (listSnippet expressionSnippet arguments)

  ObjectExpression properties extension ->
    ObjectExpressionSnippet
      (listSnippet fst (objectPropertyList properties))
      (expressionSnippet <$> extension)

  PropertyExpression object property ->
    PropertyExpressionSnippet (expressionSnippet object) (nameIdentifier property)

  ConditionalExpression test _ _ -> ConditionalExpressionSnippet (expressionSnippet test)
  BlockExpression _ -> BlockExpressionSnippet
  WrappedExpression wrapped _ -> expressionSnippet wrapped
  ErrorExpression _ (Just recovered) -> expressionSnippet (expression { expressionNode = recovered })
  ErrorExpression _ Nothing -> ErrorExpressionSnippet

-- Gets a snippet for a pattern.
patternSnippet :: Pattern -> PatternSnippet
patternSnippet pattern = case patternNode pattern of
  VariablePattern name -> VariablePatternSnippet name
