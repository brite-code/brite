module Brite.AST
  ( Statement(..)
  , Expression(..)
  , Pattern(..)
  ) where

import Brite.Source

data Statement
  = BindingStatement Pattern Expression

data Expression
  = VariableExpression Identifier

data Pattern
  = VariablePattern Pattern
