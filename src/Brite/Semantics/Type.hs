module Brite.Semantics.Type
  ( Monotype
  , MonotypeDescription(..)
  , monotypeDescription
  , Polytype
  , PolytypeDescription(..)
  , polytypeDescription
  , booleanMonotype
  , boolean
  , integerMonotype
  , integer
  , function
  , polytype
  , QuantifierBoundFlexibility(..)
  ) where

import Brite.Semantics.AST (Name, QuantifierBoundFlexibility)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

newtype TypeVariableID = TypeVariableID Int

-- Types that do not contain quantifiers.
data Monotype = Monotype
  -- The free variables in this monotype.
  { monotypeFreeVariables :: IntSet
  -- The representation of this monotype.
  , monotypeDescription :: MonotypeDescription
  }

data MonotypeDescription
  -- `T`
  --
  -- The variable referenced by this monotype.
  = Variable TypeVariableID

  -- `Bool`, `Int`
  --
  -- Primitive types with an arity of zero. Like booleans and numbers.
  | Boolean
  | Integer

  -- `fun(T) -> T`
  --
  -- A function from one type to another.
  --
  -- TODO: Currently we only allow exactly one function argument, but Brite supports any number of
  -- function arguments.
  | Function Monotype Monotype

-- Types that do contain quantifiers. When we refer to a “type” what we really mean is “polytype”.
data Polytype = Polytype
  -- Is this polytype in normal form?
  { polytypeNormal :: Bool
  -- The free variables in this polytype.
  , polytypeFreeVariables :: IntSet
  -- The representation of this polytype.
  , polytypeDescription :: PolytypeDescription
  }

data PolytypeDescription
  -- All monotypes are also polytypes. We call this constructor `Monotype_` so that it doesn’t
  -- conflict with the constructor for the `Monotype` data type.
  = Monotype_ Monotype

  -- `!`
  --
  -- The type which no values inhabit. Also known as the empty set.
  --
  -- In academic literature the bottom type is written as `⊥`.
  | Bottom

  -- `<T> T`
  --
  -- Universally quantifies a polytype.
  --
  -- In academic literature we write `∀(a = T1).T2` and `∀(a ≥ T1).T2` for rigid and flexible
  -- universal quantification respectively. We also write `∀a.T` as shorthand for `∀(a ≥ ⊥).T`.
  | Quantify Quantifier Polytype

-- A universal quantifier.
data Quantifier = Quantifier
  -- The unique identifier for this quantifier. No other quantifier will share the same ID.
  { quantifierID :: TypeVariableID
  -- The name given to this quantifier in source code. Optional since this quantifier might have
  -- been added during type inference.
  , quantifierName :: Maybe Name
  -- The flexibility of the quantifier’s bound.
  , quantifierBoundFlexibility :: QuantifierBoundFlexibility
  -- The bound of our quantifier.
  , quantifierBound :: Polytype
  }

-- A boolean monotype.
booleanMonotype :: Monotype
booleanMonotype =
  Monotype
    { monotypeFreeVariables = IntSet.empty
    , monotypeDescription = Boolean
    }

-- A boolean polytype.
boolean :: Polytype
boolean = polytype booleanMonotype

-- An integer monotype.
integerMonotype :: Monotype
integerMonotype =
  Monotype
    { monotypeFreeVariables = IntSet.empty
    , monotypeDescription = Integer
    }

-- An integer polytype.
integer :: Polytype
integer = polytype integerMonotype

-- Creates a new function monotype.
function :: Monotype -> Monotype -> Monotype
function parameter body =
  Monotype
    { monotypeFreeVariables =
        IntSet.union (monotypeFreeVariables parameter) (monotypeFreeVariables body)
    , monotypeDescription = Function parameter body
    }

-- Converts a monotype into a polytype.
polytype :: Monotype -> Polytype
polytype t =
  Polytype
    { polytypeNormal = True
    , polytypeFreeVariables = monotypeFreeVariables t
    , polytypeDescription = Monotype_ t
    }
