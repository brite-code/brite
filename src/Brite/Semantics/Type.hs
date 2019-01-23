-- This module is intended to be imported qualified:
--
-- ```hs
-- import Brite.Semantics.Type (Monotype, Polytype)
-- import qualified Brite.Semantics.Type as Type
-- ```

module Brite.Semantics.Type
  ( Monotype
  , MonotypeDescription(..)
  , monotypeDescription
  , Polytype
  , PolytypeDescription(..)
  , polytypeDescription
  , Binding(..)
  , BindingFlexibility(..)
  , variable
  , boolean
  , integer
  , function
  , polytype
  , bottom
  , normal
  ) where

import Brite.Semantics.AST (Range, Name)
import Brite.Semantics.CheckMonad (TypeVariableID, typeVariableID)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)

-- Types that do not contain quantifiers.
data Monotype = Monotype
  -- The range at which this monotype was defined. This could be the location of a type declaration
  -- or some expression’s inferred type.
  { monotypeRange :: Range
  -- The free variables in this monotype.
  , monotypeFreeVariables :: IntSet
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
  -- Is this polytype in normal form? Use the `normal` function to convert this polytype to normal
  -- form if `False`.
  { polytypeNormal :: Bool
  -- The free variables in this polytype.
  , polytypeFreeVariables :: IntSet
  -- The representation of this polytype.
  , polytypeDescription :: PolytypeDescription
  }

data PolytypeDescription
  -- All monotypes are also polytypes. We call this constructor `Monotype'` so that it doesn’t
  -- conflict with the constructor for the `Monotype` data type.
  = Monotype' Monotype

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
  | Quantify [Binding] Monotype

-- A binding of some identifier to a type.
data Binding = Binding
  -- The unique identifier for this binding. No other binding will share the same identifier.
  { bindingID :: TypeVariableID
  -- The name given to this binding in source code. Optional since this binding might have been
  -- added during type inference.
  , bindingName :: Maybe Name
  -- The flexibility of this binding.
  , bindingFlexibility :: BindingFlexibility
  -- The type being bound.
  , bindingType :: Polytype
  }

data BindingFlexibility = Flexible | Rigid

-- Creates a type variable monotype.
variable :: Range -> TypeVariableID -> Monotype
variable range i =
  Monotype
    { monotypeRange = range
    , monotypeFreeVariables = IntSet.singleton (typeVariableID i)
    , monotypeDescription = Variable i
    }

-- A boolean monotype.
boolean :: Range -> Monotype
boolean range =
  Monotype
    { monotypeRange = range
    , monotypeFreeVariables = IntSet.empty
    , monotypeDescription = Boolean
    }

-- An integer monotype.
integer :: Range -> Monotype
integer range =
  Monotype
    { monotypeRange = range
    , monotypeFreeVariables = IntSet.empty
    , monotypeDescription = Integer
    }

-- Creates a new function monotype.
function :: Range -> Monotype -> Monotype -> Monotype
function range parameter body =
  Monotype
    { monotypeRange = range
    , monotypeFreeVariables =
        IntSet.union (monotypeFreeVariables parameter) (monotypeFreeVariables body)
    , monotypeDescription = Function parameter body
    }

-- Converts a monotype into a polytype.
polytype :: Monotype -> Polytype
polytype t =
  Polytype
    { polytypeNormal = True
    , polytypeFreeVariables = monotypeFreeVariables t
    , polytypeDescription = Monotype' t
    }

-- The bottom polytype.
bottom :: Polytype
bottom =
  Polytype
    { polytypeNormal = True
    , polytypeFreeVariables = IntSet.empty
    , polytypeDescription = Bottom
    }

-- Converts a polytype to normal form as described in Section 1.5.3 of the [MLF thesis][1].
-- Returns a referentially equal type if the type is already in normal form.
--
-- The normal form transformations are:
--
-- * `nf(∀(a x t).u) = nf(u)` when `a` is not a free type variable of `u`.
-- * `nf(∀(a x t).u) = nf(u[t/a])` when `t` is a monotype.
-- * `nf(∀(a x t).a) = nf(t)`. This rule only applies when the body is the type variable `a`!
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
normal :: Polytype -> Polytype
normal t | polytypeNormal t = t
normal t = fromMaybe t (substituteAndNormalizePolytype IntMap.empty t)

-- Applies substitutions to a polytype in addition to converting it to normal form. Returns nothing
-- if the type did not change.
substituteAndNormalizePolytype :: IntMap Monotype -> Polytype -> Maybe Polytype
substituteAndNormalizePolytype initialSubstitutions t0 = case polytypeDescription t0 of
  -- Bottom types neither need to be substituted or converted to normal form.
  Bottom -> Nothing

  -- Monotypes are always in normal form, but we may need to apply substitutions.
  Monotype' t1 ->
    case substituteMonotype initialSubstitutions t1 of
      Nothing -> Nothing
      Just t2 -> Just (t0 { polytypeDescription = Monotype' t2 })

  -- If the polytype is both already in normal form _and_ does not have any free variables which
  -- need substitution then return nothing.
  _ | polytypeNormal t0 && not (needsSubstitution initialSubstitutions (polytypeFreeVariables t0)) ->
    Nothing

  -- If we have a quantified type then we’ve got some work to do. All our normal form rewrite rules
  -- act on quantified types.
  --
  -- We know from the above pre-condition that this quantified type _must_ be changed.
  Quantify initialBindings initialBody -> Just (loop initialSubstitutions initialBindings [])
    where
      -- The first `loop` function takes all the monotype bounds and substitutes them inside the
      -- quantified type’s body and subsequent bounds. While doing so we accumulate a list of the
      -- non-monotype bounds, `bindingsRev`, after `loop` completes we will call `loopRev`.
      --
      -- The second `loopRev` function iterates through the bindings backwards whereas `loop`
      -- iterated forward through the bindings. `loopRev` builds up a set of the free variables
      -- in our normalized, quantified, type and uses that to drop any unused bindings. Also, if the
      -- quantified type’s body is a variable we will inline the binding for that variable.

      loop substitutions [] bindingsRev =
        -- Apply the substitutions to our body and call the next step, `loopRev`.
        let body = fromMaybe body (substituteMonotype substitutions initialBody) in
          loopRev (monotypeFreeVariables body) body [] bindingsRev

      loop substitutions (oldBinding : bindings) bindingsRev =
        let
          -- Convert the bound’s type to normal form if necessary.
          binding = case substituteAndNormalizePolytype substitutions (bindingType oldBinding) of
            Nothing -> oldBinding
            Just newType -> binding { bindingType = newType }
        in
          case polytypeDescription (bindingType binding) of
            -- If this binding is a monotype then we want to remove the binding and substitute it
            -- wherever the binding appears in the rest of our polytype.
            Monotype' t ->
              let newSubstitutions = IntMap.insert (typeVariableID (bindingID binding)) t substitutions in
                loop newSubstitutions bindings bindingsRev

            -- Add this binding back to the list of bindings we will use for our quantified type.
            _ -> loop substitutions bindings (binding : bindingsRev)

      -- Add the end of `loopRev` create a quantified type with our new bindings. Hooray!
      loopRev _ body [] [] = polytype body
      loopRev free body bindings [] =
        Polytype
          { polytypeNormal = True
          , polytypeFreeVariables = free
          , polytypeDescription = Quantify bindings body
          }

      -- If the body of our new quantified type is a variable that references the current binding
      -- then we want to inline that binding as the new quantified type’s body.
      loopRev _ (Monotype { monotypeDescription = Variable i }) [] (binding : bindingsRev) | i == bindingID binding =
        case polytypeDescription (bindingType binding) of
          -- If we are inlining the bottom type then return it and stop looping! We know that all
          -- other bindings will be unused because bottom will never have any free variables.
          Bottom -> bindingType binding
          -- If our binding is a monotype then inline it and continue!
          Monotype' newBody -> loopRev free newBody [] bindingsRev
          -- If our binding is a quantified type then inline the quantified type’s bindings and set
          -- the bindings list to our quantified type’s binding list. We can do this safely because
          -- all bounds have already been successfully normalized in `loop`. So we know that our
          -- quantified type is in normal form.
          Quantify newBindings newBody -> loopRev free newBody newBindings bindingsRev
        where
          free = polytypeFreeVariables (bindingType binding)

      loopRev free body bindings (binding : bindingsRev) =
        -- If our binding does not exist in our set of free variables then drop the binding
        -- as unused!
        if IntSet.member (typeVariableID (bindingID binding)) free then
          let
            -- Remove this binding from our set of free variables and add all the free variables
            -- from our binding type.
            newFree = IntSet.union
              (polytypeFreeVariables (bindingType binding))
              (IntSet.delete (typeVariableID (bindingID binding)) free)
          in
            loopRev newFree body (binding : bindings) bindingsRev
        else
          loopRev free body bindings bindingsRev

-- Substitutes the free variables of the provided monotype with a substitution if one was made
-- available in the substitutions map. Returns nothing if no substitution was made.
substituteMonotype :: IntMap Monotype -> Monotype -> Maybe Monotype
substituteMonotype substitutions t0 = case monotypeDescription t0 of
  -- Try to find a substitution for this variable.
  Variable i -> IntMap.lookup (typeVariableID i) substitutions
  -- Types which will never have substitutions.
  Boolean -> Nothing
  Integer -> Nothing
  -- If we don’t need a substitution then immediately return our type without recursing.
  _ | not (needsSubstitution substitutions (monotypeFreeVariables t0)) -> Nothing
  -- Substitute the type variables in a function type. We do this below the above condition so we
  -- won’t recurse if we don’t absolutely have to.
  Function t1 t2 -> Just (function (monotypeRange t0) (recurse t1) (recurse t2))
  where
    recurse t = fromMaybe t (substituteMonotype substitutions t)

-- Determines if a type needs some substitutions by looking at the type’s free variables. If a
-- substitution exists for any free variable then we return true.
needsSubstitution :: IntMap Monotype -> IntSet -> Bool
needsSubstitution substitutions freeVariables =
  if IntMap.null substitutions || IntSet.null freeVariables then False else
    -- NOTE: If `substitutions` is smaller then it would be faster to search through that map.
    -- Looking up the size of Haskell containers is O(n) so we don’t bother. We assume that in most
    -- cases `freeVariables` is smaller.
    --
    -- NOTE: Haskell should optimize this to a lazy form which stops on the first `True`. Should we
    -- test this assumption?
    IntSet.foldr (\i yes -> yes || IntMap.member i substitutions) False freeVariables
