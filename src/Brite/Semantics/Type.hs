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
  , monotypeRange
  , monotypeFreeVariables
  , Polytype
  , PolytypeDescription(..)
  , polytypeDescription
  , polytypeRange
  , polytypeFreeVariables
  , polytypeNormal
  , Binding(..)
  , Flexibility(..)
  , isUnboundBinding
  , variable
  , boolean
  , integer
  , function
  , polytype
  , bottom
  , quantify
  , Polarity(..)
  , normal
  , substitutePolytype
  , substituteMonotype
  ) where

import Brite.Semantics.AST (Range, Identifier, Flexibility(..))
import Brite.Semantics.Namer
import Data.Foldable (any)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- Types that do not contain quantifiers.
data Monotype = Monotype
  -- The range at which our monotype was defined in source code. The range could be the position of
  -- a type or the position of an expression or pattern that comes with an associated type.
  --
  -- Remember that the range of a variable is where the _variable_ was defined. Not the range of the
  -- variable’s binding.
  { monotypeRange :: Range
  -- The free variables in this monotype.
  --
  -- NOTE: Order does matter so we use a `Set` instead of a `HashSet`. Also, from a cursory glance
  -- it looks like `Set` has a better algorithmic complexity for the `union` operation which is very
  -- common for our types.
  , monotypeFreeVariables :: Set Identifier
  -- The representation of this monotype.
  , monotypeDescription :: MonotypeDescription
  }

data MonotypeDescription
  -- `T`
  --
  -- The variable referenced by this monotype.
  = Variable Identifier

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
  --
  -- NOTE: Order does matter so we use a `Set` instead of a `HashSet`. Also, from a cursory glance
  -- it looks like `Set` has a better algorithmic complexity for the `union` operation which is very
  -- common for our types.
  , polytypeFreeVariables :: Set Identifier
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
  | Bottom Range

  -- `<T> T`
  --
  -- Universally quantifies a polytype.
  --
  -- In academic literature we write `∀(a = T1).T2` and `∀(a ≥ T1).T2` for rigid and flexible
  -- universal quantification respectively. We also write `∀a.T` as shorthand for `∀(a ≥ ⊥).T`.
  | Quantify [Binding] Monotype

-- The range at which our polytype was defined in source code. If the polytype is a quantified type
-- then the range does _not_ include the bindings. Since bindings are often invented by our
-- type checker.
polytypeRange :: Polytype -> Range
polytypeRange type0 = case polytypeDescription type0 of
  Monotype' type1 -> monotypeRange type1
  Bottom range -> range
  Quantify _ type1 -> monotypeRange type1

-- A binding of some identifier to a type.
data Binding = Binding
  -- The name given to this binding in source code. Optional since this binding might have been
  -- added during type inference.
  { bindingName :: Identifier
  -- The flexibility of this binding.
  , bindingFlexibility :: Flexibility
  -- The type being bound.
  , bindingType :: Polytype
  }

-- Is this binding unbound? That is, does it have a flexible bottom bound? In syntax `<T: !>` or
-- academic syntax `∀(a ≥ ⊥)`. We abbreviate these bindings to `<T>` and `∀a` in their
-- respective syntaxes.
isUnboundBinding :: Binding -> Bool
isUnboundBinding (Binding _ Flexible (Polytype { polytypeDescription = Bottom _ })) = True
isUnboundBinding _ = False

-- Creates a type variable monotype.
variable :: Range -> Identifier -> Monotype
variable range identifier =
  Monotype
    { monotypeRange = range
    , monotypeFreeVariables = Set.singleton identifier
    , monotypeDescription = Variable identifier
    }

-- A boolean monotype.
boolean :: Range -> Monotype
boolean range =
  Monotype
    { monotypeRange = range
    , monotypeFreeVariables = Set.empty
    , monotypeDescription = Boolean
    }

-- An integer monotype.
integer :: Range -> Monotype
integer range =
  Monotype
    { monotypeRange = range
    , monotypeFreeVariables = Set.empty
    , monotypeDescription = Integer
    }

-- Creates a new function monotype.
function :: Range -> Monotype -> Monotype -> Monotype
function range parameter body =
  Monotype
    { monotypeRange = range
    , monotypeFreeVariables = Set.union (monotypeFreeVariables parameter) (monotypeFreeVariables body)
    , monotypeDescription = Function parameter body
    }

-- Converts a monotype into a polytype.
polytype :: Monotype -> Polytype
polytype type' =
  Polytype
    { polytypeNormal = True
    , polytypeFreeVariables = monotypeFreeVariables type'
    , polytypeDescription = Monotype' type'
    }

-- The bottom polytype.
bottom :: Range -> Polytype
bottom range =
  Polytype
    { polytypeNormal = True
    , polytypeFreeVariables = Set.empty
    , polytypeDescription = Bottom range
    }

-- Quantifies a monotype with some bindings. If an empty list was provided then we return the
-- monotype converted to a polytype.
--
-- We will not add the free variables of unused bounds to the free variables of our polytype. Since
-- in normal form these bounds will be dropped.
quantify :: [Binding] -> Monotype -> Polytype
quantify [] body = polytype body
quantify bindings body =
  Polytype
    { polytypeNormal = False
    , polytypeFreeVariables =
        foldr
          (\binding free ->
            if not (Set.member (bindingName binding) free) then free else
              Set.union
                (polytypeFreeVariables (bindingType binding))
                (Set.delete (bindingName binding) free))
          (monotypeFreeVariables body)
          bindings
    , polytypeDescription = Quantify bindings body
    }

-- Polarity represents whether a type is in an “input” or an “output” position. You may also know
-- polarity as “variance”.
--
-- Consider the type `fun(T) -> U`. Here `T` is in an _input_ position so we give it a negative
-- polarity. However, `U` is in an _output_ position so we give it a positive polarity.
--
-- A negative position in a negative position is a positive position. Consider the type
-- `fun(fun(T) -> U) -> V`. Here, like before, `V` has a positive polarity. `U` has a negative
-- polarity because it it is in a negative position (function parameter) and a positive position
-- (function return) so a negative times a positive is a negative. `T`, however, has a positive
-- polarity because it is in a negative position (function parameter) and a negative position again
-- (function parameter of a function parameter) so a negative times a negative is a positive.
data Polarity = Positive | Negative

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
normal t = fromMaybe t (substituteAndNormalizePolytype HashSet.empty HashMap.empty t)

-- Applies substitutions to a polytype in addition to converting it to normal form. Returns nothing
-- if the type did not change.
substituteAndNormalizePolytype :: HashSet Identifier -> HashMap Identifier (Range -> Monotype) -> Polytype -> Maybe Polytype
substituteAndNormalizePolytype initialSeen initialSubstitutions t0 = case polytypeDescription t0 of
  -- Bottom types neither need to be substituted or converted to normal form.
  Bottom _ -> Nothing

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
  Quantify initialBindings initialBody ->
    Just (loop initialSeen Set.empty initialSubstitutions initialBindings [])
    where
      -- The first `loop` function takes all the monotype bounds and substitutes them inside the
      -- quantified type’s body and subsequent bounds. While doing so we accumulate a list of the
      -- non-monotype bounds, `bindingsRev`, after `loop` completes we will call `loopRev`.
      --
      -- The `loop` function must take care that while inlining it does not break any references.
      -- For example in the type `<T: U, U> T` we have `T` which references an unbound `U`. A naïve
      -- inlining of `T` would result in `<U> U` which is incorrect since `U` is unbound!
      --
      -- We use two sets to keep track of names. `seen` and `captured`. `seen` is a set of all the
      -- variables names that have not been inlined. `captured` is a set of all the free variables
      -- in types that will be inlined.
      --
      -- The second `loopRev` function iterates through the bindings backwards whereas `loop`
      -- iterated forward through the bindings. `loopRev` builds up a set of the free variables
      -- in our normalized, quantified, type and uses that to drop any unused bindings. Also, if the
      -- quantified type’s body is a variable we will inline the binding for that variable.

      loop :: HashSet Identifier -> Set Identifier -> HashMap Identifier (Range -> Monotype) -> [Binding] -> [Binding] -> Polytype

      loop _ _ substitutions [] bindingsRev =
        -- Apply the substitutions to our body and call the next step, `loopRev`.
        let body = fromMaybe initialBody (substituteMonotype substitutions initialBody) in
          loopRev (monotypeFreeVariables body) body [] bindingsRev

      loop seen captured substitutions (oldBinding : bindings) bindingsRev =
        let
          -- Convert the bound’s type to normal form if necessary.
          binding = case substituteAndNormalizePolytype seen substitutions (bindingType oldBinding) of
            Nothing -> oldBinding
            Just newType -> oldBinding { bindingType = newType }
        in
          case polytypeDescription (bindingType binding) of
            -- If this binding is a monotype then we want to remove the binding and substitute it
            -- wherever the binding appears in the rest of our polytype.
            Monotype' t ->
              let
                newSubstitutions = HashMap.insert (bindingName binding) (const t) substitutions
                newCaptured = Set.union (monotypeFreeVariables t) captured
              in
                loop seen newCaptured newSubstitutions bindings bindingsRev

            _ ->
              -- If this binding has a captured name (a name that is free in `substitutions`) then
              -- we need to generate a new name for this binding.
              if Set.member (bindingName binding) captured then
                let
                  -- Generate a new name for our binding that is unique. We can’t use a name that
                  -- we’ve already seen since that name might be used at some future point.
                  newName =
                    uniqueName
                      (\testName -> HashSet.member testName seen)
                      (bindingName binding)

                  newBinding = binding { bindingName = newName }

                  -- Add a substitution for the old name to the new name. This substitution won’t
                  -- be applied to our captured variable names.
                  --
                  -- If there was an old substitution in the map for this name then we will
                  -- replace it.
                  newSubstitutions =
                    HashMap.insert (bindingName binding) (\r -> variable r newName) substitutions

                  -- Insert our new name into both `seen` because our quantifier list now includes
                  -- this binding. Insert our new name into `captured` because the name was
                  -- “captured” in our substitutions map.
                  newSeen = HashSet.insert newName seen
                  newCaptured = Set.insert newName captured
                in
                  loop newSeen newCaptured newSubstitutions bindings (newBinding : bindingsRev)

              -- Otherwise, we can use the binding’s name.
              else
                let
                  -- Remove any substitutions for shadowed types.
                  newSubstitutions = HashMap.delete (bindingName binding) substitutions
                  newSeen = HashSet.insert (bindingName binding) seen
                in
                  loop newSeen captured newSubstitutions bindings (binding : bindingsRev)

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
      loopRev _ (Monotype { monotypeDescription = Variable name }) [] (binding : bindingsRev)
        | name == bindingName binding =
          case polytypeDescription (bindingType binding) of
            -- If we are inlining the bottom type then return it and stop looping! We know that all
            -- other bindings will be unused because bottom will never have any free variables.
            Bottom _ -> bindingType binding
            -- If our binding is a monotype then inline it and continue!
            Monotype' newBody -> loopRev free newBody [] bindingsRev
            -- If our binding is a quantified type then inline the quantified type’s bindings and
            -- set the bindings list to our quantified type’s binding list. We can do this safely
            -- because all bounds have already been successfully normalized in `loop`. So we know
            -- that our quantified type is in normal form.
            Quantify newBindings newBody -> loopRev free newBody newBindings bindingsRev
          where
            free = polytypeFreeVariables (bindingType binding)

      loopRev free body bindings (binding : bindingsRev) =
        -- If our binding does not exist in our set of free variables then drop the binding
        -- as unused!
        if Set.member (bindingName binding) free then
          let
            -- Remove this binding from our set of free variables and add all the free variables
            -- from our binding type.
            newFree = Set.union
              (polytypeFreeVariables (bindingType binding))
              (Set.delete (bindingName binding) free)
          in
            loopRev newFree body (binding : bindings) bindingsRev
        else
          loopRev free body bindings bindingsRev

-- Substitutes the free variables of the provided polytype with a substitution if one was made
-- available in the substitutions map. Returns nothing if no substitution was made.
substitutePolytype :: HashMap Identifier (Range -> Monotype) -> Polytype -> Maybe Polytype
substitutePolytype substitutions0 t0 = case polytypeDescription t0 of
  -- Substitute a monotype and update our polytype if there was a substitution.
  Monotype' t1 -> case substituteMonotype substitutions0 t1 of
    Nothing -> Nothing
    Just t2 -> Just (t0 { polytypeDescription = Monotype' t2 })

  -- There are never any substitutions in bottom types.
  Bottom _ -> Nothing

  -- If the polytype does not need substitution then return nothing. Otherwise we assume that the
  -- polytype will need some substitution.
  _ | not (needsSubstitution substitutions0 (polytypeFreeVariables t0)) -> Nothing

  -- Assumes that the quantified type needs substitution. Iterates through all the bindings. If a
  -- binding shadows a variable which was substituted then we need to remove it from the
  -- substitutions map.
  Quantify initialBindings body ->
    Just (t0 { polytypeDescription = uncurry Quantify (loop substitutions0 initialBindings) })
    where
      loop substitutions1 [] = ([], fromMaybe body (substituteMonotype substitutions1 body))
      loop substitutions1 (binding : bindings) =
        let
          newBinding = case substitutePolytype substitutions1 (bindingType binding) of
            Nothing -> binding
            Just newType -> binding { bindingType = newType }

          (newBindings, newBody) =
            loop (HashMap.delete (bindingName binding) substitutions1) bindings
        in
          (newBinding : newBindings, newBody)

-- Substitutes the free variables of the provided monotype with a substitution if one was made
-- available in the substitutions map. Returns nothing if no substitution was made.
substituteMonotype :: HashMap Identifier (Range -> Monotype) -> Monotype -> Maybe Monotype
substituteMonotype substitutions t0 = case monotypeDescription t0 of
  -- Try to find a substitution for this variable.
  Variable name -> ($ monotypeRange t0) <$> HashMap.lookup name substitutions
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
needsSubstitution :: HashMap Identifier a -> Set Identifier -> Bool
needsSubstitution substitutions freeVariables =
  if HashMap.null substitutions || Set.null freeVariables then False else
    -- NOTE: If `substitutions` is smaller then it would be faster to search through that map.
    -- Looking up the size of Haskell containers is O(n) so we don’t bother. We assume that in most
    -- cases `freeVariables` is smaller.
    any (\name -> HashMap.member name substitutions) freeVariables
