-- This module is intended to be imported qualified:
--
-- ```hs
-- import Brite.Semantics.Type (Monotype, Polytype)
-- import qualified Brite.Semantics.Type as Type
-- ```

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Brite.Semantics.Type
  ( Monotype
  , MonotypeDescription(..)
  , monotypeDescription
  , monotypeRangeStack
  , monotypeFreeVariables
  , Polytype
  , PolytypeDescription(..)
  , polytypeDescription
  , polytypeRangeStack
  , polytypeFreeVariables
  , polytypeNormal
  , Quantifier(..)
  , Flexibility(..)
  , quantifierBoundType
  , quantifierFreeVariables
  , variable
  , variableWithRangeStack
  , construct
  , void
  , boolean
  , integer
  , function
  , object
  , emptyObject
  , polytype
  , bottom
  , quantify
  , reposition
  , repositionMonotype
  , normal
  , substitutePolytype
  , substituteMonotype
  ) where

import Brite.Semantics.AST (Flexibility(..))
import Brite.Semantics.Namer
import Brite.Semantics.TypeConstruct
import Brite.Syntax.Identifier (Identifier)
import Brite.Syntax.Range
import Data.Foldable (any)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (mapAccumL)

-- Types that do not contain quantifiers.
data Monotype = Monotype
  -- The range at which our monotype was defined in source code. The range could be the position of
  -- a type or the position of an expression or pattern that comes with an associated type.
  --
  -- Remember that the range of a variable is where the _variable_ was defined. Not the range of the
  -- variable’s quantifier.
  { monotypeRangeStack :: RangeStack
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

  -- `void`, `fun(T) -> U` `Bool`, `Int`
  --
  -- Some constructed type. Like a function or an integer.
  | Construct (Construct Monotype)

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
  | Bottom RangeStack

  -- `<T> T`
  --
  -- Universally quantifies a polytype.
  --
  -- In academic literature we write `∀(a = T1).T2` and `∀(a ≥ T1).T2` for rigid and flexible
  -- universal quantification respectively. We also write `∀a.T` as shorthand for `∀(a ≥ ⊥).T`.
  --
  -- We use a sequence (`Seq`) of quantifiers instead of a list (`[]`) because we want to have
  -- efficient append to the end of the list and efficient concatenation.
  | Quantify (Seq (Identifier, Quantifier)) Monotype

-- The range at which our polytype was defined in source code. If the polytype is a quantified type
-- then the range does _not_ include the quantifiers. Since quantifiers are often invented by our
-- type checker.
polytypeRangeStack :: Polytype -> RangeStack
polytypeRangeStack type0 = case polytypeDescription type0 of
  Monotype' type1 -> monotypeRangeStack type1
  Bottom rangeStack -> rangeStack
  Quantify _ type1 -> monotypeRangeStack type1

-- Brite supports first-class quantification with an [MLF][1] based type system extended with
-- [existential types][2]. This means Brite supports arbitrary usage of the two most common
-- [logical quantifiers][3] “for all” (universal quantifier, ∀) and “there exists” (existential
-- quantifier, ∃).
--
-- This data type represents a single quantifier. Either universal or existential.
--
-- This data type does not contain the associated name being quantified.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
-- [2]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.120.5025&rep=rep1&type=pdf
-- [3]: https://en.wikipedia.org/wiki/Quantifier_(logic)
data Quantifier
  -- A universal quantifier (∀) always has a polytype bound which is either a flexible or
  -- rigid bound.
  = UniversalQuantifier Flexibility Polytype

  -- An existential quantifier (∃) has no bound. Implicitly, the most polymorphic type for an
  -- existential quantifier is the bottom type. Existential quantifiers carry around a range so we
  -- can point to where the existential quantifier was defined.
  | ExistentialQuantifier Range

-- Returns the bound type of our quantifier disregarding the flexibility. You can think of the bound
-- as the most polymorphic type possible for this quantifier where bottom (⊥) is the most
-- polymorphic type.
quantifierBoundType :: Quantifier -> Polytype
quantifierBoundType (UniversalQuantifier _ t) = t
quantifierBoundType (ExistentialQuantifier r) = bottom r

-- All of the free variables in a quantifier.
quantifierFreeVariables :: Quantifier -> Set Identifier
quantifierFreeVariables (UniversalQuantifier _ t) = polytypeFreeVariables t
quantifierFreeVariables (ExistentialQuantifier _) = Set.empty

-- Creates a type variable monotype.
variable :: Range -> Identifier -> Monotype
variable range = variableWithRangeStack (singletonRange range)

-- Creates a type variable monotype.
variableWithRangeStack :: RangeStack -> Identifier -> Monotype
variableWithRangeStack rangeStack identifier =
  Monotype
    { monotypeRangeStack = rangeStack
    , monotypeFreeVariables = Set.singleton identifier
    , monotypeDescription = Variable identifier
    }

-- Creates a constructed monotype.
construct :: Range -> Construct Monotype -> Monotype
construct range = constructWithRangeStack (singletonRange range)

-- Creates a constructed monotype.
constructWithRangeStack :: RangeStack -> Construct Monotype -> Monotype
constructWithRangeStack rangeStack c =
  Monotype
    { monotypeRangeStack = rangeStack
    , monotypeFreeVariables = Set.unions (monotypeFreeVariables <$> c)
    , monotypeDescription = Construct c
    }

-- A void monotype.
void :: Range -> Monotype
void range = construct range Void

-- A boolean monotype.
boolean :: Range -> Monotype
boolean range = construct range Boolean

-- An integer monotype.
integer :: Range -> Monotype
integer range = construct range Integer

-- Creates a new function monotype.
function :: Range -> Monotype -> Monotype -> Monotype
function range parameter body = construct range (Function parameter body)

-- Creates a new object monotype.
object :: Range -> Map Identifier [(Range, Monotype)] -> Maybe Monotype -> Monotype
object range properties extension = construct range (Object properties extension)

-- Creates an empty object with a range stack.
emptyObject :: Range -> Monotype
emptyObject range = construct range (Object Map.empty Nothing)

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
    , polytypeDescription = Bottom (singletonRange range)
    }

-- Quantifies a monotype with some quantifiers. If an empty list was provided then we return the
-- monotype converted to a polytype.
--
-- We will not add the free variables of unused bounds to the free variables of our polytype. Since
-- in normal form these bounds will be dropped.
quantify :: Seq (Identifier, Quantifier) -> Monotype -> Polytype
quantify quantifiers body = if Seq.null quantifiers then polytype body else
  Polytype
    { polytypeNormal = False
    , polytypeFreeVariables =
        foldr
          (\(name, quantifier) free ->
            if not (Set.member name free) then free else
              Set.union (quantifierFreeVariables quantifier) (Set.delete name free))
          (monotypeFreeVariables body)
          quantifiers
    , polytypeDescription = Quantify quantifiers body
    }

-- Pushes a range to the polytype’s range stack.
reposition :: Range -> Polytype -> Polytype
reposition r t0 =
  t0
    { polytypeDescription =
        case polytypeDescription t0 of
          Monotype' t1 -> Monotype' (repositionMonotype r t1)
          Bottom rs -> Bottom (pushRange r rs)
          Quantify qs t1 -> Quantify qs (repositionMonotype r t1)
    }

-- Pushes a range to the monotype’s range stack.
repositionMonotype :: Range -> Monotype -> Monotype
repositionMonotype r t = t { monotypeRangeStack = pushRange r (monotypeRangeStack t) }

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
substituteAndNormalizePolytype :: HashSet Identifier -> HashMap Identifier (Either Identifier Monotype) -> Polytype -> Maybe Polytype
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
      -- non-monotype bounds, `quantifiersRev`, after `loop` completes we will call `loopRev`.
      --
      -- The `loop` function must take care that while inlining it does not break any references.
      -- For example in the type `<T: U, U> T` we have `T` which references an unbound `U`. A naïve
      -- inlining of `T` would result in `<U> U` which is incorrect since `U` is unbound!
      --
      -- We use two sets to keep track of names. `seen` and `captured`. `seen` is a set of all the
      -- variables names that have not been inlined. `captured` is a set of all the free variables
      -- in types that will be inlined.
      --
      -- The second `loopRev` function iterates through the quantifiers backwards whereas `loop`
      -- iterated forward through the quantifiers. `loopRev` builds up a set of the free variables
      -- in our normalized, quantified, type and uses that to drop any unused quantifiers. Also, if
      -- the quantified type’s body is a variable we will inline the quantifier for that variable.

      loop :: HashSet Identifier -> Set Identifier -> HashMap Identifier (Either Identifier Monotype) -> Seq (Identifier, Quantifier) -> [(Identifier, Quantifier)] -> Polytype

      loop _ _ substitutions Empty quantifiersRev =
        -- Apply the substitutions to our body and call the next step, `loopRev`.
        let body = fromMaybe initialBody (substituteMonotype substitutions initialBody) in
          loopRev (monotypeFreeVariables body) body Empty quantifiersRev

      loop seen captured substitutions (oldEntry@(oldName, oldQuantifier) :<| quantifiers) quantifiersRev =
        let
          -- Convert the bound’s type to normal form if necessary.
          entry@(name, quantifier) = case oldQuantifier of
            ExistentialQuantifier _ -> oldEntry
            UniversalQuantifier f t ->
              maybe oldEntry ((,) oldName . UniversalQuantifier f) (substituteAndNormalizePolytype seen substitutions t)
        in
          case quantifier of
            -- If this quantifier is a universal quantifier with a monotype bound then we want to
            -- remove the quantifier and substitute it wherever the quantifier is referenced in the
            -- rest of our polytype.
            UniversalQuantifier _ (polytypeDescription -> Monotype' t) ->
              let
                newSubstitutions = HashMap.insert name (Right t) substitutions
                newCaptured = Set.union (monotypeFreeVariables t) captured
              in
                loop seen newCaptured newSubstitutions quantifiers quantifiersRev

            _ ->
              -- If this quantifier has a captured name (a name that is free in `substitutions`)
              -- then we need to generate a new name for this quantifier.
              if Set.member name captured then
                let
                  -- Generate a new name for our quantifier that is unique. We can’t use a name that
                  -- we’ve already seen since that name might be used at some future point.
                  newName = uniqueName (\testName -> HashSet.member testName seen) name

                  -- Add a substitution for the old name to the new name. This substitution won’t
                  -- be applied to our captured variable names.
                  --
                  -- If there was an old substitution in the map for this name then we will
                  -- replace it.
                  newSubstitutions = HashMap.insert name (Left newName) substitutions

                  -- Insert our new name into both `seen` because our quantifier list now includes
                  -- this quantifier. Insert our new name into `captured` because the name was
                  -- “captured” in our substitutions map.
                  newSeen = HashSet.insert newName seen
                  newCaptured = Set.insert newName captured
                in
                  loop newSeen newCaptured newSubstitutions quantifiers ((newName, quantifier) : quantifiersRev)

              -- Otherwise, we can use the binding’s name.
              else
                let
                  -- Remove any substitutions for shadowed types.
                  newSubstitutions = HashMap.delete name substitutions
                  newSeen = HashSet.insert name seen
                in
                  loop newSeen captured newSubstitutions quantifiers (entry : quantifiersRev)

      loopRev :: Set Identifier -> Monotype -> Seq (Identifier, Quantifier) -> [(Identifier, Quantifier)] -> Polytype

      -- Add the end of `loopRev` create a quantified type with our new quantifiers. Hooray!
      loopRev _ body Empty [] = polytype body
      loopRev free body quantifiers [] =
        Polytype
          { polytypeNormal = True
          , polytypeFreeVariables = free
          , polytypeDescription = Quantify quantifiers body
          }

      -- If the body of our new quantified type is a variable that references the current universal
      -- quantifier then we want to inline that quantifier as the new quantified type’s body.
      loopRev _ (Monotype { monotypeDescription = Variable name1 }) Empty ((name2, UniversalQuantifier _ t) : quantifiersRev) | name1 == name2 =
          case polytypeDescription t of
            -- If we are inlining the bottom type then return it and stop looping! We know that all
            -- other quantifiers will be unused because bottom will never have any free variables.
            Bottom _ -> t
            -- If our quantifier is a monotype then inline it and continue!
            Monotype' newBody -> loopRev free newBody Seq.empty quantifiersRev
            -- If our bound is a quantified type then inline the quantified type’s quantifiers
            -- and set the quantifiers list to our quantified type’s quantifier list. We can do this
            -- safely  because all bounds have already been successfully normalized in `loop`. So we
            -- know that our quantified type is in normal form.
            Quantify newQuantifiers newBody -> loopRev free newBody newQuantifiers quantifiersRev
          where
            free = polytypeFreeVariables t

      loopRev free body quantifiers ((name, quantifier) : quantifiersRev) =
        -- If our quantifier does not exist in our set of free variables then drop the quantifier
        -- as unused!
        if Set.member name free then
          let
            -- Remove this quantifier from our set of free variables and add all the free variables
            -- from our quantifier type.
            newFree = Set.union (quantifierFreeVariables quantifier) (Set.delete name free)
          in
            loopRev newFree body ((name, quantifier) <| quantifiers) quantifiersRev
        else
          loopRev free body quantifiers quantifiersRev

-- Substitutes the free variables of the provided polytype with a substitution if one was made
-- available in the substitutions map. Returns nothing if no substitution was made.
substitutePolytype :: HashMap Identifier (Either Identifier Monotype) -> Polytype -> Maybe Polytype
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

  -- Assumes that the quantified type needs substitution. Iterates through all the quantifiers. If a
  -- quantifier shadows a variable which was substituted then we need to remove it from the
  -- substitutions map.
  Quantify quantifiers body ->
    let
      (substitutions3, newBindings) =
        mapAccumL
          (\substitutions1 entry@(name, quantifier) ->
            let
              newEntry = case quantifier of
                ExistentialQuantifier _ -> entry
                UniversalQuantifier f t ->
                  maybe entry ((,) name . UniversalQuantifier f) (substitutePolytype substitutions1 t)

              substitutions2 = HashMap.delete name substitutions1
            in
              (substitutions2, newEntry))
          substitutions0
          quantifiers

      newBody = fromMaybe body (substituteMonotype substitutions3 body)
    in
      Just (t0 { polytypeDescription = Quantify newBindings newBody })

-- Substitutes the free variables of the provided monotype with a substitution if one was made
-- available in the substitutions map. Returns nothing if no substitution was made.
substituteMonotype :: HashMap Identifier (Either Identifier Monotype) -> Monotype -> Maybe Monotype
substituteMonotype substitutions t0 = case monotypeDescription t0 of
  -- Try to find a substitution for this variable.
  Variable name ->
    case HashMap.lookup name substitutions of
      Nothing -> Nothing
      Just (Left newName) -> Just (variableWithRangeStack (monotypeRangeStack t0) newName)
      Just (Right newType) -> Just newType

  -- If we don’t need a substitution then immediately return our type without recursing.
  _ | not (needsSubstitution substitutions (monotypeFreeVariables t0)) -> Nothing

  -- Substitute the type variables in a constructed type. We do this below the above condition so we
  -- won’t recurse if we don’t absolutely have to.
  Construct c -> Just $
    constructWithRangeStack
      (monotypeRangeStack t0)
      (fmap (\t -> fromMaybe t (substituteMonotype substitutions t)) c)

-- Determines if a type needs some substitutions by looking at the type’s free variables. If a
-- substitution exists for any free variable then we return true.
needsSubstitution :: HashMap Identifier a -> Set Identifier -> Bool
needsSubstitution substitutions freeVariables =
  if HashMap.null substitutions || Set.null freeVariables then False else
    -- NOTE: If `substitutions` is smaller then it would be faster to search through that map.
    -- Looking up the size of Haskell containers is O(n) so we don’t bother. We assume that in most
    -- cases `freeVariables` is smaller.
    any (\name -> HashMap.member name substitutions) freeVariables
