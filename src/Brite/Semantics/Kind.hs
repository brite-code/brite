-- We have a small kind system on top of our type system. The kind system exists to prevent
-- mal-formed types. Particularly when it comes to records. The type `{p: Bool | Int}` is invalid
-- because `Int` is not an object.
--
-- Unlike other kind systems, ours features subtyping. This allows us to have objects which have an
-- object kind but also a value kind because the object kind is a subtype of the value kind.

module Brite.Semantics.Kind () where

import Control.Monad.ST
import Data.STRef

-- data PositiveKind
--   = PositiveValue
--   | PositiveObjectValue

-- data NegativeKind
--   = NegativeValue
--   | NegativeObjectValue

-- subtype :: PositiveKind -> NegativeKind -> Bool
-- subtype kind1 kind2 = case (kind1, kind2) of
--   (PositiveValue, NegativeValue) -> True

--   (PositiveObjectValue, NegativeObjectValue) -> True
--   (PositiveObjectValue, NegativeValue) -> True
--   (_, PositiveObjectValue) -> False


data Kind s
  -- = Unknown (STRef s (UnknownKind s))

  -- `⊤`
  --
  -- Any type that is representable by a value at runtime. Currently all types are representable as
  -- a value at runtime so this is also our “top” type.
  = Value

  -- `{}`
  --
  -- Any type that represents an object at runtime.
  | ObjectValue

  -- `⊥`
  --
  -- The smallest possible kind.
  | Bottom

data UnknownKind s = UnknownKind s
  -- `K <: a`
  { lowerBound :: Kind s
  -- `a <: K`
  , upperBound :: Kind s
  }

-- Is the first kind a subtype of the second?
subtypeST :: Kind s -> Kind s -> ST s Bool
subtypeST kind1 kind2 = case (kind1, kind2) of
  -- An unknown kind is of course a subtype of itself.
  (Unknown unknownRef1, Unknown unknownRef2) | unknownRef1 == unknownRef2 ->
    return True

  -- (Unknown unknownRef1, Unknown unknownRef2) -> do
  --   unknown1 <- readSTRef unknownRef1
  --   unknown2 <- readSTRef unknownRef2
  --   subtypeST (unknownLowerKind unknown1) (unknownUpperKind unknown2)
  --   -- writeSTRef (unknown1 { unknownUpperKind = Just (maybe (return kind2) (\k1 -> leastUpper k1 kind2) (unknownUpperKind unknown1)) })
  --   -- writeSTRef (unknown2 { unknownLowerKind = Just (maybe (return kind1) (\k2 -> greatestLower kind1 k2) (unknownLowerKind unknown2)) })

  -- `Value` is, of course, a subtype of `Value`.
  (Value, Value) -> return True

  -- `ObjectValue` is a subtype of `Value`.
  (ObjectValue, ObjectValue) -> return True
  (ObjectValue, Value) -> return True

  -- Nothing is a subtype of `ObjectValue`.
  (_, ObjectValue) -> return False

  -- Bottom is the subtype of everything.
  (Bottom, _) -> return True
  (_, Bottom) -> return False

-- we have their least upper bound t1 ⨆ t2 (“or”, union in set theory, `T1 | T2` in TypeScript) and
-- greatest lower bound t1 ⨅ t2 (“and”, intersection in set theory, `T1 & T2` in TypeScript)

-- The least upper bound relation takes returns the shared upper bound of both kinds.
--
-- Other ways to think about this relation:
--
-- * The relation represents a kind of `K1` *or* `K2`.
-- * As the “union” operation in set theory.
-- * In TypeScript syntax: `K1 | K2`.
-- * In mathematical syntax: `K1 ⨆ K2`.
leastUpper :: Kind s -> Kind s -> ST s (Kind s)
leastUpper kind1 kind2 = case (kind1, kind2) of

  (Unknown unknownRef1, Unknown unknownRef2) ->
    error "TODO"

  (Unknown unknownRef1, _) -> do
    unknown1 <- readSTRef unknownRef1
    kind <- maybe (return kind2) (\k1 -> leastUpper k1 kind2) (unknownUpperKind unknown1)
    writeSTRef unknownRef1 (unknown1 { unknownUpperKind = Just kind })
    return kind

  -- Same kind always returns the same thing.
  (Value, Value) -> return kind1

  -- `ObjectValue` is a subtype of `Value` so the least upper bound would be `Value`.
  (Value, ObjectValue) -> return kind1
  (ObjectValue, Value) -> return kind2

  -- Same kind always returns the same thing.
  (ObjectValue, ObjectValue) -> return kind1
