-- This module is intended to be imported qualified:
--
-- ```hs
-- import Brite.Semantics.Prefix (Prefix)
-- import qualified Brite.Semantics.Prefix as Prefix
-- ```

module Brite.Semantics.Prefix
  ( Prefix
  , new
  , withLevel
  , add
  , fresh
  , freshWithBound
  ) where

import Brite.Semantics.CheckMonad
import Brite.Semantics.Type (Polytype, Monotype)
import qualified Brite.Semantics.Type as Type
import Data.HashTable.ST.Cuckoo (HashTable)
import qualified Data.HashTable.ST.Cuckoo as HashTable
import Data.Maybe (isJust)
import Data.STRef

-- The prefix manages all the type variables we create during type checking. The prefix uses the
-- `ST` monad for mutability. Unlike other immutable Haskell data types.
--
-- The prefix gets its name from the theory of a polytype “prefix” in the [MLF thesis][1]. See
-- Section 1.2 for an introduction to prefixes in theory. From the thesis:
--
-- > A prefix `Q` is a sequence of bindings `(a1 x o1)...(an x on)`
--
-- Our prefix, in theory, is also a sequence of bindings, however, from just a casual observation
-- one will note that the type is a bit more complex then that. You see, throughout our type system
-- there are a number of complicated set operations that we must perform on prefixes. For instance,
-- in the `infer()` algorithm (Section 7.1) the MLF thesis uses a “split” operation to add
-- quantifiers back to a type. Operations like this are expensive if we literally implement them on
-- a sequence of bindings, so instead we implement them with our type.
--
-- Our prefix takes a page from traditional ML level-based type checkers. Like the one described in
-- [“How the OCaml type checker works -- or what polymorphism and garbage collection have
-- in common”.][2] The section on “Unsound generalization as memory mismanagement” is particularly
-- applicable to explaining why our implementation is a bit complex.
--
-- We maintain a stack of “levels” in our prefix. Every time we create a type variable we add it to
-- the top level in our level stack. When we pop the level off our level stack we remove all the
-- type variables inside of that level from our prefix. If we update a type variable from an earlier
-- level in the stack with a type variable from a later level in the stack then we move the type
-- variable from later in the stack up to the earliest level. This way we won’t have a dangling type
-- variable pointer. (See, memory mismanagement!)
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
-- [2]: http://okmij.org/ftp/ML/generalization.html
data Prefix s = Prefix
  { prefixLevels :: STRef s [PrefixLevel s]
  , prefixEntries :: HashTable s TypeVariableID (PrefixEntry s)
  }

-- An entry for a type variable in the prefix. The entry remembers the level at which the entry is
-- stored and the quantifier which defined this type variable.
data PrefixEntry s = PrefixEntry
  { prefixEntryBinding :: Type.Binding
  , prefixEntryLevel :: STRef s (PrefixLevel s)
  }

-- In our prefix, levels are a way to manage garbage collection. Whenever we introduce a type
-- variable into our prefix that type variable has a lifetime. That lifetime is expressed by its
-- level. Whenever the type variable’s level is removed from the prefix’s level stack then the type
-- variable is removed from the prefix entirely.
--
-- However, sometimes the programmer will update a type so that a type with a longer lifetime
-- depends on it. When that happens we change the level of the type variable.
data PrefixLevel s = PrefixLevel
  { prefixLevelIndex :: Int
  , prefixLevelVariables :: HashTable s TypeVariableID ()
  }

-- Creates a new prefix.
new :: Check s (Prefix s)
new = liftST (Prefix <$> newSTRef [] <*> HashTable.new)

-- Introduces a new level for the execution of the provided action. Cleans up the level after the
-- action finishes. Any type variables created in the level will be removed from the prefix unless
-- they were moved in an update.
withLevel :: Prefix s -> Check s a -> Check s a
withLevel prefix action = do
  -- Add a new level to the prefix.
  oldLevels <- liftST $ readSTRef (prefixLevels prefix)
  let index = case oldLevels of { [] -> 0; level : _ -> prefixLevelIndex level + 1 }
  newLevel <- liftST $ PrefixLevel index <$> HashTable.new
  liftST $ writeSTRef (prefixLevels prefix) (newLevel : oldLevels)
  -- Execute our action with the new level added to our prefix.
  result <- action
  -- Remove the level we just added from the prefix.
  liftST $ writeSTRef (prefixLevels prefix) oldLevels
  -- Delete all type variables at this level from the prefix.
  liftST $ flip HashTable.mapM_ (prefixLevelVariables newLevel)
    (\(i, _) -> HashTable.delete (prefixEntries prefix) i)
  -- Return the result from executing our action.
  return result

-- Adds a type variable binding to our prefix.
--
-- * If the prefix level stack is empty we don’t add this binding.
-- * If the binding already exists in the prefix we don’t add this binding, leaving the current
--   one as-is.
-- * If the binding type has free type variables that don’t exist in the prefix then we will happily
--   add your binding to the prefix anyway. Just know that you’re in for a world of trouble later
--   when you try to use those unbound type variables.
add :: Prefix s -> Type.Binding -> Check s ()
add prefix binding = do
  -- If there are no levels or a binding with the same ID already exists then we don’t add
  -- the binding.
  levels <- liftST $ readSTRef (prefixLevels prefix)
  hasBinding <- liftST $ isJust <$> HashTable.lookup (prefixEntries prefix) (Type.bindingID binding)
  if null levels || hasBinding then return () else do
    -- Add the type variable binding to the current level.
    let level = head levels
    liftST $ HashTable.insert (prefixLevelVariables level) (Type.bindingID binding) ()
    -- Add the type variable binding to the prefix.
    entry <- liftST $ PrefixEntry binding <$> newSTRef level
    liftST $ HashTable.insert (prefixEntries prefix) (Type.bindingID binding) entry

-- Creates a fresh type variable with no bound.
fresh :: Prefix s -> Check s Monotype
fresh prefix = do
  i <- freshTypeVariable
  add prefix (Type.Binding i Nothing Type.Flexible Type.bottom)
  return (Type.variable i)

-- Creates a fresh type variable with the provided type as the bound. If the provided type is a
-- monotype then we return the monotype directly instead of creating a fresh type variable.
freshWithBound :: Prefix s -> Type.Flexibility -> Polytype -> Check s Monotype
freshWithBound prefix k t =
  -- As an optimization, directly return monotypes instead of creating a new binding. Monotypes are
  -- always inlined in normal form anyway.
  case Type.polytypeDescription t of
    Type.Monotype' t' -> return t'
    _ -> do
      -- Creates a fresh type variable ID and adds a binding with that ID to the prefix.
      i <- freshTypeVariable
      add prefix (Type.Binding i Nothing k t)
      return (Type.variable i)
