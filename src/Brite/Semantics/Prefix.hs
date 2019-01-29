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
  , lookup
  , instantiate
  , generalize
  , update
  , mergeUpdate
  , allBindingNames
  , allBindings
  ) where

import Prelude hiding (lookup)
import Brite.Diagnostic
import Brite.Semantics.AST (Range, Identifier)
import Brite.Semantics.CheckMonad
import Brite.Semantics.Namer
import Brite.Semantics.Type (Polytype, Monotype)
import qualified Brite.Semantics.Type as Type
import Control.Monad.ST
import Data.Foldable (foldlM, traverse_)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashTable.ST.Cuckoo (HashTable)
import qualified Data.HashTable.ST.Cuckoo as HashTable
import Data.List (sort)
import Data.Maybe (isJust, fromMaybe)
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
  { prefixCounter :: STRef s FreshCounter
  , prefixLevels :: STRef s [PrefixLevel s]
  , prefixEntries :: HashTable s Identifier (PrefixEntry s)
  }

-- An entry for a type variable in the prefix. The entry remembers the level at which the entry is
-- stored and the quantifier which defined this type variable.
data PrefixEntry s = PrefixEntry
  { prefixEntryBinding :: STRef s Type.Binding
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
  , prefixLevelVariables :: HashTable s Identifier ()
  }

-- Creates a new prefix.
new :: Check s (Prefix s)
new = liftST (Prefix <$> newSTRef initialFreshCounter <*> newSTRef [] <*> HashTable.new)

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
  -- Get the current counter value. We will set this back after our action is done.
  oldCounter <- liftST $ readSTRef (prefixCounter prefix)
  -- Execute our action with the new level added to our prefix.
  result <- action
  -- Restore the old counter value.
  liftST $ writeSTRef (prefixCounter prefix) oldCounter
  -- Remove the level we just added from the prefix.
  liftST $ writeSTRef (prefixLevels prefix) oldLevels
  -- Delete all type variables at this level from the prefix.
  liftST $ flip HashTable.mapM_ (prefixLevelVariables newLevel)
    (\(i, _) -> HashTable.delete (prefixEntries prefix) i)
  -- Return the result from executing our action.
  return result

-- Does this name exist in the prefix?
exists :: Prefix s -> Identifier -> ST s Bool
exists prefix name = isJust <$> HashTable.lookup (prefixEntries prefix) name

-- Adds a type binding to our prefix assuming that the binding’s name is not already bound.
--
-- * If the prefix level stack is empty we don’t add this binding.
-- * If the binding type has free type variables that don’t exist in the prefix then we will happily
--   add your binding to the prefix anyway. Just know that you’re in for a world of trouble later
--   when you try to use those unbound type variables.
addAssumingThatNameIsUnbound :: Prefix s -> Type.Binding -> ST s ()
addAssumingThatNameIsUnbound prefix binding = do
  -- If there are no levels or a binding with the same ID already exists then we don’t add
  -- the binding.
  levels <- readSTRef (prefixLevels prefix)
  if null levels then return () else do
    -- Add the type variable binding to the current level.
    let level = head levels
    HashTable.insert (prefixLevelVariables level) (Type.bindingName binding) ()
    -- Add the type variable binding to the prefix.
    entry <- PrefixEntry <$> newSTRef binding <*> newSTRef level
    HashTable.insert (prefixEntries prefix) (Type.bindingName binding) entry

-- Adds a type binding to our prefix. If the name is already bound in the prefix then we generate a
-- unique name. We return `Nothing` if we did not need to generate a new name. We return `Just` if
-- we did need to generate a new name.
add :: Prefix s -> Type.Binding -> Check s (Maybe (Range -> Monotype))
add prefix binding = liftST $ do
  -- If the binding has the name of a fresh type then let’s generate a fresh type name instead of
  -- using `uniqueNameM`. We have a much smaller chance of collision that way since the prefix may
  -- contain any number of fresh type names already.
  newName <-
    if isFreshTypeName (Type.bindingName binding) then freshName prefix
    else uniqueNameM (exists prefix) (Type.bindingName binding)
  -- If we did not generate a new name then we can use our binding unchanged.
  if newName == Type.bindingName binding then do
    addAssumingThatNameIsUnbound prefix binding
    return Nothing
  else do
    -- Otherwise we need to add a binding with our new name to the prefix.
    addAssumingThatNameIsUnbound prefix (binding { Type.bindingName = newName })
    return (Just (\range -> Type.variable range newName))

-- Generates a fresh type variable name.
freshName :: Prefix s -> ST s Identifier
freshName prefix = do
  oldCounter <- readSTRef (prefixCounter prefix)
  (name, newCounter) <- freshTypeNameM (exists prefix) oldCounter
  writeSTRef (prefixCounter prefix) (newCounter)
  return name

-- Creates a fresh type variable with no bound and returns the name of the type variable.
fresh :: Prefix s -> Range -> Check s Monotype
fresh prefix range = liftST $ do
  name <- freshName prefix
  addAssumingThatNameIsUnbound prefix (Type.Binding name Type.Flexible (Type.bottom range))
  return (Type.variable range name)

-- Creates a fresh type variable with the provided type as the bound. If the provided type is a
-- monotype then we return the monotype directly instead of creating a fresh type variable.
freshWithBound :: Prefix s -> Range -> Type.Flexibility -> Polytype -> Check s Monotype
freshWithBound prefix range flexibility type0 = liftST $
  -- As an optimization, directly return monotypes instead of creating a new binding. Monotypes are
  -- always inlined in normal form anyway.
  case Type.polytypeDescription type0 of
    Type.Monotype' type1 -> return type1
    _ -> do
      name <- freshName prefix
      addAssumingThatNameIsUnbound prefix (Type.Binding name flexibility type0)
      return (Type.variable range name)

-- Finds the binding for the provided name in the prefix. If no type variable could be found then we
-- return nothing. The bound returned will always be in normal form.
lookup :: Prefix s -> Identifier -> Check s (Maybe Type.Binding)
lookup prefix name = liftST $ do
  -- Lookup the entry in the table. If the type is not in normal form we want to convert the type
  -- before returning.
  maybeEntry <- HashTable.lookup (prefixEntries prefix) name
  flip traverse maybeEntry $ \entry -> do
    -- Read the binding from our entry.
    binding <- readSTRef (prefixEntryBinding entry)
    -- If the type is not in normal form, convert it to normal form and update the binding in
    -- our entry.
    if not (Type.polytypeNormal (Type.bindingType binding)) then do
      let newType = Type.normal (Type.bindingType binding)
      let newBinding = binding { Type.bindingType = newType }
      writeSTRef (prefixEntryBinding entry) newBinding
      return newBinding
    else
      return binding

-- Merges the bounds of a quantified type into the prefix. If any of the bound names already exist
-- in the prefix then we need to generate new names. Those names will be substituted in the bounds
-- and the provided body type. The new body type with substituted names will be returned.
instantiate :: Prefix s -> [Type.Binding] -> Monotype -> Check s Monotype
instantiate prefix bindings body = do
  -- Loop through our bindings and add them to our prefix. All of the bindings with a naming
  -- conflict in our prefix will add a substitution to our substitutions map.
  substitutions2 <-
    foldlM
      (\substitutions1 binding -> do
        let
          -- If we have some substitutions then apply them to our binding.
          newBinding =
            if HashMap.null substitutions1 then binding
            else case Type.substitutePolytype substitutions1 (Type.bindingType binding) of
              Nothing -> binding
              Just newType -> binding { Type.bindingType = newType }
        -- Add the binding to our prefix.
        maybeNewType <- add prefix newBinding
        -- If we had to generate a new name then add a substitution to our map.
        return $ case maybeNewType of
          Nothing -> substitutions1
          Just newType -> HashMap.insert (Type.bindingName newBinding) newType substitutions1)
      -- Start with no substitutions and loop through bindings.
      HashMap.empty
      bindings
  -- If we have some substitutions we need to apply them to our body type.
  if HashMap.null substitutions2 then return body
  else return (fromMaybe body (Type.substituteMonotype substitutions2 body))

-- Generalizes a type at the current level of the prefix. If the type references variables in the
-- prefix at the current level then we will quantify the provided monotype with those type
-- variables. If the type references variables at another level then we will not quantify those. The
-- returned type will be in normal form.
--
-- If there are no levels then we return the type without generalizing.
generalize :: Prefix s -> Monotype -> Check s Polytype
generalize prefix body = liftST $ do
  -- Get the current prefix level. Return the body type directly if we have no levels.
  levels <- readSTRef (prefixLevels prefix)
  if null levels then return (Type.polytype body) else do
    let currentLevelIndex = prefixLevelIndex (head levels)
    -- Create a hash table for tracking the type variables we’ve visited. Also create a list of the
    -- bounds we are going to quantify.
    visited <- HashTable.new
    bindingsRef <- newSTRef []
    -- Our visitor will look at each free type variable and possibly add it to our bounds list
    -- and recurse.
    let
      visit name = do
        -- If we have not yet visited this type variable...
        nameVisited <- isJust <$> HashTable.lookup visited name
        if not nameVisited then do
          HashTable.insert visited name ()
          -- Find the type variable in our prefix. If it exists then we want to look at the
          -- entry’s level.
          HashTable.lookup (prefixEntries prefix) name >>= traverse_ (\entry -> do
            -- If the type variable is on our current level...
            entryLevelIndex <- prefixLevelIndex <$> readSTRef (prefixEntryLevel entry)
            if entryLevelIndex >= currentLevelIndex then do
              -- Read the binding reference.
              entryBinding <- readSTRef (prefixEntryBinding entry)
              -- Recurse and visit all free variables in our bound because the bound might have some
              -- dependencies on this level.
              traverse_ visit (Type.polytypeFreeVariables (Type.bindingType entryBinding))
              -- Add our bound to the list we will use to quantify. It is important we do this after
              -- recursing! Since the bound we add depends on the bounds we might add
              -- while recursing.
              modifySTRef bindingsRef (entryBinding :)
            else
              return ())
        else
          return ()
    -- Visit all the free variables in this type.
    traverse_ visit (Type.monotypeFreeVariables body)
    -- Quantify the type by the list of bounds we collected. Also convert the type to normal form.
    bindings <- readSTRef bindingsRef
    if null bindings then return (Type.polytype body)
    else return (Type.normal (Type.quantify (reverse bindings) body))

-- Checks to see if the provided name occurs anywhere in the type or in the bounds of any free type
-- variables recursively. If it does then we can’t update the type variable at `name` with the
-- provided type.
occurs :: Prefix s -> Identifier -> Polytype -> ST s Bool
occurs prefix targetName type0 = if HashSet.null (Type.polytypeFreeVariables type0) then return False else do
  -- Cache for type variables we have seen before so we don’t need to check them twice.
  seen <- HashTable.new
  let
    loop type1 = do
      -- Lazily search through all the type’s free variables. The lazy part is important here since
      -- we want to stop searching when we first find an occurrence.
      --
      -- NOTE: Validate that we stop at the first occurrence since `HashSet` does not implement
      -- `Foldable`. Instead it implements its own primitives.
      HashSet.foldr
        (\name seenOccurrenceM -> do
          seenOccurrence <- seenOccurrenceM
          if seenOccurrence then return True else
            if targetName == name then return True else do
              seenName <- isJust <$> HashTable.lookup seen name
              if seenName then return False else do
                HashTable.insert seen name ()
                HashTable.lookup (prefixEntries prefix) name >>=
                  mapM (readSTRef . prefixEntryBinding) >>=
                  maybe (return False) (loop . Type.bindingType))
        (return False)
        (Type.polytypeFreeVariables type1)
  -- Start our check with the initial type.
  loop type0

-- “Re-orders” the dependencies of a polytype in the prefix. The prefix is ordered by levels. All
-- the type variable in a given level will be generalized together. So if our new level comes
-- before the level of our type’s dependencies then we need to move our type’s dependencies to the
-- new level. Otherwise we’ll have dangling pointers when the type’s dependencies’ level is removed
-- from our prefix.
--
-- Oh, and yes, pun intended.
levelUp :: Prefix s -> PrefixLevel s -> Polytype -> ST s ()
levelUp prefix newLevel type0 =
  -- Loop through all of our free type variables...
  flip traverse_ (Type.polytypeFreeVariables type0) $ \name ->
    -- Lookup the type variable. If it exists then we want to check its level...
    HashTable.lookup (prefixEntries prefix) name >>= traverse_ (\entry -> do
      -- We know that the old level will be removed before the new level if the index of the old
      -- level is larger.
      oldLevel <- readSTRef (prefixEntryLevel entry)
      if prefixLevelIndex oldLevel > prefixLevelIndex newLevel then do
        -- Change our variable’s level.
        writeSTRef (prefixEntryLevel entry) newLevel
        HashTable.delete (prefixLevelVariables oldLevel) name
        HashTable.insert (prefixLevelVariables newLevel) name ()
        -- Recurse because we might have dependencies in our bound that also need to be updated.
        (Type.bindingType <$> readSTRef (prefixEntryBinding entry)) >>= levelUp prefix newLevel
      else
        return ())

-- Performs the checks which must pass for an update to be safe.
updateCheck :: Prefix s -> Type.Binding -> Polytype -> Check s (Either Diagnostic ())
updateCheck prefix oldBinding newType = do
  -- TODO: kinds
  -- TODO: abstraction

  -- Run an “occurs” check to make sure that we aren’t creating an infinite type with this update.
  -- If we would create an infinite type then return an error.
  nameOccurs <- liftST $ occurs prefix (Type.bindingName oldBinding) newType
  if nameOccurs then
    error "TODO: diagnostic"
  else if Type.bindingFlexibility oldBinding == Type.Rigid then
    error "TODO: update rigid bound"
  else
    -- The update is ok. You may proceed to commit changes...
    return (Right ())

-- IMPORTANT: We assume that `(Q) t1 ⊑ t2` holds. Where `t1` is the old type and `t2` is the new
-- type. Do not call this function with two types which do not uphold this relation!
--
-- The update algorithm replaces the bound of a type variable in our prefix with a new one. This
-- operation only succeeds if the prefix after updating is strictly an instance of the prefix before
-- updating. In other words `Q ⊑ Q'` must hold. We also must not add a circular dependency to our
-- prefix as this breaks the theoretical prefix ordering. So our update function has
-- these invariants:
--
-- 1. The binding already exists in the prefix.
-- 2. If `bound` or any of the referenced type variables in `bound` contain a
--    reference to `name` we return an error.
-- 3. If `Q ⊑ Q'` would not hold after applying this update we return an error.
--
-- We assume that the new type is an instance of the old type. However, we do check to ensure that
-- the new type is an abstraction of the old type for rigid bindings.
update :: Prefix s -> Identifier -> Polytype -> Check s (Either Diagnostic ())
update prefix name newType = do
  -- Lookup the existing binding entry in the prefix. If it does not exist then report an error
  -- diagnostic and abort.
  maybeEntry <- liftST $ HashTable.lookup (prefixEntries prefix) name
  case maybeEntry of
    Nothing -> error "TODO: Internal error diagnostic"
    Just entry -> do
      -- Make sure our update is safe.
      oldBinding <- liftST $ readSTRef (prefixEntryBinding entry)
      updateCheckResult <- updateCheck prefix oldBinding newType
      case updateCheckResult of
        -- If the update is not safe then return an error without committing anything.
        Left e -> return (Left e)
        -- If the update is safe then update our entry and update the levels of our new
        -- type’s dependencies.
        Right () -> liftST $ do
          writeSTRef (prefixEntryBinding entry) (Type.Binding name Type.Rigid newType)
          entryLevel <- readSTRef (prefixEntryLevel entry)
          levelUp prefix entryLevel newType
          return (Right ())

-- IMPORTANT: We assume that `(Q) t1 ⊑ t3` and `(Q) t2 ⊑ t3` hold. Where `t1` and `t2` are the old
-- types for both type variables respectively and `t3` is the new type. Do not call this function
-- with types which do not uphold this relation!
--
-- Updates two types to the same bound. Corresponds to the merge algorithm in the [MLF Thesis][1]
-- along with two calls to update. It is important we do these two updates together in a transaction
-- so a single error means we don’t commit anything.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
mergeUpdate :: Prefix s -> Identifier -> Identifier -> Type.Flexibility -> Polytype -> Check s (Either Diagnostic ())
mergeUpdate prefix name1 name2 flex newType = do
  -- Lookup both types in our hash table. Both _must_ exist. If one of them doesn’t then report an
  -- internal error diagnostic.
  maybeEntry1 <- liftST $ HashTable.lookup (prefixEntries prefix) name1
  maybeEntry2 <- liftST $ HashTable.lookup (prefixEntries prefix) name2
  case (maybeEntry1, maybeEntry2) of
    (Nothing, _) -> error "TODO: Internal error diagnostic"
    (_, Nothing) -> error "TODO: Internal error diagnostic"
    (Just entry1, Just entry2) -> do
      -- Make sure _both_ our updates are safe.
      oldBinding1 <- liftST $ readSTRef (prefixEntryBinding entry1)
      oldBinding2 <- liftST $ readSTRef (prefixEntryBinding entry2)
      updateCheckResult1 <- updateCheck prefix oldBinding1 newType
      updateCheckResult2 <- updateCheck prefix oldBinding2 newType
      case (updateCheckResult1, updateCheckResult2) of
        -- If the update is not safe then return an error without committing anything.
        (Left e, _) -> return (Left e)
        (_, Left e) -> return (Left e)
        -- If the update is safe then update our entry and update the levels of our new
        -- type’s dependencies.
        (Right (), Right ()) -> liftST $ do
          -- Update one of the entries to equal the provided bound. Then, link the other entry to
          -- the updated entry. We let the entry which will live longer be the one to hold our
          -- bound. We let the entry which will live shorter be our link.
          entryLevel1 <- readSTRef (prefixEntryLevel entry1)
          entryLevel2 <- readSTRef (prefixEntryLevel entry2)
          if prefixLevelIndex entryLevel1 <= prefixLevelIndex entryLevel2 then do
            let linkType = Type.polytype (Type.variable (Type.polytypeRange newType) name1)
            writeSTRef (prefixEntryBinding entry1) (Type.Binding name1 flex newType)
            writeSTRef (prefixEntryBinding entry2) (Type.Binding name2 Type.Rigid linkType)
            levelUp prefix entryLevel1 newType
            return (Right ())
          else do
            let linkType = Type.polytype (Type.variable (Type.polytypeRange newType) name2)
            writeSTRef (prefixEntryBinding entry2) (Type.Binding name2 flex newType)
            writeSTRef (prefixEntryBinding entry1) (Type.Binding name1 Type.Rigid linkType)
            levelUp prefix entryLevel2 newType
            return (Right ())

-- Gets a set of all the binding names in the prefix for debugging.
--
-- We return a set instead of a list because the bindings in the prefix are unordered.
allBindingNames :: Prefix s -> Check s (HashSet Identifier)
allBindingNames prefix = liftST $
  HashTable.foldM
    (\names (name, _) -> return (HashSet.insert name names))
    HashSet.empty
    (prefixEntries prefix)

-- Collects all the current bounds of the prefix into the list. The bounds are sorted in dependency
-- order. That is, dependents are listed after their dependencies.
allBindings :: Prefix s -> Check s [Type.Binding]
allBindings prefix = liftST $ do
  visited <- HashTable.new
  allVariables <- sort <$> HashTable.foldM (\names (name, _) -> return (name : names)) [] (prefixEntries prefix)
  reverse <$> foldlM (visit visited) [] allVariables
  where
    visit visited bindings0 name = do
      alreadyVisited <- isJust <$> HashTable.lookup visited name
      if alreadyVisited then return bindings0 else do
        HashTable.insert visited name ()
        maybeEntry <- HashTable.lookup (prefixEntries prefix) name
        case maybeEntry of
          Nothing -> return bindings0
          Just entry -> do
            binding <- readSTRef (prefixEntryBinding entry)
            let freeVariables = sort (HashSet.toList (Type.polytypeFreeVariables (Type.bindingType binding)))
            bindings1 <- foldlM (visit visited) bindings0 freeVariables
            return (binding : bindings1)
