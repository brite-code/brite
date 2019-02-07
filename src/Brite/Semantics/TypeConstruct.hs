module Brite.Semantics.TypeConstruct
  ( Construct(..)
  , typeConstructorSnippet
  , mergeProperties
  ) where

import Brite.Syntax.Identifier
import Brite.Syntax.Range
import Brite.Syntax.Snippet
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

-- In Brite, all types with an associated runtime value are constructed types. Other types like
-- variable types or quantified types are used to represent polymorphism or aid in type inference,
-- but only constructed types have corresponding runtime values.
--
-- The `Construct` type includes all of our type constructors along with their arguments. For
-- example, the function type constructor may have an arity of two or more. The void type
-- constructor has an arity of zero.
--
-- So whereas `Function` is a constructor, `Function a a` is a construct since it carries the
-- arguments `a`.
--
-- We don’t export the variants of `Construct`. We only export the type itself.
data Construct a
  -- `void`
  --
  -- There is only one ”void” value that inhabits the void type.
  = Void

  -- `Bool`
  --
  -- A boolean is either the value “true” or the value “false”.
  | Boolean

  -- `Int`
  --
  -- An integer is a 32-bit signed integer.
  | Integer

  -- `fun(T) -> T`
  --
  -- A function takes some input and returns some value as the output.
  --
  -- TODO: Currently we only allow exactly one function argument, but Brite should support any
  -- number of function arguments.
  | Function a a

  -- `{p: T}`
  --
  -- An object type has some properties and optionally extends some other type. We use an ordered
  -- `Map` for the properties so that comparison between object keys is efficient.
  --
  -- Our object types implement records as specified in the [“Extensible records with scoped
  -- labels”][1] paper. That means every property in the object may have _multiple_
  -- associated values! This is an important feature of the paper which greatly simplifies
  -- type checking.
  --
  -- [1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf
  | Object (Map Identifier [(Range, a)]) (Maybe a)

instance Functor Construct where
  fmap _ Void = Void
  fmap _ Boolean = Boolean
  fmap _ Integer = Integer
  fmap f (Function a b) = Function (f a) (f b)
  fmap f (Object ps e) = Object (fmap (fmap f) <$> ps) (f <$> e)

instance Foldable Construct where
  foldMap _ Void = mempty
  foldMap _ Boolean = mempty
  foldMap _ Integer = mempty
  foldMap f (Function a b) = f a <> f b
  foldMap f (Object ps e) = foldMap (foldMap (f . snd)) ps <> foldMap f e

-- Gets a snippet representing the type constructor.
typeConstructorSnippet :: Construct a -> TypeConstructorSnippet
typeConstructorSnippet Void = VoidConstructorSnippet
typeConstructorSnippet Boolean = BooleanConstructorSnippet
typeConstructorSnippet Integer = IntegerConstructorSnippet
typeConstructorSnippet (Function _ _) = FunctionConstructorSnippet
typeConstructorSnippet (Object _ _) = ObjectConstructorSnippet

-- Merges two property maps returning a map of the merged properties and two maps containing all the
-- properties which were not merged.
mergeProperties ::
  Map Identifier [(Range, a)] -> Map Identifier [(Range, a)] ->
    ( Map Identifier [((Range, a), (Range, a))]                  -- Shared properties
    , (Map Identifier [(Range, a)], Map Identifier [(Range, a)]) -- Overflow properties
    )

-- Optimization: If we only have one property, don’t run a full `Map.mergeA` which is O(n) but
-- instead do a single delete which is O(log(n)). Calling `mergeProperties` with a single property
-- is very common for property lookups like `o.p`.
mergeProperties properties1 properties2 | Map.size properties1 == 1 =
  let
    (name, nameProperties1) = Map.elemAt 0 properties1

    ((overflowNameProperties1, sharedNameProperties), overflowProperties2) =
      Map.alterF
        (\maybeNameProperties2 ->
          case maybeNameProperties2 of
            Nothing -> ((nameProperties1, []), Nothing)
            Just nameProperties2 ->
              let
                loop (p1 : ps1) (p2 : ps2) = ((p1, p2) :) <$> loop ps1 ps2
                loop [] [] = (([], []), [])
                loop ps1@(_ : _) [] = ((ps1, []), [])
                loop [] ps2@(_ : _) = (([], ps2), [])

                ((overflowNameProperties1', overflowNameProperties2), sharedNameProperties') =
                  loop nameProperties1 nameProperties2
              in
                ( (overflowNameProperties1', sharedNameProperties')
                , if null overflowNameProperties2 then Nothing else Just overflowNameProperties2
                ))
        name
        properties2
  in
    ( if null sharedNameProperties then Map.empty else Map.singleton name sharedNameProperties
    , ( if null overflowNameProperties1 then Map.empty else Map.singleton name overflowNameProperties1
      , overflowProperties2
      )
    )

-- Optimization: If we only have one property, don’t run a full `Map.mergeA` which is O(n) but
-- instead do a single delete which is O(log(n)). Calling `mergeProperties` with a single property
-- is very common for property lookups like `o.p`.
mergeProperties properties1 properties2 | Map.size properties2 == 1 =
  let
    (name, nameProperties2) = Map.elemAt 0 properties2

    ((overflowNameProperties2, sharedNameProperties), overflowProperties1) =
      Map.alterF
        (\maybeNameProperties1 ->
          case maybeNameProperties1 of
            Nothing -> ((nameProperties2, []), Nothing)
            Just nameProperties1 ->
              let
                loop (p1 : ps1) (p2 : ps2) = ((p1, p2) :) <$> loop ps1 ps2
                loop [] [] = (([], []), [])
                loop ps1@(_ : _) [] = ((ps1, []), [])
                loop [] ps2@(_ : _) = (([], ps2), [])

                ((overflowNameProperties1, overflowNameProperties2'), sharedNameProperties') =
                  loop nameProperties1 nameProperties2
              in
                ( (overflowNameProperties2', sharedNameProperties')
                , if null overflowNameProperties1 then Nothing else Just overflowNameProperties1
                ))
        name
        properties1
  in
    ( if null sharedNameProperties then Map.empty else Map.singleton name sharedNameProperties
    , ( overflowProperties1
      , if null overflowNameProperties2 then Map.empty else Map.singleton name overflowNameProperties2
      )
    )

-- General case:
mergeProperties properties1 properties2 = flip runState (Map.empty, Map.empty) $
  Map.mergeA
    -- Insert missing properties into an overflow property map. There won’t be any collision because
    -- the overflow maps are subsets of the original property maps.
    (Map.traverseMaybeMissing
      (\name nameProperties -> state $ \(overflowProperties1, overflowProperties2) ->
        (Nothing, (Map.insert name nameProperties overflowProperties1, overflowProperties2))))
    (Map.traverseMaybeMissing
      (\name nameProperties -> state $ \(overflowProperties1, overflowProperties2) ->
        (Nothing, (overflowProperties1, Map.insert name nameProperties overflowProperties2))))

    -- Zip matching properties together. Any overflow properties with the same name will go into our
    -- overflow maps.
    (Map.zipWithAMatched
      (\name nameProperties1 nameProperties2 ->
        let
          loop (p1 : ps1) (p2 : ps2) = ((p1, p2) :) <$> loop ps1 ps2
          loop [] [] = return []
          loop ps1@(_ : _) [] = stateT (\(xps1, xps2) -> ([], (Map.insert name ps1 xps1, xps2)))
          loop [] ps2@(_ : _) = stateT (\(xps1, xps2) -> ([], (xps1, Map.insert name ps2 xps2)))
        in
          loop nameProperties1 nameProperties2))

    -- Pass in our original property maps.
    properties1
    properties2

  where
    -- Give a concrete type to the `state` function. Otherwise type checking fails without
    -- the `FlexibleContexts` extension.
    stateT :: (s -> (a, s)) -> State s a
    stateT = state
