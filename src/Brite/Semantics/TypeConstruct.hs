{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypeConstruct
  ( Construct(..)
  , booleanTypeName
  , integerTypeName
  , typeConstructorSnippet
  ) where

import Brite.Syntax.Identifier
import Brite.Syntax.Snippet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

  -- -- `{p: T}`
  -- --
  -- -- An object type has some properties and optionally extends some other type. We use an ordered
  -- -- `Map` for the properties so that comparison between objects is efficient.
  -- | Object (Map Identifier a) (Maybe a)

instance Functor Construct where
  fmap _ Void = Void
  fmap _ Boolean = Boolean
  fmap _ Integer = Integer
  fmap f (Function a b) = Function (f a) (f b)
  -- fmap f (Object ps e) = Object (f <$> ps) (f <$> e)

instance Foldable Construct where
  foldMap _ Void = mempty
  foldMap _ Boolean = mempty
  foldMap _ Integer = mempty
  foldMap f (Function a b) = f a <> f b
  -- foldMap f (Object ps e) = foldMap f ps <> foldMap f e

-- The type name for booleans.
booleanTypeName :: Identifier
booleanTypeName = unsafeIdentifier "Bool"

-- The type name for integers.
integerTypeName :: Identifier
integerTypeName = unsafeIdentifier "Int"

-- Gets a snippet representing the type constructor.
typeConstructorSnippet :: Construct a -> TypeConstructorSnippet
typeConstructorSnippet Void = VoidConstructorSnippet
typeConstructorSnippet Boolean = BooleanConstructorSnippet
typeConstructorSnippet Integer = IntegerConstructorSnippet
typeConstructorSnippet (Function _ _) = FunctionConstructorSnippet
-- typeConstructorSnippet (Object _ _) = ObjectConstructorSnippet
