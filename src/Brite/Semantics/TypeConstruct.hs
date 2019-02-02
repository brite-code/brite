{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypeConstruct
  ( Construct(..)
  , Constructor
  , booleanTypeName
  , integerTypeName
  ) where

import Brite.Syntax.Identifier

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
  -- There is only one ”void” value that inhabits the void type.
  = Void

  -- A boolean is either the value “true” or the value “false”.
  | Boolean

  -- An integer is a 32-bit signed integer.
  | Integer

  -- A function takes some input and returns some value as the output.
  --
  -- TODO: Currently we only allow exactly one function argument, but Brite should support any
  -- number of function arguments.
  | Function a a

-- If we only want the type constructor we zero-out all the arguments.
type Constructor = Construct ()

instance Functor Construct where
  fmap _ Void = Void
  fmap _ Boolean = Boolean
  fmap _ Integer = Integer
  fmap f (Function a b) = Function (f a) (f b)

instance Foldable Construct where
  foldMap _ Void = mempty
  foldMap _ Boolean = mempty
  foldMap _ Integer = mempty
  foldMap f (Function a b) = f a <> f b

-- The type name for booleans.
booleanTypeName :: Identifier
booleanTypeName = unsafeIdentifier "Bool"

-- The type name for integers.
integerTypeName :: Identifier
integerTypeName = unsafeIdentifier "Int"
