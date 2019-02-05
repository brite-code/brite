{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypeNames
  ( booleanTypeName
  , integerTypeName
  ) where

import Brite.Syntax.Identifier

-- The type name for booleans.
booleanTypeName :: Identifier
booleanTypeName = unsafeIdentifier "Bool"

-- The type name for integers.
integerTypeName :: Identifier
integerTypeName = unsafeIdentifier "Int"
