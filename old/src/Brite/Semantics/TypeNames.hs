{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypeNames
  ( bottomTypeName
  , booleanTypeName
  , integerTypeName
  ) where

import Brite.Syntax.Identifier
import Data.Text (Text)

-- A textual representation of the bottom type.
bottomTypeName :: Text
bottomTypeName = "!"

-- The type name for booleans.
booleanTypeName :: Identifier
booleanTypeName = unsafeIdentifier "Bool"

-- The type name for integers.
integerTypeName :: Identifier
integerTypeName = unsafeIdentifier "Int"
