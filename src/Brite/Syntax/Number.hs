{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Number
  ( Number(..)
  , numberSource
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

-- A number in Brite source code could be written in a few different ways:
--
-- * Integer: `42`
-- * Binary integer: `0b1101`
-- * Hexadecimal integer: `0xFFF`
-- * Floating point: `3.1415`, `1e2`
--
-- For floating point numbers we use the same syntax as the [JSON specification][1] with the one
-- modification that we permit leading zeroes.
--
-- We never parse a negative sign as part of our number syntax. Instead we use a negative operator
-- in our language syntax.
--
-- [1]: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
data Number
  -- `0b1101`
  --
  -- An integer written in binary form. The boolean is true if the “b” after `0` was lowercase. Then
  -- we have the integer’s raw text form and value.
  = BinaryInteger Bool Text Integer

  -- `0xFFF`
  --
  -- An integer written in hexadecimal form. The boolean is true if the “x” after `0` was lowercase.
  -- Then we have the integer’s raw text form and value.
  | HexadecimalInteger Bool Text Integer
  deriving (Show)

-- Gets the source code for a number.
numberSource :: Number -> Text.Builder
numberSource (BinaryInteger True raw _) = Text.Builder.fromText "0b" <> Text.Builder.fromText raw
numberSource (BinaryInteger False raw _) = Text.Builder.fromText "0B" <> Text.Builder.fromText raw
numberSource (HexadecimalInteger True raw _) = Text.Builder.fromText "0x" <> Text.Builder.fromText raw
numberSource (HexadecimalInteger False raw _) = Text.Builder.fromText "0X" <> Text.Builder.fromText raw
