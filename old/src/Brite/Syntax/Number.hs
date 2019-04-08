{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Number
  ( Number(..)
  , IntegerBase(..)
  , numberSource
  , printInteger
  ) where

import Data.Char (intToDigit, toUpper)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder
import Numeric (showIntAtBase)

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
  -- `42`
  --
  -- A base 10 integer. We have the raw text representation of the integer in addition to its value.
  = DecimalInteger Text Integer

  -- `0b1101`
  --
  -- An integer written in binary form. The boolean is true if the “b” after `0` was lowercase. Then
  -- we have the integer’s raw text form and value.
  | BinaryInteger Bool Text Integer

  -- `0xFFF`
  --
  -- An integer written in hexadecimal form. The boolean is true if the “x” after `0` was lowercase.
  -- Then we have the integer’s raw text form and value.
  | HexadecimalInteger Bool Text Integer

  -- `3.1415`, `1e2`
  --
  -- A 64-bit floating point number. We aim for our floating point syntax to be compatible with the
  -- [JSON specification][1].
  --
  -- [1]: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
  | DecimalFloat Text Double
  deriving (Show)

-- The integer bases we support in Brite.
data IntegerBase
  -- 2
  = Binary
  -- 10
  | Decimal
  -- 16
  | Hexadecimal

-- Gets the source code for a number.
numberSource :: Number -> Text.Builder
numberSource (DecimalInteger raw _) = Text.Builder.fromText raw
numberSource (BinaryInteger True raw _) = Text.Builder.fromText "0b" <> Text.Builder.fromText raw
numberSource (BinaryInteger False raw _) = Text.Builder.fromText "0B" <> Text.Builder.fromText raw
numberSource (HexadecimalInteger True raw _) = Text.Builder.fromText "0x" <> Text.Builder.fromText raw
numberSource (HexadecimalInteger False raw _) = Text.Builder.fromText "0X" <> Text.Builder.fromText raw
numberSource (DecimalFloat raw _) = Text.Builder.fromText raw

-- Prints an integer to text at the provided base. Also prints the appropriate prefix. So `0b` for
-- binary and `0x` for hexadecimal. The printed integer should be ready for parsing as source code!
printInteger :: IntegerBase -> Integer -> Text.Builder
printInteger Decimal value = Text.Builder.decimal value
printInteger Binary value = Text.Builder.fromText "0b" <> Text.Builder.fromString (showIntAtBase 2 intToDigit value "")
printInteger Hexadecimal value = Text.Builder.fromText "0x" <> Text.Builder.fromString (showIntAtBase 16 (toUpper . intToDigit) value "")
