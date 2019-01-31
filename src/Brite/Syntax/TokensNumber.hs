module Brite.Syntax.TokensNumber
  ( DecimalDigit(..)
  , charDecimalDigit
  , BinaryDigit(..)
  , HexadecimalDigit(..)
  ) where

-- 0-9
data DecimalDigit
  = D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  deriving (Eq)

-- Gets a `DecimalDigit` for a character.
charDecimalDigit :: Char -> Maybe DecimalDigit
charDecimalDigit '0' = Just D0
charDecimalDigit '1' = Just D1
charDecimalDigit '2' = Just D2
charDecimalDigit '3' = Just D3
charDecimalDigit '4' = Just D4
charDecimalDigit '5' = Just D5
charDecimalDigit '6' = Just D6
charDecimalDigit '7' = Just D7
charDecimalDigit '8' = Just D8
charDecimalDigit '9' = Just D9
charDecimalDigit _ = Nothing

-- 0 and 1
data BinaryDigit
  = B0
  | B1

-- 0-9 and A-F
data HexadecimalDigit
  = X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | XA
  | XB
  | XC
  | XD
  | XE
  | XF
