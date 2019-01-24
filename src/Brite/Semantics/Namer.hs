-- And Taborlin the Great said to the stone: "BREAK!" and the stone broke...

{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.Namer
  ( uniqueName
  , freshTypeName
  , freshTypeBaseName
  ) where

import Brite.Syntax.Tokens (Identifier, unsafeIdentifier, identifierText)
import Data.Text (Text)
import qualified Data.Text as Text

-- Generates a unique name based on the provided name. If the name is already unique we return it.
-- If the name is not unique we increments an integer suffix count on the name to create new names.
-- For instance, `x` becomes `x2`, `x2` becomes `x3`, `x3` becomes `x4` and so on. Uses the provided
-- function to check if a name already exists.
uniqueName :: (Identifier -> Bool) -> Identifier -> Identifier
uniqueName exists name | not (exists name) = name
uniqueName exists initialName = loop start
  where
    (start, namePrefix) = case integerSuffix (identifierText initialName) of
      Nothing -> (2, identifierText initialName)
      Just (name, suffix) -> (suffix + 1, name)

    loop i =
      let newName = unsafeIdentifier (Text.append namePrefix (Text.pack (show i))) in
        if exists newName then loop (i + 1) else newName

-- Generates the name of a fresh type when we don’t have a user provided name name to base our type
-- name off of. Implement the same as `uniqueName` under the hood.
--
-- We choose to name these types `TypeN` where “N” is a positive integer. Other languages may choose
-- `tN` or `TN` or a sequence of `a`, `b`, `c`, etc. We choose `TypeN` because we want to enforce
-- for the programmer that type variable names are just like regular variable names and they should
-- be named as such. A name like `T` (while popular in Java, JavaScript, and C) is not
-- very expressive.
freshTypeName :: (Identifier -> Bool) -> Identifier
freshTypeName exists = loop (1 :: Int)
  where
    loop i =
      let newName = unsafeIdentifier (Text.append freshTypeBaseName (Text.pack (show i))) in
        if exists newName then loop (i + 1) else newName

-- The base name for `freshTypeName`. Does not include an integer suffix. Useful if you want to
-- implement your own name generator.
freshTypeBaseName :: Text
freshTypeBaseName = "Type"

-- Gets the integer suffix of the provided name. For instance, for `x` we return nothing, but for
-- `x42` we return the integer 42. Also returns the string before the suffix.
integerSuffix :: Text -> Maybe (Text, Int)
integerSuffix text0 =
  case Text.unsnoc text0 of
    Just (text1, '0') -> Just (loop 0 0 text1)
    Just (text1, '1') -> Just (loop 0 1 text1)
    Just (text1, '2') -> Just (loop 0 2 text1)
    Just (text1, '3') -> Just (loop 0 3 text1)
    Just (text1, '4') -> Just (loop 0 4 text1)
    Just (text1, '5') -> Just (loop 0 5 text1)
    Just (text1, '6') -> Just (loop 0 6 text1)
    Just (text1, '7') -> Just (loop 0 7 text1)
    Just (text1, '8') -> Just (loop 0 8 text1)
    Just (text1, '9') -> Just (loop 0 9 text1)
    _ -> Nothing
  where
    loop :: Int -> Int -> Text -> (Text, Int)
    loop k n text1 =
      case Text.unsnoc text1 of
        Just (text2, '0') -> loop (k + 1) (n + (0 * 10 ^ (k + 1))) text2
        Just (text2, '1') -> loop (k + 1) (n + (1 * 10 ^ (k + 1))) text2
        Just (text2, '2') -> loop (k + 1) (n + (2 * 10 ^ (k + 1))) text2
        Just (text2, '3') -> loop (k + 1) (n + (3 * 10 ^ (k + 1))) text2
        Just (text2, '4') -> loop (k + 1) (n + (4 * 10 ^ (k + 1))) text2
        Just (text2, '5') -> loop (k + 1) (n + (5 * 10 ^ (k + 1))) text2
        Just (text2, '6') -> loop (k + 1) (n + (6 * 10 ^ (k + 1))) text2
        Just (text2, '7') -> loop (k + 1) (n + (7 * 10 ^ (k + 1))) text2
        Just (text2, '8') -> loop (k + 1) (n + (8 * 10 ^ (k + 1))) text2
        Just (text2, '9') -> loop (k + 1) (n + (9 * 10 ^ (k + 1))) text2
        _ -> (text1, n)