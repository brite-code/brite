-- And Taborlin the Great said to the stone: "BREAK!" and the stone broke...

module Brite.Semantics.Namer
  ( uniqueName
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
uniqueName exists name = loop start
  where
    (start, namePrefix) = case integerSuffix (identifierText name) of
      Nothing -> (2, identifierText name)
      Just (t, i) -> (i + 1, t)

    loop i =
      let newName = unsafeIdentifier (Text.append namePrefix (Text.pack (show i))) in
        if exists newName then loop (i + 1) else newName

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
