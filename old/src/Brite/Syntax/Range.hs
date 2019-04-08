module Brite.Syntax.Range
  ( Position(..)
  , initialPosition
  , nextPosition
  , nextPositionLine
  , utf16Length
  , Range(..)
  , rangeBetween
  , rangeContains
  , rangeIntersects
  , debugPosition
  , debugRange
  , RangeStack
  , singletonRange
  , pushRange
  , currentRange
  , initialRange
  ) where

import Data.Bits ((.&.))
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Custom as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder

-- A position between two characters in a Brite source code document. The encoding of a position is
-- based on the [Language Server Protocol][1].
--
-- * Positions are expressed with a zero-based line and character offset.
-- * A new line is started at one of the sequences `\n`, `\r\n`, and `\r`.
-- * Character offsets are determined by the number of code units in the characters UTF-16 encoding.
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
data Position = Position { positionLine :: !Int, positionCharacter :: !Int }
  deriving (Eq, Ord)

instance Show Position where
  show = Text.Builder.toString . debugPosition

-- The position at the start of a document.
initialPosition :: Position
initialPosition = Position 0 0

-- Adds the provided integer to the position character.
nextPosition :: Int -> Position -> Position
nextPosition n p = p { positionCharacter = positionCharacter p + n }

-- Adds the provided integer to the position line.
nextPositionLine :: Int -> Position -> Position
nextPositionLine n p = p { positionCharacter = 0, positionLine = positionLine p + n }

-- Debug a position.
debugPosition :: Position -> Text.Builder
debugPosition (Position line character) =
  Text.Builder.decimal line <> Text.Builder.singleton ':' <> Text.Builder.decimal character

-- Get the number of UTF-16 code units in one Unicode code point.
--
-- [Description of UTF-16 encoding.](https://en.wikipedia.org/wiki/UTF-16)
utf16Length :: Char -> Int
utf16Length c = if n .&. 0xFFFF == n then 1 else 2
  where n = fromEnum c

-- A range in a Brite source code document represents the space between two characters. The end
-- position is exclusive. A `Range` is comparable to a selection in an editor while a `Position` is
-- comparable to a cursor in an editor.
--
-- Ranges are also based on the [Language Server Protocol][1].
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
data Range = Range { rangeStart :: Position, rangeEnd :: Position }

instance Show Range where
  show = Text.Builder.toString . debugRange

-- Gets the range between two other ranges. Takes the first position from the first range and the
-- second position from the second range.
rangeBetween :: Range -> Range -> Range
rangeBetween (Range p1 _) (Range _ p2) = Range p1 p2
{-# INLINE rangeBetween #-}

-- True if the first range completely contains the second.
rangeContains :: Range -> Range -> Bool
rangeContains (Range start1 end1) (Range start2 end2) = start1 <= start2 && end2 <= end1
{-# INLINE rangeContains #-}

-- True if the two ranges intersect.
rangeIntersects :: Range -> Range -> Bool
rangeIntersects (Range start1 end1) (Range start2 end2) = not (end1 < start2 || end2 < start1)
{-# INLINE rangeIntersects #-}

-- Debug a range of characters.
debugRange :: Range -> Text.Builder
debugRange (Range start end) =
  debugPosition start <> Text.Builder.singleton '-' <> debugPosition end

-- A non-empty stack of ranges. We use a range stack to model a range which will may change over
-- time. For example, in our type checker we give every type a `RangeStack`. When a type is
-- referenced by a variable we push to the range stack while keeping the initial range intact.
--
-- Operations we support:
--
-- * Push to a range stack. O(1)
-- * Peek current range. O(1)
-- * Get initial range. O(1)
--
-- You’ll notice we don’t support “popping” from the range stack and you’ll notice that getting the
-- initial range is O(1) instead of O(n). That’s because we only store the current range and the
-- initial range. We don’t store all the ranges in between. Guess that means our stack isn’t
-- actually a stack. Naming things is hard. I’m no Taborlin.
data RangeStack = RangeStack (Maybe Range) Range

-- Creates a range stack with a single range.
singletonRange :: Range -> RangeStack
singletonRange = RangeStack Nothing

-- Adds a range to our stack.
pushRange :: Range -> RangeStack -> RangeStack
pushRange range1 (RangeStack _ range2) = RangeStack (Just range1) range2

-- Look at the current range in our stack.
currentRange :: RangeStack -> Range
currentRange (RangeStack Nothing range) = range
currentRange (RangeStack (Just range) _) = range

-- Get the initial range in our stack.
initialRange :: RangeStack -> Range
initialRange (RangeStack _ range) = range
