-- Custom operators on `Data.Text` using the internal text representation.

{-# LANGUAGE BangPatterns #-}

module Data.Text.Custom
  ( spanWithState
  ) where

import Data.Text.Internal (Text(..), text)
import Data.Text.Unsafe (Iter(..), iter)

-- `Data.Text.span` but it carries around some state.
spanWithState :: (a -> Char -> Maybe a) -> a -> Text -> (Text, a, Text)
spanWithState p a0 t@(Text arr off len) = (hd, a1, tl)
  where
    hd = text arr off k
    tl = text arr (off + k) (len - k)
    (a1, !k) = loop a0 0
    loop a !i =
      if i < len then
        case p a c of
          Nothing -> (a, i)
          Just a' -> loop a' (i + d)
      else
        (a, i)
      where
        Iter c d = iter t i
{-# INLINE spanWithState #-}
