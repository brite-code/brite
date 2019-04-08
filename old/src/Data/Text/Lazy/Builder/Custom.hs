module Data.Text.Lazy.Builder.Custom
  ( toStrictText
  , toString
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

-- Converts a text builder to a strict text value.
toStrictText :: Text.Builder -> Text
toStrictText = Text.Lazy.toStrict . Text.Builder.toLazyText

-- Converts a text builder to a string.
toString :: Text.Builder -> String
toString = Text.Lazy.unpack . Text.Builder.toLazyText
