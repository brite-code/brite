{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brite.DiagnosticsMarkup
  ( Markup
  , plain
  , code
  , printMarkup
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

-- A simple markup string builder to be used when rendering diagnostic  messages. This is not
-- intended to be a full markdown parsing and printing framework. Just enough for
-- diagnostic messages.
newtype Markup = Markup Text.Builder
  deriving (Monoid, Semigroup)

-- Creates a lazy text builder from the provided markup. The builder’s text will be in
-- markdown form.
printMarkup :: Markup -> Text.Builder
printMarkup (Markup t) = t

-- Some plain text without formatting.
plain :: Text -> Markup
plain t = Markup (Text.Builder.fromText (Text.replace "`" "\\`" t))

-- Text formatted as inline code.
code :: Text -> Markup
code t = Markup $
  Text.Builder.singleton '`'
    <> Text.Builder.fromText (Text.replace "`" "\\`" t)
    <> Text.Builder.singleton '`'
