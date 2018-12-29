{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brite.DiagnosticsMarkup
  ( Markup
  , plain
  , code
  , toBuilder
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

-- A simple markup string builder to be used when rendering diagnostic  messages. This is not
-- intended to be a full markdown parsing and printing framework. Just enough for
-- diagnostic messages.
newtype Markup = Markup B.Builder
  deriving (Monoid, Semigroup)

-- Creates a lazy text builder from the provided markup. The builder’s text will be in
-- markdown form.
toBuilder :: Markup -> B.Builder
toBuilder (Markup b) = b

-- Some plain text without formatting.
plain :: T.Text -> Markup
plain t = Markup (B.fromText (T.replace "`" "\\`" t))

-- Text formatted as inline code.
code :: T.Text -> Markup
code t = Markup (B.singleton '`' <> B.fromText (T.replace "`" "\\`" t) <> B.singleton '`')
