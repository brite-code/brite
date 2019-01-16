{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brite.DiagnosticsMarkup
  ( Markup
  , plain
  , code
  , toText
  , toANSIDoc
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

-- A simple markup string builder to be used when rendering diagnostic  messages. This is not
-- intended to be a full markdown parsing and printing framework. Just enough for
-- diagnostic messages.
newtype Markup = Markup { markupTextBuilder :: Text.Builder }
  deriving (Monoid, Semigroup)

-- Some plain text without formatting.
plain :: Text -> Markup
plain t = Markup (Text.Builder.fromText (escape t))

-- Text formatted as inline code.
code :: Text -> Markup
code t = Markup $
  Text.Builder.singleton '`' <> Text.Builder.fromText (escape t) <> Text.Builder.singleton '`'

-- Escapes sensitive characters in the Markup format.
--
-- NOTE: When adding escape sequences also make sure to update `toDoc`.
escape :: Text -> Text
escape = Text.concatMap
  (\c ->
    case c of
      '`' -> "\\`"
      '\\' -> "\\\\"
      _ -> Text.singleton c)

-- Creates a lazy text builder from the provided markup. The builder’s text will be in
-- markdown form.
toText :: Markup -> Text.Builder
toText (Markup t) = t

-- Converts our markup into a document to be rendered in an ANSI terminal.
toANSIDoc :: Markup -> Doc
toANSIDoc = loop mempty . Text.Builder.toLazyText . markupTextBuilder
  where
    loop acc0 t0 =
      let
        (t1, t2) = Text.Lazy.span (\c -> c /= ' ' && c /= '\\' && c /= '`') t0
        acc1 = acc0 <> Doc.text (Text.Lazy.unpack t1)
      in
        case Text.Lazy.uncons t2 of
          -- If we reached the end of `t0` then return `t1` and go home.
          Nothing -> acc1

          -- If we see some inline code then let’s start a sub-loop which only unescapes escape
          -- sequences. The content collected by the sub-loop will be formatted in bold.
          Just ('`', t3) ->
            let (t4, t5) = subLoop mempty t3 in
              loop (acc1 <> Doc.bold t4) t5
            where
              subLoop acc2 t4 =
                let
                  (t5, t6) = Text.Lazy.span (\c -> c /= '`' && c /= '\\') t4
                  acc3 = acc2 <> Doc.text (Text.Lazy.unpack t5)
                in
                  case Text.Lazy.uncons t6 of
                    Nothing -> (acc3, t6)

                    -- Unescape escape sequences!
                    Just ('\\', t7) ->
                      case unescape t7 of
                        Nothing -> subLoop acc3 t7
                        Just (c, t8) -> subLoop (acc3 <> Doc.text c) t8

                    -- We only expect "`" here, but we use a wildcard so that our pattern match is
                    -- exhaustive.
                    Just (_, t7) -> (acc3, t7)

          -- If we reached an escape sequence then we need to unescape!
          Just ('\\', t3) ->
            case unescape t3 of
              Nothing -> loop acc1 t3
              Just (c, t4) -> loop (acc1 <> Doc.text c) t4

          -- Insert a `Doc.line` instead of spaces. This will allow the pretty printing framework to
          -- break our text onto multiple lines.
          --
          -- We only expect spaces here, but we use a wildcard so that our pattern match
          -- is exhaustive.
          Just (_, t3) -> loop (acc1 <> Doc.line) t3

    -- Unescapes the characters after a backslash.
    unescape t0 =
      case Text.Lazy.uncons t0 of
        Nothing -> Nothing
        Just ('`', t1) -> Just ("`", t1)
        Just ('\\', t1) -> Just ("\\", t1)
        Just (_, _) -> Nothing
