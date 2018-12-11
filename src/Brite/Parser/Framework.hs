{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Brite.Parser.Framework
  ( Parser
  , ParseResult
  , glyph
  , keyword
  , identifier
  , runParser
  ) where

import Brite.Diagnostics
import Brite.Source
import Data.Maybe

-- Parses some tokens from a `TokenList` returning `a`.
data Parser a where
  -- A terminal parser may only parse a single token. It takes a function which tries to parse a
  -- single token. If that function returns `Just` we know our terminal parser was successful.
  --
  -- Also contains `ExpectedToken` for error reporting.
  TerminalParser :: ExpectedToken -> (Token -> Maybe b) -> Parser (ParseResult (Range, b))
  -- An empty parser never parses any tokens. It always returns its payload.
  EmptyParser :: a -> Parser a
  -- A sequence parser combines two parsers to be executed one after another. The first parser
  -- should return a function which we can apply with the result of the second parser.
  SequenceParser :: Parser (b -> a) -> Parser b -> Parser a

instance Functor Parser where
  -- To map a terminal parser we sequence it with an empty parser that returns the mapper function.
  fmap f p@(TerminalParser _ _) = SequenceParser (EmptyParser f) p

  -- Empty parsers map their constant argument.
  fmap f (EmptyParser a) = EmptyParser (f a)

  -- To map a sequence parser we sequence it with an empty parser that returns the mapper function.
  -- We could also `fmap` into the first parser of `SequenceParser`, but that would be O(n) whereas
  -- this solution is O(1).
  fmap f p@(SequenceParser _ _) = SequenceParser (EmptyParser f) p

-- You may have noticed that the signatures of `EmptyParser` and `SequenceParser` are identical to
-- the signatures needed to implement `Applicative`. That’s intentional.
instance Applicative Parser where
  pure = EmptyParser
  (<*>) = SequenceParser

-- A parse result is returned by our terminal parser after attempting to parse a token.
--
-- * `Right` if we were able to successfully parse some data.
-- * `Left Nothing` if we failed to parse any data.
-- * `Left Just` if we ran into an error, but we were able to recover.
type ParseResult a = Either (Diagnostic, Maybe a) a

-- Glyph parser. Returns the range that was parsed if we could parse the glyph.
glyph :: Glyph -> Parser (ParseResult (Range, ()))
glyph g = TerminalParser (ExpectedGlyph g) (\case
  Glyph g' | g == g' -> Just ()
  _ -> Nothing)

-- Keyword parser. Returns the range that was parsed if we could parse the glyph.
keyword :: Keyword -> Parser (ParseResult (Range, ()))
keyword k = glyph (Keyword k)

-- Identifier parser. Returns the identifier along with the range of the identifier.
identifier :: Parser (ParseResult (Range, Identifier))
identifier = TerminalParser ExpectedIdentifier (\case
  IdentifierToken ident -> Just ident
  _ -> Nothing)

-- Tests if the token is the _first_ token of the parser.
test :: Parser a -> Token -> Bool
test (TerminalParser _ p) t = isJust (p t)
test (EmptyParser _) _ = False
test (SequenceParser (EmptyParser _) q) t = test q t
test (SequenceParser p _) t = test p t

-- Parses some tokens from a list returning the result and the list of tokens we did not parse.
runParser :: Parser a -> TokenList -> DiagnosticWriter (a, TokenList)
runParser = runParser' (const True)

-- The internal implementation of `parse`. Also takes a `Token -> Bool` retry function. If while
-- trying to parse a terminal token we run into an unrecognized token then we will call the retry
-- function. If the retry function returns true then we will skip the unrecognized token and try
-- to parse again. By default, we always retry. Unless we are parsing a sequence. In that case the
-- sequence parser will not allow us to retry if the unrecognized token is recognized as the start
-- of the parser we are sequenced with.
runParser' :: (Token -> Bool) -> Parser a -> TokenList -> DiagnosticWriter (a, TokenList)

-- Empty parser consumes no tokens and always returns its payload.
runParser' _ (EmptyParser a) tokens = return (a, tokens)

-- The terminal parser attempts to parse a single token. If we can’t immediately parse the token,
-- then we will keep trying to parse the token as long as `retry` returns true.
runParser' retry (TerminalParser expected p) tokens =
  loop Nothing tokens
  where
    loop err ts =
      case ts of
        -- If there’s a token, let’s try parsing it!
        NextToken range token ts' ->
          case p token of
            -- If we could successfully parse the token then return the range for this token, the
            -- associated data, and the remaining tokens in our list.
            --
            -- If there is an error we return `Left` instead of `Right`.
            Just x ->
              let
                result = maybe (Right (range, x)) (\e -> Left (e, Just (range, x))) err
              in
                return (result, ts')

            Nothing ->
              case token of
                -- If we failed to parse the token then check if we can retry by calling `retry`. If
                -- we can retry, then loop! (As an optimization, assume that we can always
                -- retry `UnexpectedChar`.)
                UnexpectedChar _ -> do
                  err' <- unexpectedToken range token expected
                  loop (Just (fromMaybe err' err)) ts'

                _ | retry token -> do
                  err' <- unexpectedToken range token expected
                  loop (Just (fromMaybe err' err)) ts'

                -- If we cannot retry then give up on parsing this token.
                --
                -- Only report an unexpected token error if we didn’t have another error. Since,
                -- presumably, if we can’t retry then this will be a valid token for another parser.
                _ -> do
                  err' <- maybe (unexpectedToken range token expected) return err
                  return (Left (err', Nothing), ts)

        -- If we reach the end of our token list then give up trying to parse this terminal token.
        --
        -- Only report an unexpected ending error if we didn’t have another error.
        EndToken position -> do
          err' <- maybe (unexpectedEnding (Range position position) expected) return err
          return (Left (err', Nothing), ts)

-- For our sequence parser, execute both parsers in order. The first parser may not retry a token
-- that the second parser should parse.
runParser' retry (SequenceParser p q) ts0 = do
  (f, ts1) <- runParser' (\t -> not (test q t) && retry t) p ts0
  (b, ts2) <- runParser' retry q ts1
  return (f b, ts2)
