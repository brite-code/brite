{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Brite.Parser.Framework
  ( Parser
  , ParseResult
  , glyph
  , keyword
  , identifier
  , parse
  ) where

import Brite.Diagnostics
import Brite.Source
import Data.Maybe

-- The result of a parser is a doozy: `Either (Diagnostic, Maybe a) a`. We return `Right` if we were
-- able to successfully parse the terminal. We return `Left` if we were not. `Left` also contains
-- the diagnostic we reported so that we can use it to crash at runtime. However, `Left` _also_
-- contains `Maybe (Range, a)`. We might have been able to recover from our error. If so then we
-- will have `Just`.
type ParseResult a = Either (Diagnostic, Maybe a) a

-- Parses some tokens from a `TokenList` returning `a`.
data Parser a where
  -- A terminal parser may only parse a single token. It takes a function which tries to parse a
  -- single token. If that function returns `Just` we know our terminal parser was successful.
  --
  -- Also contains `ExpectedToken` for error reporting.
  TerminalParser :: ExpectedToken -> (Token -> Maybe b) -> Parser (Result (Range, a))
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

-- Glyph parser. Returns the range that was parsed if we could parse the glyph.
glyph :: Glyph -> Parser (Maybe (Range, ()))
glyph g = TerminalParser (ExpectedGlyph g) (\case
  Glyph g' | g == g' -> Just ()
  _ -> Nothing)

-- Keyword parser. Returns the range that was parsed if we could parse the glyph.
keyword :: Keyword -> Parser (Maybe (Range, ()))
keyword k = glyph (Keyword k)

-- Identifier parser. Returns the identifier along with the range of the identifier.
identifier :: Parser (Maybe (Range, Identifier))
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
parse :: DiagnosticMonad m => Parser a -> TokenList -> m (a, TokenList)
parse = parse' (const True)

-- The internal implementation of `parse`. Also takes a `Token -> Bool` retry function. If while
-- trying to parse a terminal token we run into an unrecognized token then we will call the retry
-- function. If the retry function returns true then we will skip the unrecognized token and try
-- to parse again. By default, we always retry. Unless we are parsing a sequence. In that case the
-- sequence parser will not allow us to retry if the unrecognized token is recognized as the start
-- of the parser we are sequenced with.
parse' :: DiagnosticMonad m => (Token -> Bool) -> Parser a -> TokenList -> m (a, TokenList)

-- Empty parser consumes no tokens and always returns its payload.
parse' _ (EmptyParser a) tokens = return (a, tokens)

-- The terminal parser attempts to parse a single token. If we can’t immediately parse the token,
-- then we will keep trying to parse the token as long as `retry` returns true.
parse' retry (TerminalParser expected p) tokens =
  loop Nothing tokens
  where
    loop error ts =
      case ts of
        -- If there’s a token, let’s try parsing it!
        NextToken range token ts' ->
          case p token of
            -- If we could successfully parse the token then return the range for this token, the
            -- associated data, and the remaining tokens in our list.
            --
            -- If there is an error we return `Left` instead of `Right`.
            Just x ->
              return (maybe (Right (range, x)) (\e -> Left (e, Just (range, x))) error, ts')

            Nothing ->
              case token of
                -- If we failed to parse the token then check if we can retry by calling `retry`. If
                -- we can retry, then loop! (As an optimization, assume that we can always
                -- retry `UnexpectedChar`.)
                UnexpectedChar _ -> do
                  error' <- unexpectedToken range token expected
                  loop (Just (fromMaybe error' error)) ts'

                _ | retry token -> do
                  error' <- unexpectedToken range token expected
                  loop (Just (fromMaybe error' error)) ts'

                -- If we cannot retry then give up on parsing this token.
                --
                -- Only report an unexpected token error if we didn’t have another error. Since,
                -- presumably, if we can’t retry then this will be a valid token for another parser.
                _ -> do
                  error' <- maybe (unexpectedToken (Range position position) token expected) return error
                  return (Left (error', Nothing), ts)

        -- If we reach the end of our token list then give up trying to parse this terminal token.
        --
        -- Only report an unexpected ending error if we didn’t have another error.
        EndToken position -> do
          error' <- maybe (unexpectedEnding (Range position position) expected) return error
          return (Left (error', Nothing), ts)

-- For our sequence parser, execute both parsers in order. The first parser may not retry a token
-- that the second parser should parse.
parse' retry (SequenceParser p q) ts0 = do
  (f, ts1) <- parse' (\t -> not (test q t) && retry t) p ts0
  (b, ts2) <- parse' retry q ts1
  return (f b, ts2)
