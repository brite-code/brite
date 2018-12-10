{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Brite.Parser.Framework
  ( Parser
  , glyph
  , keyword
  , parse
  ) where

import Brite.Source

-- Parses some tokens from a `TokenList` returning `a`.
data Parser a where
  -- A terminal parser may only parse a single token. It takes a test function and if the test
  -- function returns true our parser returns `Just` with the token’s range.
  TerminalParser :: (Token -> Bool) -> Parser (Maybe Range)
  -- An empty parser never parses any tokens. It always returns its payload.
  EmptyParser :: a -> Parser a
  -- A sequence parser combines two parsers to be executed one after another. The first parser
  -- should return a function which we can apply with the result of the second parser.
  SequenceParser :: Parser (b -> a) -> Parser b -> Parser a

instance Functor Parser where
  -- To map a terminal parser we sequence it with an empty parser that returns the mapper function.
  fmap f p@(TerminalParser _) = SequenceParser (EmptyParser f) p

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
glyph :: Glyph -> Parser (Maybe Range)
glyph g = TerminalParser (\case
  GlyphToken g' -> g == g'
  _ -> False)

-- Keyword parser. Returns the range that was parsed if we could parse the glyph.
keyword :: Keyword -> Parser (Maybe Range)
keyword k = glyph (Keyword k)

-- Tests if the token is the _first_ token of the parser.
test :: Parser a -> Token -> Bool
test (TerminalParser p) t = p t
test (EmptyParser _) _ = False
test (SequenceParser (EmptyParser _) q) t = test q t
test (SequenceParser p _) t = test p t

-- Parses some tokens from a list returning the result and the list of tokens we did not parse.
parse :: Parser a -> TokenList -> (a, TokenList)
parse = parse' (const True)

-- The internal implementation of `parse`. Also takes a `Token -> Bool` retry function. If while
-- trying to parse a terminal token we run into an unrecognized token then we will call the retry
-- function. If the retry function returns true then we will skip the unrecognized token and try
-- to parse again. By default, we always retry. Unless we are parsing a sequence. In that case the
-- sequence parser will not allow us to retry if the unrecognized token is recognized as the start
-- of the parser we are sequenced with.
parse' :: (Token -> Bool) -> Parser a -> TokenList -> (a, TokenList)

-- Empty parser consumes no tokens and always returns its payload.
parse' _ (EmptyParser a) tokens = (a, tokens)

-- The terminal parser attempts to parse its list of tokens...
parse' retry (TerminalParser p) tokens =
  loop tokens
  where
    loop ts =
      case ts of
        -- If the next token matches our terminal parser then eat that token and return!
        NextToken range token ts' | p token -> (Just range, ts')

        -- Always retry if we see an unexpected character. We don’t have to call `retry` to
        -- know that.
        NextToken _ (UnexpectedChar _) ts' -> loop ts'

        -- Retry if calling `retry` returns true.
        NextToken _ token ts' | retry token -> loop ts'

        -- Otherwise we are not allowed to retry. Abort trying to parse this terminal.
        _ -> (Nothing, ts)

-- For our sequence parser, execute both parsers in order. The first parser may not retry a token
-- that the second parser should parse.
parse' retry (SequenceParser p q) ts0 =
  let
    (f, ts1) = parse' (\t -> not (test q t) && retry t) p ts0
    (b, ts2) = parse' retry q ts1
  in
    (f b, ts2)
