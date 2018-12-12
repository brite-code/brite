{-# LANGUAGE Rank2Types #-}

module Brite.Parser.Framework3
  ( Parser
  , runParser
  , glyph
  , keyword
  , identifier
  ) where

import Prelude hiding (sequence)
import Brite.Diagnostics
import Brite.Source

newtype Parser a = Parser
  { unParser :: forall b. TokenList -> ParserOk a b -> ParserErr a b -> DiagnosticWriter b
  }

type ParserOk a b = a -> TokenList -> DiagnosticWriter b

type ParserErr a b = a -> TokenList -> (TokenList -> DiagnosticWriter b) -> DiagnosticWriter b

instance Functor Parser where
  fmap f p = Parser (\ts0 ok err ->
    unParser p ts0
      (\x ts1 -> ok (f x) ts1)
      (\x ts1 k -> err (f x) ts1 k))

instance Applicative Parser where
  pure a = Parser (\ts ok _ -> ok a ts)
  (<*>) = sequence

runParser :: Parser a -> TokenList -> DiagnosticWriter a
runParser p ts0 =
  unParser p ts0
    (\a _ -> return a)
    (\a ts1 k ->
      case ts1 of
        NextToken _ _ ts2 -> k ts2
        EndToken _ -> return a)

glyph :: Glyph -> Parser (Either Diagnostic Range)
glyph g = fmap (fmap fst) $ terminal (ExpectedGlyph g) $ \t ->
  case t of
    Glyph g' | g == g' -> Just ()
    _ -> Nothing

keyword :: Keyword -> Parser (Either Diagnostic Range)
keyword k = glyph (Keyword k)

identifier :: Parser (Either Diagnostic (Range, Identifier))
identifier = terminal ExpectedIdentifier $ \t ->
  case t of
    IdentifierToken ident -> Just ident
    _ -> Nothing

terminal :: ExpectedToken -> (Token -> Maybe a) -> Parser (Either Diagnostic (Range, a))
terminal ex parse = Parser run
  where
    run _ts ok err = try _ts
      where
        try ts =
          case ts of
            NextToken r t ts' ->
              case parse t of
                Just a -> ok (Right (r, a)) ts'
                Nothing -> do
                  e <- unexpectedToken r t ex
                  case t of
                    UnexpectedChar _ -> try ts' -- If we see an unexpected character we assume no one can handle it, so try again.
                    _ -> err (Left e) ts try

            EndToken p -> do
              e <- unexpectedEnding (Range p p) ex
              err (Left e) ts try

sequence :: Parser (a -> b) -> Parser a -> Parser b
sequence p1 p2 = Parser $ \ts0 ok err ->
  unParser p1 ts0                      -- 1) Run the first parser.
    (\f ts1 ->                         -- 2) If the first parser succeeds:
      unParser p2 ts1                  --    2.1) Run the second parser.
        (\a ts2 -> ok (f a) ts2)       --    2.2) If the second parser succeeds we’re all good!
        (\a ts2 k -> err (f a) ts2 k)) --    2.3) If the second parser errs we err with the second continuation.
    (\f ts1 k ->                       -- 3) If the first parser errs:
      unParser p2 ts1                  --    3.1) Run the second parser.
        (\a ts2 -> ok (f a) ts2)       --    3.2) If the second parser succeeds we recover with the first parser’s error value.
        (\a ts2 _ -> err (f a) ts2 k)) --    3.3) If the second parser errs we err with the first continuation so we resume there.
