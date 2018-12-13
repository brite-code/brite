{-# LANGUAGE Rank2Types #-}

module Brite.Parser.Framework
  ( Parser
  , runParser
  , (<|>)
  , glyph
  , keyword
  , identifier
  ) where

import Prelude hiding (sequence)
import Brite.Diagnostics
import Brite.Source
import Data.Maybe

-- Parsers turn a stream of tokens into usable data for our program.
--
-- This parsing framework is built for parsing Brite source code. That is accepting a text document
-- and turning it into a Brite AST. More specifically, this parsing framework accepts a stream of
-- tokens which is created from a text document.
--
-- Our parsing framework also does something novel. It implements sophisticated error recovery. That
-- means small errors in the source document won’t stop the parser from producing a reasonable
-- Brite AST.
--
-- The implementation of this parsing framework is heavily inspired by the implementation of Parsec.
-- Just like Parsec our parser is an abstraction over a continuation-passing function.
--
-- Here’s how error recovery works:
--
-- * If we come accross an unrecognized token we ask our sibling parsers if they recognize
--   the token.
-- * If someone recognizes the token we return an error value for the failed parser and resume from
--   the successful parser.
-- * If no one recognizes the token then we skip the token and try our original parser again on the
--   next token.
newtype Parser a = Parser
  { unParser :: forall b. Mode -> TokenList -> ParserOk a b -> ParserErr a b -> DiagnosticWriter b
  }

data Mode = Normal | Speculative

-- The “ok” continuation for our parser.
--
-- If the parser was successful it calls this function with the remaining tokens stream and
-- the parsed value.
type ParserOk a b = Mode -> TokenList -> a -> DiagnosticWriter b

-- The “error” continuation for our parser.
--
-- If the parser was unsuccessful it calls this function with the reamining tokens stream, the
-- parsed value, and a continuation function.
--
-- The parsed value is inside the `DiagnosticWriter` monad. This is because we report diagnostics
-- when constructing the parsed error value, but if we retry the failed parser we throw away the
-- temporary parsed error value and so we also want to throw its reported diagnostics away. If we
-- end up using the parsed error value then its diagnostics are reported.
--
-- The error continuation also takes a continuation function as a parameter itself! This
-- continuation function is called to retry parsing after we skip a token.
type ParserErr a b = TokenList -> DiagnosticWriter a -> ParserErrRetry b -> DiagnosticWriter b

-- See the documentation of `ParserErr`. This is a continuation function which allows us to retry a
-- parser that failed.
type ParserErrRetry b = TokenList -> DiagnosticWriter b

instance Functor Parser where
  fmap f p = Parser (\m ts0 ok err ->
    unParser p m ts0
      (\m' ts1 x -> ok m' ts1 (f x))
      (\ts1 x k -> err ts1 (f <$> x) k))

instance Applicative Parser where
  pure a = Parser (\m ts ok _ -> ok m ts a)
  (<*>) = sequence

infixr 1 <|>

-- Tries to parse the first parser. If we can’t then parse the second parser.
--
-- `Control.Applicative.Alternative` has an operator of the same name which does basically the same
-- thing. Chooses between two alternatives. However, we don’t have a way to express the empty state
-- required by that type class since error states are decided on a parser-by-parser basis.
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = choose

-- Run a parser against a stream of tokens returning the parsed value and diagnostics reported
-- during the process.
--
-- This does not necessarily parse the entire token list! We do not return the tokens that were
-- not parsed.
runParser :: Parser a -> TokenList -> DiagnosticWriter a
runParser p ts0 =
  unParser p Normal ts0
    (\_ _ a -> return a)
    (\ts1 a k ->
      case ts1 of
        NextToken _ _ ts2 -> k ts2
        EndToken _ -> a)

-- Parses a glyph. If successful we will return `Right` the range of the glyph. If unsuccessful we
-- will return `Left` with a failure diagnostic.
glyph :: Glyph -> Parser (Either Diagnostic Range)
glyph g = fmap (fmap fst) $ terminal (ExpectedGlyph g) $ \t ->
  case t of
    Glyph g' | g == g' -> Just ()
    _ -> Nothing

-- Parses a keyword. If successful we will return `Right` the range of the keyword. If unsuccessful
-- we will return `Left` with a failure diagnostic.
keyword :: Keyword -> Parser (Either Diagnostic Range)
keyword k = glyph (Keyword k)

-- Parses an identifier. If successful we will return `Right` with the identifier and the range of
-- the identifier. If unsuccessful we will return `Left` with a failure diagnostic.
identifier :: Parser (Either Diagnostic (Range, Identifier))
identifier = terminal ExpectedIdentifier $ \t ->
  case t of
    IdentifierToken ident -> Just ident
    _ -> Nothing

-- A utility function for creating parsers which only parse a single token. The name is inspired by
-- the vocabulary of [Context Free Grammars][1].
--
-- [1]: https://en.wikipedia.org/wiki/Context-free_grammar
terminal :: ExpectedToken -> (Token -> Maybe a) -> Parser (Either Diagnostic (Range, a))
terminal ex parse = Parser (\_ ts ok err -> run Nothing ts ok err)
  where
    run e ts1 ok err =
      case ts1 of
        NextToken r t ts2 ->
          case parse t of
            Just a -> ok Normal ts2 $ Right (r, a)
            Nothing ->
              case t of
                -- If we see an unexpected character we assume no one can handle it, so report
                -- an error and immediately try again.
                --
                -- Call `run` with the first reported error.
                UnexpectedChar _ -> do
                  e' <- unexpectedToken r t ex
                  run (Just (fromMaybe e' e)) ts2 ok err

                _ ->
                  -- Call the error callback so that we may attempt recovery. The error value either
                  -- uses the first reported error while calling `run` or it reports its own error.
                  --
                  -- If the continuation is called then we report an error and call `run` with the
                  -- first reported error.
                  err ts1
                    (Left <$> maybe (unexpectedToken r t ex) return e)
                    (\ts3 -> do
                      e' <- unexpectedToken r t ex
                      run (Just (fromMaybe e' e)) ts3 ok err)

        EndToken p ->
          -- Call the error callback so that we may attempt recovery. The error value either uses
          -- the first reported error while calling `run` or it reports its own error.
          --
          -- The continuation should never be used since we’re at the end. There are no more tokens
          -- to retry.
          err ts1
            (Left <$> maybe (unexpectedEnding (Range p p) ex) return e)
            (error "unreachable")

-- Sequences two parsers one after another. We execute the first parser to get a function for
-- constructing the second parser.
--
-- If the first parser fails but the second parser succeeds then we recover from the first
-- parser’s error.
sequence :: Parser (a -> b) -> Parser a -> Parser b
sequence p1 p2 = Parser $ \m0 ts0 ok err ->
  -- Run the first parser.
  unParser p1 m0 ts0

    -- If the first parser succeeds:
    (\m1 ts1 f ->
      -- Run the second parser. If it succeeds call our “ok” callback. If it fails, call our
      -- “err” callback.
      unParser p2 m1 ts1
        (\m2 ts2 a -> ok m2 ts2 (f a))
        (\ts2 a k -> err ts2 (f <$> a) k))

    -- If the first parser fails:
    (\ts1 f k ->
      case m0 of
        -- If we are in speculative mode and we error _don’t_ run the second parser and _don’t_ try
        -- to recover from the error. We also don’t provide any arguments to `err`. We expect that
        -- speculative mode won’t need them.
        Speculative ->
          err (error "speculative") (error "speculative") (error "speculative")

        Normal ->
          -- Run the second parser. If it suceeds we can recover from our first parser’s error! If
          -- it fails, call our “err” callback.
          unParser p2 Normal ts1
            (\m1 ts2 a -> f >>= \f' -> ok m1 ts2 (f' a))
            (\ts2 a _ -> err ts2 (f <*> a) k))

-- Tries to parse the first parser. If we can’t then parse the second parser.
choose :: Parser a -> Parser a -> Parser a
choose p1 p2 = Parser $ \m ts ok err ->
  unParser p1 Speculative ts ok
    (\_ _ _ -> unParser p2 m ts ok err)
