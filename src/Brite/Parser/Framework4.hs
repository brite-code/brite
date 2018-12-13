{-# LANGUAGE Rank2Types #-}

module Brite.Parser.Framework4
  ( Parser
  , runParser
  , retry
  , optional
  , glyph
  , keyword
  , identifier
  ) where

import Brite.Diagnostics
import Brite.Source
import Control.Applicative hiding (optional)
import Data.Maybe

-- The Brite parser turns a stream of tokens into the AST of a Brite program. The Brite parser is
-- build from a parser combinator framework much like Parsec. We do this because the Brite parser
-- implements the advanced feature of error recovery. The framework abstracts away complex error
-- recovery behavior.
newtype Parser a = Parser { unParser :: forall b. ParserRun a b }

-- The monad in which our parser runs. Our parser needs to report diagnostics as it discovers them.
-- Unlike traditional parsers which panic when they hit the first error, the Brite parser will
-- attempt to recover from every error. Meaning we’ll have many diagnostics.
type ParserMonad = DiagnosticWriter

-- The Brite parsing framework is implemented as a function which calls a continuation function on
-- completion. A parser is usually constructed like so:
--
-- ```hs
-- Parser $ \ts err ok skip ->
--   *Insert parsing code here*
-- ```
--
-- `Parser` is merely a newtype wrapper around a function which takes four arguments.
--
-- * The first argument is the token stream we are parsing.
--
-- * The second argument is an error callback. We call this function with an error diagnostic when
--   we can’t parse the token stream and need to panic.
--
-- * The third argument is a success callback. We call this function with a parsed value and the
--   reamining token stream when we were able to successfully parse the token stream.
--
-- * The fourth argument is interesting it allows us to “skip” the current parser and let another
--   parser try to process the token list. This function is how we implement error recovery. The
--   skip function takes a value which we will return if we end up skipping the parser. However, if
--   no other parsers can handle the remaining token stream we want to retry our parser from the
--   next token.
--
-- See the types below for more information on each callback.
type ParserRun a b = TokenList -> ParserErr b -> ParserOk a b -> ParserSkip a b -> ParserMonad b

-- The error callback is called when our parser can’t handle the token stream. Effectively this
-- function “throws” an error. We will recover in one of two ways after a throw:
--
-- 1. The nearest choice operator (`<|>`) will catch the error and will try the next parser. So in
--    `p <|> q` if `p` throws by calling this error callback we will ignore the error and try to
--    parse `q`.
--
-- 2. The nearest `retry` call will catch the error and will try to let other parsers handle the
--    token stream. If no other parser can handle the stream then `retry` will skip the token that
--    caused its parser to error and will try again on the next token. So in `retry p <*> q` if `p`
--    throws by calling this error callback we will see if `q` can handle the token. If so then we
--    will return an error value (`Left`) for `p` and parse `q`. If `q` cannot handle the token then
--    we will skip the current token and attempt to parse `p` again with the new token stream.
type ParserErr b = ParserMonad Diagnostic -> ParserMonad b

-- The ok callback is called when our parser successfully parses a value.
type ParserOk a b = TokenList -> a -> ParserMonad b

-- The “skip” callback is called when we are in error recovery mode. The skip callback is a lot like
-- the ok callback with two major differences:
--
-- 1. The skip callback takes a continuation as its third argument. If no parser matches a token
--    stream then we advance the token stream to the next token and call the continuation to retry
--    the parser that _first_ called the skip callback.
--
-- 2. The result may have some deferred diagnostic reporting. Which is why it is wrapped in a
--    `ParserMonad`. These diagnostics are not reported until we recover and use the value which
--    only happens when another parser can parse the token stream.
--
-- You can think of the second and third arguments as “recover” and “retry” respectively. If we
-- “recover” from an error then we use the second argument. If we advance the token stream and want
-- to “retry” then we call the continuation passed as a third argument.
type ParserSkip a b = TokenList -> ParserMonad a -> (TokenList -> ParserMonad b) -> ParserMonad b

-- Parsers are functors. We can map the value they carry.
instance Functor Parser where
  fmap f p = Parser $ \ts0 err ok skip ->
    unParser p ts0 err
      (\ts1 a -> ok ts1 (f a))
      (\ts1 a k -> skip ts1 (f <$> a) k)

-- Parsers are applicatives, we can sequence them together. This is a very important combinator!
instance Applicative Parser where
  -- Always succeeds without consuming any tokens from the stream.
  pure a = Parser (\ts _ ok _ -> ok ts a)

  -- The parser sequencing operator. This operator is incredibly important to our parsing framework.
  -- It sequences two parsers `p1` and `p2` together. First we parse `p1` and then we parse `p2`.
  --
  -- In `p1 <*> p2` if `p1` fails (by calling its error callback) then the entire sequence fails. If
  -- `p2` fails then the entire sequence fails, even if `p1` succeeded!
  --
  -- In `p1 <*> p2` if `p1` goes into error recovery mode (by calling its “skip” callback) then we
  -- we try to parse `p2`. If `p2` succeeds then we recover from the error! If `p2` fails (by
  -- calling its error callback) then the entire sequence fails. If `p2` also calls its skip
  -- callback then we call the skip callback for our sequence.
  --
  -- In `p1 <*> p2` if `p1` succeeds but `p2` goes into error recovery mode (by calling its “skip”
  -- callback) then we put the entire sequence into error recovery mode (by calling the “skip”
  -- callback of the sequence).
  p1 <*> p2 = Parser $ \ts0 err ok skip ->
    unParser p1 ts0 err
      (\ts1 f ->
        unParser p2 ts1 err
          (\ts2 a -> ok ts2 (f a))
          (\ts2 a k -> skip ts2 (f <$> a) k))
      (\ts1 f k ->
        unParser p2 ts1 err
          (\ts2 a -> f >>= \f' -> ok ts2 (f' a))
          (\_ a _ -> skip ts1 (f <*> a) k))

-- Parsers are alternatives, we can choose between two alternatives. This is another very
-- important combinator!
instance Alternative Parser where
  -- The error message for `empty` is really unspecific. We will consider it a bug if the error
  -- message ever appears. Please don’t use it in production!
  empty = Parser (\ts err _ _ ->
    case ts of
      NextToken r t _ -> err (unexpectedToken r t ExpectedUnknown)
      EndToken p -> err (unexpectedEnding (Range p p) ExpectedUnknown))

  -- The parser choice operator. This operator is incredibly important to our parsing framework.
  -- It chooses between two parsers, `p1` and `p2`. First we attempt to parse `p1`. If it fails by
  -- calling its error callback then we ignore the error and we parse `p2`.
  --
  -- In `p1 <|> p2` if `p1` fails (by calling its error callback) then we parse `p2`. If `p2` also
  -- fails (by calling its error callback) then the entire parser fails with `p2`’s error.
  p1 <|> p2 = Parser $ \ts0 err ok skip ->
    unParser p1 ts0
      (\_ -> unParser p2 ts0 err ok skip)
      ok skip

-- Runs a parser against a token stream. If the parser fails then we return `Left` with a
-- diagnostic. While parsing we may report some diagnostics.
--
-- We may not parse the token stream to its end. We don’t return the remaining tokens after we
-- finish parsing.
runParser :: Parser a -> TokenList -> DiagnosticWriter (Either Diagnostic a)
runParser p ts0 =
  unParser p ts0
    (\e -> Left <$> e)
    (\_ a -> return (Right a))
    (\ts1 a k ->
      case ts1 of
        NextToken _ _ ts2 -> k ts2
        EndToken _ -> Right <$> a)

-- Keeps retrying a parser when it fails until one of the following happens:
--
-- 1. A parser we are sequenced with succeeds on the token we failed to parse then we return `Left`
--    and resume parsing from the parser which succeeded.
-- 2. We skip some tokens which no parser can handle and our parser finally succeeds. We return
--    `Right` in this case even though technically we skipped some tokens.
-- 3. We reach the end of the token stream.
retry :: Parser a -> Parser (Either Diagnostic a)
retry p = Parser (\ts _ ok skip -> run Nothing ts ok skip)
  where
    run e1 ts0 ok skip =
      unParser p ts0
        (\e2 ->
          let
            a = Left <$> maybe e2 return e1
            k ts1 = e2 >>= \e2' -> run (Just (fromMaybe e2' e1)) ts1 ok skip
          in
            case ts0 of
              -- Optimization: if the token is an unexpected character don’t bother calling `skip`
              -- since we know no one can handle an unexpected character. Instead call the
              -- continuation immediately.
              NextToken _ (UnexpectedChar _) ts1 -> k ts1
              _ -> skip ts0 a k)

        (\ts1 a -> ok ts1 (Right a))
        (\ts1 a k -> skip ts1 (Right <$> a) k)

-- Either runs our parser or does not run our parser.
--
-- `Control.Applicative` also has an operator named `optional`. It is implemented as
-- `Just <$> v <|> pure Nothing`. However, our implementation does a very similar thing but is more
-- specialized to the task at hand.
--
-- Notably, `Control.Applicative` returns `Nothing` if we can’t run the parser.
--
-- Our implementation returns `Nothing` when we know that some other parser can successfully run.
--
-- This minor difference ends up meaning a lot for error recovery. If we encounter an unexpected
-- token our optional parser will skip the unexpected token and try to optionally run our parser
-- again. The `Control.Applicative` version would see an unexpected token and give up instead of
-- skipping it.
optional :: Parser a -> Parser (Maybe a)
optional p = Parser (\ts _ ok skip -> run ts ok skip)
  where
    run ts0 ok skip =
      unParser p ts0
        (\e2 ->
          let
            a = return Nothing
            k ts1 = e2 *> run ts1 ok skip
          in
            case ts0 of
              -- Optimization: if the token is an unexpected character don’t bother calling `skip`
              -- since we know no one can handle an unexpected character. Instead call the
              -- continuation immediately.
              NextToken _ (UnexpectedChar _) ts1 -> k ts1
              _ -> skip ts0 a k)

        (\ts1 a -> ok ts1 (Just a))
        (\ts1 a k -> skip ts1 (Just <$> a) k)

-- Parses a glyph.
glyph :: Glyph -> Parser Range
glyph g = Parser $ \ts err ok _ ->
  case ts of
    NextToken r (Glyph g') ts' | g == g' -> ok ts' r
    NextToken r t _ -> err (unexpectedToken r t (ExpectedGlyph g))
    EndToken p -> err (unexpectedEnding (Range p p) (ExpectedGlyph g))

-- Parses a keyword.
keyword :: Keyword -> Parser Range
keyword = glyph . Keyword

-- Parses an identifier.
identifier :: Parser (Range, Identifier)
identifier = Parser $ \ts err ok _ ->
  case ts of
    NextToken r (IdentifierToken i) ts' -> ok ts' (r, i)
    NextToken r t _ -> err (unexpectedToken r t ExpectedIdentifier)
    EndToken p -> err (unexpectedEnding (Range p p) ExpectedIdentifier)
