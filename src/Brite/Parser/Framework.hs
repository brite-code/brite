{-# LANGUAGE Rank2Types #-}

module Brite.Parser.Framework
  ( Parser
  , (<|>)
  , runParser
  , retry
  , optional
  , many
  , unexpected
  , glyph
  , keyword
  , identifier
  , tryGlyph
  , tryKeyword
  , tryIdentifier
  ) where

import Brite.Diagnostics
import Brite.Source
import Control.Applicative (liftA2)
import qualified Data.DList as DL
import Data.Maybe

-- The Brite parser turns a stream of tokens into the AST of a Brite program. The Brite parser is
-- build from a parser combinator framework much like Parsec. We do this because the Brite parser
-- implements the advanced feature of error recovery. The framework abstracts away complex error
-- recovery behavior.
newtype Parser a = Parser { unParser :: forall b. ParserRun a b }

-- The Brite parsing framework is implemented as a function which calls a continuation function on
-- completion. A parser is usually constructed like so:
--
-- ```hs
-- Parser $ \ok yield throw ts ->
--   *Insert parsing code here*
-- ```
--
-- `Parser` is merely a newtype wrapper around a function which takes four arguments.
--
-- * The first argument is a success callback. We call this function with a parsed value and the
--   reamining token stream when we were able to successfully parse the token stream.
--
-- * The second argument is interesting it allows us to “yield” the current parser and let another
--   parser try to process the token stream. This function is how we implement error recovery. The
--   yield function takes a value which we will return if we end up skipping the parser. However, if
--   no other parsers can handle the remaining token stream we call the continuation passed
--   to yield.
--
-- * The third argument is an error callback. We call this function with an error diagnostic when
--   we can’t parse the token stream and need to panic.
--
-- * The fourth argument is the token stream we are parsing.
--
-- See the types below for more information on each callback.
type ParserRun a b = ParserOk a b -> ParserYield a b -> ParserThrow b -> TokenList -> b

-- The ok callback is called when our parser successfully parses a value.
type ParserOk a b = ParserMonad a -> TokenList -> b

-- The yield callback allows us to yield a possible parsed value. If another parser may continue
-- from the point at which we yielded then that possible value becomes the actual parsed value. If
-- no parser may continue then we err and try parsing again from the point at which we yielded.
--
-- The yield callback is a lot like the “ok” callback with two differences:
--
-- 1. The yield callback takes a continuation as its second argument. If no parser matches a token
--    stream then we advance the token stream to the next token and call the continuation to retry
--    from where we yielded.
--
-- 2. The first parameter parsing result may have some deferred diagnostic reporting. Which is why
--    it is wrapped in a `ParserMonad`. These diagnostics are not reported until we recover and use
--    the value which only happens when another parser can parse the token stream.
--
-- You can think of the first and second arguments as “recover” and “retry” respectively. If we
-- “recover” from an error then we use the first argument. If we advance the token stream and want
-- to “retry” then we call the continuation passed as the second argument.
type ParserYield a b = ParserMonad a -> ParserCont b -> TokenList -> b

-- We use a newtype to make it easier to track when continuations are created and called.
newtype ParserCont b = ParserCont { continue :: TokenList -> b }

-- The throw callback is called when our parser can’t handle the token stream. Effectively this
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
type ParserThrow b = ParserMonad Diagnostic -> TokenList -> b

-- The monad in which our parser runs. Our parser needs to report diagnostics as it discovers them.
-- Unlike traditional parsers which panic when they hit the first error, the Brite parser will
-- attempt to recover from every error. Meaning we’ll have many diagnostics.
type ParserMonad = DiagnosticWriter

-- Parsers are functors. We can map the value they carry.
instance Functor Parser where
  fmap f p = Parser $ \ok yield -> unParser p (ok . (f <$>)) (yield . (f <$>))

-- Parsers are applicatives, we can sequence them together. This is a very important combinator!
instance Applicative Parser where
  -- Always succeeds without consuming any tokens from the stream.
  pure a = Parser $ \ok _ _ -> ok (pure a)

  -- The parser sequencing operator. This operator is incredibly important to our parsing framework.
  -- It sequences two parsers `p1` and `p2` together. First we parse `p1` and then we parse `p2`.
  --
  -- In `p1 <*> p2` if `p1` throws then the entire sequence throws. If `p2` throws then the entire
  -- sequence throws, even if `p1` succeeded!
  --
  -- In `p1 <*> p2` if `p1` yields then we we try to parse `p2`. If `p2` succeeds then we use the
  -- yielded value! If `p2` throws then the entire sequence throws. If `p2` also yields then our
  -- sequence yields too. We yield with the first continuation if `p2` parsed no tokens. Otherwise
  -- we yield with the second continuation.
  --
  -- In `p1 <*> p2` if `p1` succeeds but `p2` yields then the entire sequence yields.
  p1 <*> p2 = Parser $ \ok yield throw ->
    unParser p1
      (\f -> unParser p2 (ok . (f <*>)) (yield . (f <*>)) throw)
      (\f k1 ts1 ->
        case ts1 of
          -- Optimization: if the token is an unexpected character don’t bother trying to run `p2`.
          -- We know that no parser can handle an unexpected token. Instead call the
          -- continuation immediately.
          NextToken (Token { tokenKind = UnexpectedChar _ }) ts2 -> continue k1 ts2
          _ ->
            unParser p2
              (\a ts2 -> ok (f <*> a) ts2)
              (\a k2 ts2 -> yield (f <*> a) (if sameStart ts1 ts2 then k1 else k2) ts2)
              throw ts1)
      throw

infixl 3 <|>

-- The parser choice operator. This operator is incredibly important to our parsing framework.
-- It chooses between two parsers, `p1` and `p2`. First we attempt to parse `p1`. If it throws then
-- we ignore the error and we parse `p2`.
--
-- This combinator, `optional`, and `many` exist as a part of the type class
-- `Control.Applicative.Alternative`. We don’t use `Alternative` and instead implement these
-- combinators ourselves for more domain specific logic.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser (\ok yield throw -> unParser p1 ok yield (\_ -> unParser p2 ok yield throw))

-- Runs a parser against a token stream. If the parser fails then we return `Left` with a
-- diagnostic. While parsing we may report some diagnostics.
--
-- We may not parse the token stream to its end. We don’t return the remaining tokens after we
-- finish parsing.
runParser :: Parser a -> TokenList -> DiagnosticWriter (Either Diagnostic a)
runParser p =
  unParser p
    (\a _ -> Right <$> a)
    (\a k ts1 ->
      case ts1 of
        NextToken (Token {}) ts2 -> continue k ts2
        EndToken _ -> Right <$> a)
    (\e _ -> Left <$> e)

-- Keeps retrying a parser when it fails until one of the following happens:
--
-- 1. A parser we are sequenced with succeeds on the token we failed to parse then we return `Left`
--    and resume parsing from the parser which succeeded.
-- 2. We skip some tokens which no parser can handle and our parser finally succeeds. We return
--    `Right` in this case even though technically we skipped some tokens.
-- 3. We reach the end of the token stream.
retry :: Parser a -> Parser (Either Diagnostic a)
retry p = Parser $ \ok yield _ ->
  let
    run e1 = unParser p
      (\a -> ok (maybe id (*>) e1 (Right <$> a)))
      (\a -> yield (maybe id (*>) e1 (Right <$> a)))
      (\e2 ->
        yield
          (Left <$> fromMaybe e2 e1)
          (ParserCont (run (Just (maybe e2 (<* e2) e1)))))
  in
    run Nothing

-- Either runs our parser or does not run our parser.
--
-- `Control.Applicative` also has an operator named `optional`. It is implemented as
-- `Just <$> v <|> pure Nothing`. Our implementation does a very similar thing but is more
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
optional p = Parser $ \ok yield _ ->
  let
    run es = unParser p
      (\a -> ok (es *> (Just <$> a)))
      (\a -> yield (es *> (Just <$> a)))
      (\e -> yield (es *> pure Nothing) (ParserCont (run (es <* e))))
  in
    run (pure ())

-- Applies the parser zero or more times and returns a list of the parsed values.
--
-- `Control.Applicative` also has an operator named `many`. It is implemented as roughly
-- `many p = ((:) <$> p <*> (many p)) <|> pure []`. Our implementation does a very similar thing but
-- is more specialized to the task at hand.
--
-- Notably, our implementation will not stop parsing until we reach a token which _can_ be parsed by
-- a subsequent parser. Unlike the `Control.Applicative` version which will not stop parsing until
-- we reach a token which _can’t_ be parsed by our current parser.
--
-- This parser will never fail. If nothing can be parsed we will report some diagnostics and return
-- an empty list.
many :: Parser a -> Parser [a]
many p = Parser $ \_ yield _ ->
  let
    run acc =
      unParser p
        (\a -> run (liftA2 DL.snoc acc a))
        (\a -> runYield (liftA2 DL.snoc acc a))
        (\e -> yield (DL.toList <$> acc) (ParserCont (run (acc <* e))))

    runYield acc k1 =
      unParser p
        (\a -> run (liftA2 DL.snoc acc a))
        (\a k2 -> runYield (liftA2 DL.snoc acc a) k2)
        (\_ -> yield (DL.toList <$> acc) k1)
  in
    run (return DL.empty)

-- Always fails with an unexpected token error. We can use this at the end of an alternative chain
-- to make the error message better.
unexpected :: ExpectedToken -> Parser a
unexpected ex = Parser $ \_ _ throw ts ->
  case ts of
    NextToken t _ -> throw (unexpectedToken (tokenRange t) (tokenKind t) ex) ts
    EndToken p -> throw (unexpectedEnding (Range p p) ex) ts

-- Parses a glyph.
glyph :: Glyph -> Parser (Either Diagnostic Range)
glyph = retry . tryGlyph

-- Parses a keyword.
keyword :: Keyword -> Parser (Either Diagnostic Range)
keyword = retry . tryKeyword

-- Parses an identifier.
identifier :: Parser (Either Diagnostic (Range, Identifier))
identifier = retry tryIdentifier

-- Parses a glyph. Throws an error if we fail without retrying.
--
-- Technically this function is more primitive then its counterpart `glyph`, but we fall into the
-- pit of success by adding a bit of syntactic vinegar to this name.
tryGlyph :: Glyph -> Parser Range
tryGlyph g = Parser $ \ok _ throw ts ->
  case ts of
    NextToken (Token { tokenRange = r, tokenKind = Glyph g' }) ts' | g == g' -> ok (pure r) ts'
    NextToken t _ -> throw (unexpectedToken (tokenRange t) (tokenKind t) (ExpectedGlyph g)) ts
    EndToken p -> throw (unexpectedEnding (Range p p) (ExpectedGlyph g)) ts

-- Parses a keyword. Throws an error if we fail without retrying.
--
-- Technically this function is more primitive then its counterpart `keyword`, but we fall into the
-- pit of success by adding a bit of syntactic vinegar to this name.
tryKeyword :: Keyword -> Parser Range
tryKeyword = tryGlyph . Keyword

-- Parses an identifier. Throws an error if we fail without retrying.
--
-- Technically this function is more primitive then its counterpart `identifier`, but we fall into
-- the pit of success by adding a bit of syntactic vinegar to this name.
tryIdentifier :: Parser (Range, Identifier)
tryIdentifier = Parser $ \ok _ throw ts ->
  case ts of
    NextToken (Token { tokenRange = r, tokenKind = IdentifierToken i }) ts' -> ok (pure (r, i)) ts'
    NextToken t _ -> throw (unexpectedToken (tokenRange t) (tokenKind t) ExpectedIdentifier) ts
    EndToken p -> throw (unexpectedEnding (Range p p) ExpectedIdentifier) ts

-- Determines if two token streams start at the same position. We use this to determine if a parser
-- parsed something or nothing.
sameStart :: TokenList -> TokenList -> Bool
sameStart (NextToken t1 _) (NextToken t2 _) = rangeStart (tokenRange t1) == rangeStart (tokenRange t2)
sameStart (NextToken t1 _) (EndToken p2) = rangeStart (tokenRange t1) == p2
sameStart (EndToken p1) (NextToken t2 _) = p1 == rangeStart (tokenRange t2)
sameStart (EndToken p1) (EndToken p2) = p1 == p2
