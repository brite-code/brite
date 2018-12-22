{-# LANGUAGE Rank2Types #-}

module Brite.Parser.Framework
  ( Parser
  , TryParser
  , Recover(..)
  , runParser
  , (<&>)
  , (<|>)
  , unexpected
  , retry
  , optional
  , many
  , glyph
  , keyword
  , identifier
  , tryGlyph
  , tryKeyword
  , tryIdentifier
  , tryGlyphOnSameLine
  , CommaList(..)
  , commaList
  ) where

import Brite.Diagnostics
import Brite.Source
import Control.Applicative (liftA2)

-- The Brite parser turns a tokens into the AST of a Brite program.
--
-- The Brite parser is built from a parser combinator framework, much like Parsec. We don’t use
-- Parsec because we want to implement error recovery. Also it is more convenient to use our own
-- custom token, location, and error types.
--
-- There are two parser data types, `Parser` and `TryParser`. Both are newtypes over a function
-- which use a continuation passing style. `Parser` unconditionally parses a value which is usually
-- an `Either` or some other type which has an error case. `TryParser` might panic and throw an
-- error after looking at the current token instead of returning a value.
--
-- We use `TryParser` everywhere we need to choose between alternatives. Such as the choice operator
-- `<|>` or `many`. We use `Parser` everywhere else.
--
-- A `Parser` has three continuation functions it may call: `ok`, `yield1`, and `yield2`. Here’s
-- what they do:
--
-- * `ok`: We call `ok` when the parser has successfully parsed a value! We are absolutely sure that
--   this is the parser’s final value.
--
-- * `yield1` and `yield2`: We call one of our yield functions when we have a parsed value but we
--   aren’t absolutely sure that it’s the right value. To confirm it’s the right value we need to
--   see if the next token can be parsed by a parser we are sequenced with. This is why in our
--   sequencing operators `p1 <*> p2` and `p1 <&> p2` if `p1` calls a yield continuation and `p2`
--   calls the ok continuation we call the ok continuation for the whole parser. We “recover” from
--   `p1` yielding if `p2` is ok. If we don’t recover then we call the continuation passed to yield
--   with the next token to retry.
--
-- The difference between `yield1` and `yield2` is subtle. `yield1` is called when we want to yield
-- but we didn’t consume any tokens. `yield2` is called when we want to yield and we have consumed
-- tokens. In a sequencing operation like `p1 <*> p2`:
--
-- * If both `p1` and `p2` call `yield1` we call `yield1` with `p1`’s continuation.
-- * If `p1` calls `yield2` and `p2` calls `yield1` we call `yield2` with `p1`’s continuation.
-- * If `p1` calls `yield1` and `p2` calls `yield2` we call `yield2` with `p2`’s continuation.
-- * If both `p1` and `p2` call `yield2` we call `yield2` with `p2`’s continuation.
--
-- Another way to explain the difference between `yield1` and `yield2` is that if we call `yield1`
-- we are saying “if we are sequenced, continue from the previous parser”. Otherwise calling
-- `yield2` will continue from the current parser.
newtype Parser a = Parser
  { parser :: forall b.
         (DiagnosticWriter a -> ParserState -> b)                                -- ok
      -> (DiagnosticWriter a -> (Token -> ParserState -> b) -> ParserState -> b) -- yield1
      -> (DiagnosticWriter a -> (Token -> ParserState -> b) -> ParserState -> b) -- yield2
      -> ParserState -> b
  }

-- See the documentation for `Parser`.
--
-- `TryParser` has the ability to “panic” and throw if we are unable to parse a value. `TryParser`
-- will only throw after looking at the current token. This means our parser doesn’t have to do
-- significant backtracking when parsing.
--
-- `TryParser` does not have a `yield1` continuation, only `yield2`. This is because currently we’d
-- never use a `yield1` since `TryParser` always throws if it can’t consume input instead of
-- yielding.
--
-- Unlike `Parser` which is `Applicative`, `TryParser` does not implement `Applicative`. We only
-- allow sequencing `TryParser` with `Parser`. This is because if two `TryParser`s were sequenced
-- together and the second one threw we’d have to backtrack our parser and re-parse which
-- is innefficient!
newtype TryParser a = TryParser
  { tryParser :: forall b.
         (DiagnosticWriter a -> ParserState -> b)                                -- ok
      -> (DiagnosticWriter a -> (Token -> ParserState -> b) -> ParserState -> b) -- yield2
      -> (DiagnosticWriter Diagnostic -> ParserState -> b)                       -- throw
      -> ParserState -> b
  }

-- When parsing a value we may find some unexpected tokens. After skipping over those tokens we may
-- either be able to parse our value or we’ll be able to continue parsing something else.
data Recover a
  -- We were able to successfully parse the value.
  = Ok a
  -- We recovered from an unexpected token error. Holds the tokens we skipped, the first error
  -- diagnostic, and the recovered value.
  | Recover [Token] Diagnostic a
  -- We were unable to parse the value. Holds the tokens we skipped and the first error diagnostic.
  | Fatal [Token] Diagnostic

instance Functor Recover where
  fmap f (Ok a) = Ok (f a)
  fmap f (Recover ets e a) = Recover ets e (f a)
  fmap _ (Fatal ets e) = Fatal ets e

-- Runs a parser against against a token stream returning the parsed value and the end token for the
-- file. Thanks to our error recovery implementation our parser always returns a value!
--
-- We expect the parser to consume all tokens. If it does not then we will report an unexpected
-- token diagnostic up until the end of the file.
runParser :: Parser a -> TokenStream -> DiagnosticWriter (a, EndToken)
runParser p ts0 = parser p ok yield yield s0
  where
    s0 =
      ParserState
        { parserStep = nextToken ts0
        , parserLastLine = positionLine (tokenStreamPosition ts0)
        }

    ok a s1 = (,) <$> a <*> toEnd (parserStep s1)

    toEnd (Left t) = return t
    toEnd (Right (t, ts)) =
      unexpectedToken (tokenRange t) (tokenKind t) ExpectedEnd *> toEnd (nextToken ts)

    yield a k s =
      case parserStep s of
        Right (t, ts) -> k t (skipToken ts s)
        Left t -> (,) <$> a <*> pure t

-- The internal state of the parser.
data ParserState = ParserState
  -- The current step of the token stream.
  { parserStep :: TokenStreamStep
  -- The line number of the last token we ate. Does not count skipped error recovery tokens. We use
  -- this for disambiguating certain language constructs when the user does not type semicolons.
  , parserLastLine :: Int
  }

-- Skips the next token in the provided token stream.
skipToken :: TokenStream -> ParserState -> ParserState
skipToken ts s = s { parserStep = nextToken ts }

-- Uses the next token in the provided token stream.
--
-- `parserLastLine` is updated when calling this.
eatToken :: Token -> TokenStream -> ParserState -> ParserState
eatToken t ts s = s
  { parserStep = nextToken ts
  , parserLastLine = positionLine (rangeEnd (tokenRange t))
  }

instance Functor Parser where
  fmap f p = Parser $ \ok yield1 yield2 ->
    parser p (ok . (f <$>)) (yield1 . (f <$>)) (yield2 . (f <$>))

instance Applicative Parser where
  pure a = Parser $ \ok _ _ -> ok (pure a)

  -- The sequencing operator for two parsers. Note that while `Parser` is `Applicative` that
  -- `TryParser` is not `Applicative`.
  p1 <*> p2 = Parser $ \ok yield1 yield2 ->
    parser p1
      (\f ->
        parser p2
          (\a -> ok (f <*> a))            -- ok → ok
          (\a -> yield2 (f <*> a))        -- ok → yield1
          (\a -> yield2 (f <*> a)))       -- ok → yield2
      (\f k1 ->
        parser p2
          (\a -> ok (f <*> a))            -- yield1 → ok
          (\a _ -> yield1 (f <*> a) k1)   -- yield1 → yield1
          (\a k2 -> yield2 (f <*> a) k2)) -- yield1 → yield2
      (\f k1 ->
        parser p2
          (\a -> ok (f <*> a))            -- yield2 → ok
          (\a _ -> yield2 (f <*> a) k1)   -- yield2 → yield1
          (\a k2 -> yield2 (f <*> a) k2)) -- yield2 → yield2

instance Functor TryParser where
  fmap f p = TryParser $ \ok yield2 ->
    tryParser p (ok . (f <$>)) (yield2 . (f <$>))

infixl 4 <&>
infixl 3 <|>

-- Sequencing operator for a `TryParser` and a `Parser`. You can think of `p1 <&> p2` as `p1`
-- _and_ `p2`.
--
-- `TryParser` is not `Applicative` so we can’t use the `<*>` sequencing operator like we can with
-- `Parser`. Instead we define our own, custom, operator for sequencing `TryParser` and `Parser`.
--
-- `TryParser` is not `Applicative` because if we could sequence two `TryParser`s then we run the
-- risk of significant backtracking if the second `TryParser` in a sequence failed!
(<&>) :: TryParser (a -> b) -> Parser a -> TryParser b
p1 <&> p2 = TryParser $ \ok yield2 throw ->
  tryParser p1
    (\f ->
      parser p2
        (\a -> ok (f <*> a))            -- ok → ok
        (\a -> yield2 (f <*> a))        -- ok → yield1
        (\a -> yield2 (f <*> a)))       -- ok → yield2
    (shortcut $ \f k1 ->
      parser p2
        (\a -> ok (f <*> a))            -- yield2 → ok
        (\a _ -> yield2 (f <*> a) k1)   -- yield2 → yield1
        (\a k2 -> yield2 (f <*> a) k2)) -- yield2 → yield2
    throw

-- We know that no parser will be able to handle the `UnexpectedChar` token. So we can immediately
-- call the continuation function provided to “yield” when we see an `UnexpectedChar`.
--
-- NOTE: Does this actually help performance? Maybe it hurts performance? We should test!
shortcut :: (a -> (Token -> ParserState -> b) -> ParserState -> b) -> (a -> (Token -> ParserState -> b) -> ParserState -> b)
shortcut f a k s =
  case parserStep s of
    Right (t @ Token { tokenKind = UnexpectedChar _ }, ts) -> k t (skipToken ts s)
    _ -> f a k s
{-# INLINE shortcut #-}

-- The choice operator between two `TryParser`s. You can think of `p1 <|> p2` as `p1` _or_ `p2`.
--
-- Because `TryParser` is not `Applicative` it is also not `Alternative`. Instead we implement our
-- own choice operator.
(<|>) :: TryParser a -> TryParser a -> TryParser a
p1 <|> p2 = TryParser $ \ok yield2 throw ->
  tryParser p1 ok yield2 (\_ ->
    tryParser p2 ok yield2 throw)

-- Always reports an unexpected token diagnostic. Typically used at the end of a choice operator
-- chain to customize the error message.
unexpected :: ExpectedToken -> TryParser a
unexpected ex = TryParser $ \_ _ throw s ->
  case parserStep s of
    Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) ex) s
    Left t -> throw (unexpectedEnding (endTokenRange t) ex) s

-- Keeps retrying a `TryParser` when it throws until one of the following happens:
--
-- 1. A parser we are sequenced with succeeds on the token we failed to parse.
-- 2. We skip some tokens which no parser can handle and our parser finally succeeds.
-- 3. We reach the end of the token stream.
retry :: TryParser a -> Parser (Recover a)
retry p = Parser $ \ok yield1 yield2 ->
  let
    recover ts e1 =
      tryParser p
        (\a -> ok (Recover (reverse ts) <$> e1 <*> a))
        (\a -> yield2 (Recover (reverse ts) <$> e1 <*> a))
        (\e -> yield1 (Fatal (reverse ts) <$> e1) (\t -> recover (t : ts) (e1 <* e)))
  in
    tryParser p
      (\a -> ok (Ok <$> a))
      (\a -> yield2 (Ok <$> a))
      (\e -> yield1 (Fatal [] <$> e) (\t -> recover [t] e))

-- Optionally runs a `TryParser`.
--
-- If there are tokens no parser recongizes we skip the tokens and try to run our parser again.
optional :: TryParser a -> Parser (Maybe (Recover a))
optional p = Parser $ \ok yield1 yield2 ->
  let
    recover ts e1 =
      tryParser p
        (\a -> ok (Just <$> (Recover (reverse ts) <$> e1 <*> a)))
        (\a -> yield2 (Just <$> (Recover (reverse ts) <$> e1 <*> a)))
        (\e -> yield1 (Just . Fatal (reverse ts) <$> e1) (\t -> recover (t : ts) (e1 <* e)))
  in
    tryParser p
      (\a -> ok (Just . Ok <$> a))
      (\a -> yield2 (Just . Ok <$> a))
      (\e -> yield1 (pure Nothing) (\t -> recover [t] e))

-- Parses zero or more of a `TryParser`.
--
-- If there are tokens no parser recongizes we skip the tokens and try to run our parser again.
many :: TryParser a -> Parser [Recover a]
many p = Parser $ \_ yield1 yield2 ->
  let
    loopOk first as =
      let
        recover ts e1 =
          tryParser p
            (\a -> loopOk False (add as (Recover (reverse ts) <$> e1 <*> a)))
            (\a -> loopYield (add as (Recover (reverse ts) <$> e1 <*> a)))
            (\e -> yieldn (reverse <$> add as (Fatal (reverse ts) <$> e1)) (\t -> recover (t : ts) (e1 <* e)))
      in
        tryParser p
          (\a -> loopOk False (add as (Ok <$> a)))
          (\a -> loopYield (add as (Ok <$> a)))
          (\e -> yieldn (reverse <$> as) (\t -> recover [t] e))
      where
        yieldn = if first then yield1 else yield2

    loopYield as k1 =
      tryParser p
        (\a -> loopOk False (add as (Ok <$> a)))
        (\a k2 -> loopYield (add as (Ok <$> a)) k2)
        (\_ -> yield2 (reverse <$> as) k1)
  in
    loopOk True (pure [])
  where
    add as a = liftA2 (flip (:)) as a

-- Parses a glyph.
glyph :: Glyph -> Parser (Recover Token)
glyph = retry . tryGlyph

-- Parses a keyword.
keyword :: Keyword -> Parser (Recover Token)
keyword = retry . tryKeyword

-- Parses an identifier.
identifier :: Parser (Recover (Identifier, Token))
identifier = retry tryIdentifier

-- Tries to parse a glyph.
tryGlyph :: Glyph -> TryParser Token
tryGlyph g = TryParser $ \ok _ throw s ->
  case parserStep s of
    Right (t @ Token { tokenKind = Glyph g' }, ts) | g == g' -> ok (pure t) (eatToken t ts s)
    Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) (ExpectedGlyph g)) s
    Left t -> throw (unexpectedEnding (endTokenRange t) (ExpectedGlyph g)) s

-- Tries to parse a keyword.
tryKeyword :: Keyword -> TryParser Token
tryKeyword = tryGlyph . Keyword

-- Tries to parse an identifier.
tryIdentifier :: TryParser (Identifier, Token)
tryIdentifier = TryParser $ \ok _ throw s ->
  case parserStep s of
    Right (t @ Token { tokenKind = IdentifierToken i }, ts) -> ok (pure (i, t)) (eatToken t ts s)
    Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) ExpectedIdentifier) s
    Left t -> throw (unexpectedEnding (endTokenRange t) ExpectedIdentifier) s

-- Tries to parse a glyph, but only if that glyph is on the _same line_ as the previously
-- parsed glyph.
tryGlyphOnSameLine :: Glyph -> TryParser Token
tryGlyphOnSameLine g = TryParser $ \ok _ throw s ->
  case parserStep s of
    Right (t @ Token { tokenKind = Glyph g' }, ts) | g == g'
      && parserLastLine s == positionLine (rangeStart (tokenRange t)) -> ok (pure t) (eatToken t ts s)
    Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) (ExpectedGlyph g)) s
    Left t -> throw (unexpectedEnding (endTokenRange t) (ExpectedGlyph g)) s

data CommaList a = CommaList
  { commaListItems :: [(Recover a, Recover Token)]
  , commaListLastItem :: Maybe (Recover a)
  }

commaList :: TryParser a -> Parser (CommaList a)
commaList p = Parser $ \_ yield1 yield2 ->
  let
    loop empty acc =
      tryParser p
        (\a -> itemOk acc (Ok <$> a))
        (\a k -> itemYield acc (Ok <$> a) k)
        (\e ->
          tryComma
            (\b -> loop False (add acc ((,) <$> (Fatal [] <$> e) <*> (Ok <$> b))))
            (\_ ->
              yieldn
                (CommaList <$> (reverse <$> acc) <*> pure Nothing)
                (\t -> recover [t] e)))
      where
        recover ts e1 =
          tryParser p
            (\a -> itemOk acc (Recover (reverse ts) <$> e1 <*> a))
            (\a k -> itemYield acc (Recover (reverse ts) <$> e1 <*> a) k)
            (\e ->
              tryComma
                (\b -> loop False (add acc ((,) <$> (Fatal (reverse ts) <$> e1) <*> (Ok <$> b))))
                (\_ ->
                  yieldn
                    (CommaList <$> (reverse <$> acc) <*> (Just . Fatal (reverse ts) <$> e1))
                    (\t -> recover (t : ts) (e1 <* e))))

        yieldn = if empty then yield1 else yield2

    itemOk acc a1 =
      tryComma
        (\b -> loop False (add acc ((,) <$> a1 <*> (Ok <$> b))))
        (\e -> yield2 (CommaList <$> (reverse <$> acc) <*> (Just <$> a1)) (\t ->
          recover [t] e))
      where
        recover ts e1 =
          tryComma
            (\b -> loop False (add acc ((,) <$> a1 <*> (Recover (reverse ts) <$> e1 <*> b))))
            (\e ->
              tryParser p
                (\a2 -> itemOk (add acc ((,) <$> a1 <*> (Fatal (reverse ts) <$> e1))) (Ok <$> a2))
                (\a2 k -> itemYield (add acc ((,) <$> a1 <*> (Fatal (reverse ts) <$> e1))) (Ok <$> a2) k)
                (\_ ->
                  yield2
                    (let acc' = add acc ((,) <$> a1 <*> (Fatal (reverse ts) <$> e1)) in
                      CommaList <$> (reverse <$> acc') <*> pure Nothing)
                    (\t -> recover (t : ts) (e1 <* e))))

    itemYield acc a1 k1 =
      tryComma
        (\b -> loop False (add acc ((,) <$> a1 <*> (Ok <$> b))))
        (\e ->
          tryParser p
            (\a2 -> itemOk (add acc ((,) <$> a1 <*> (Fatal [] <$> e))) (Ok <$> a2))
            (\a2 k2 -> itemYield (add acc ((,) <$> a1 <*> (Fatal [] <$> e))) (Ok <$> a2) k2)
            (\_ ->
              yield2 (CommaList <$> (reverse <$> acc) <*> (Just <$> a1)) k1))
  in
    loop True (pure [])
  where
    add as a = liftA2 (flip (:)) as a

    tryComma :: (DiagnosticWriter Token -> ParserState -> b) -> (DiagnosticWriter Diagnostic -> ParserState -> b) -> ParserState -> b
    tryComma ok throw s =
      case parserStep s of
        Right (t @ Token { tokenKind = Glyph Comma }, ts) -> ok (pure t) (eatToken t ts s)
        Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) (ExpectedGlyph Comma)) s
        Left t -> throw (unexpectedEnding (endTokenRange t) (ExpectedGlyph Comma)) s
