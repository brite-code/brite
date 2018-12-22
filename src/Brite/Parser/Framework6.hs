{-# LANGUAGE Rank2Types #-}

module Brite.Parser.Framework6
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
  ) where

import Brite.Diagnostics
import Brite.Source
import Control.Applicative (liftA2)

newtype Parser a = Parser
  { parser :: forall b.
         (DiagnosticWriter a -> ParserState -> b)                                -- ok
      -> (DiagnosticWriter a -> (Token -> ParserState -> b) -> ParserState -> b) -- yield1
      -> (DiagnosticWriter a -> (Token -> ParserState -> b) -> ParserState -> b) -- yield2
      -> ParserState -> b
  }

newtype TryParser a = TryParser
  { tryParser :: forall b.
         (DiagnosticWriter a -> ParserState -> b)                                -- ok
      -> (DiagnosticWriter a -> (Token -> ParserState -> b) -> ParserState -> b) -- yield2
      -> (DiagnosticWriter Diagnostic -> ParserState -> b)                       -- throw
      -> ParserState -> b
  }

data Recover a
  = Ok a
  | Recover [Token] Diagnostic a
  | Fatal [Token] Diagnostic

instance Functor Recover where
  fmap f (Ok a) = Ok (f a)
  fmap f (Recover ets e a) = Recover ets e (f a)
  fmap _ (Fatal ets e) = Fatal ets e

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

data ParserState = ParserState
  { parserStep :: TokenStreamStep
  , parserLastLine :: Int
  }

-- Skips the next token in the provided token stream.
skipToken :: TokenStream -> ParserState -> ParserState
skipToken ts s = s { parserStep = nextToken ts }

-- Uses the next token in the provided token stream.
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

shortcut :: (a -> (Token -> ParserState -> b) -> ParserState -> b) -> (a -> (Token -> ParserState -> b) -> ParserState -> b)
shortcut f a k s =
  case parserStep s of
    Right (t @ Token { tokenKind = UnexpectedChar _ }, ts) -> k t (skipToken ts s)
    _ -> f a k s
{-# INLINE shortcut #-}

(<|>) :: TryParser a -> TryParser a -> TryParser a
p1 <|> p2 = TryParser $ \ok yield2 throw ->
  tryParser p1 ok yield2 (\_ ->
    tryParser p2 ok yield2 throw)

unexpected :: ExpectedToken -> TryParser a
unexpected ex = TryParser $ \_ _ throw s ->
  case parserStep s of
    Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) ex) s
    Left t -> throw (unexpectedEnding (endTokenRange t) ex) s

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

many :: TryParser a -> Parser [Recover a]
many p = Parser $ \_ yield1 yield2 ->
  let
    loopOk first as =
      let
        recover ts e1 =
          tryParser p
            (\a -> loopOk False (add as (Recover (reverse ts) <$> e1 <*> a)))
            (\a -> loopYield2 (add as (Recover (reverse ts) <$> e1 <*> a)))
            (\e -> yieldn (reverse <$> add as (Fatal (reverse ts) <$> e1)) (\t -> recover (t : ts) (e1 <* e)))
      in
        tryParser p
          (\a -> loopOk False (add as (Ok <$> a)))
          (\a -> loopYield2 (add as (Ok <$> a)))
          (\e -> yieldn (reverse <$> as) (\t -> recover [t] e))
      where
        yieldn = if first then yield1 else yield2

    loopYield2 as k1 =
      tryParser p
        (\a -> loopOk False (add as (Ok <$> a)))
        (\a k2 -> loopYield2 (add as (Ok <$> a)) k2)
        (\_ -> yield2 (reverse <$> as) k1)
  in
    loopOk True (pure [])
  where
    add as a = liftA2 (flip (:)) as a

glyph :: Glyph -> Parser (Recover Token)
glyph = retry . tryGlyph

keyword :: Keyword -> Parser (Recover Token)
keyword = retry . tryKeyword

identifier :: Parser (Recover (Identifier, Token))
identifier = retry tryIdentifier

tryGlyph :: Glyph -> TryParser Token
tryGlyph g = TryParser $ \ok _ throw s ->
  case parserStep s of
    Right (t @ Token { tokenKind = Glyph g' }, ts) | g == g' -> ok (pure t) (eatToken t ts s)
    Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) (ExpectedGlyph g)) s
    Left t -> throw (unexpectedEnding (endTokenRange t) (ExpectedGlyph g)) s

tryKeyword :: Keyword -> TryParser Token
tryKeyword = tryGlyph . Keyword

tryIdentifier :: TryParser (Identifier, Token)
tryIdentifier = TryParser $ \ok _ throw s ->
  case parserStep s of
    Right (t @ Token { tokenKind = IdentifierToken i }, ts) -> ok (pure (i, t)) (eatToken t ts s)
    Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) ExpectedIdentifier) s
    Left t -> throw (unexpectedEnding (endTokenRange t) ExpectedIdentifier) s

tryGlyphOnSameLine :: Glyph -> TryParser Token
tryGlyphOnSameLine g = TryParser $ \ok _ throw s ->
  case parserStep s of
    Right (t @ Token { tokenKind = Glyph g' }, ts) | g == g'
      && parserLastLine s == positionLine (rangeStart (tokenRange t)) -> ok (pure t) (eatToken t ts s)
    Right (t, _) -> throw (unexpectedToken (tokenRange t) (tokenKind t) (ExpectedGlyph g)) s
    Left t -> throw (unexpectedEnding (endTokenRange t) (ExpectedGlyph g)) s
