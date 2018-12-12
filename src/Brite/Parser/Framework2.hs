{-# LANGUAGE DeriveGeneric #-}

module Brite.Parser.Framework2
  () where

import Brite.Diagnostics
import Brite.Source
import Data.Hashable (Hashable)
import qualified Data.HashSet as Set
import Data.Maybe
import GHC.Generics (Generic)

data Parser a = Parser
  { runParser' :: Set.HashSet TokenKind -> TokenList -> DiagnosticWriter (a, TokenList)
  }

glyph :: Glyph -> Parser (Either Diagnostic Range)
glyph g =
  let
    p = terminal (ExpectedGlyph g) (\token ->
      case token of
        Glyph g' | g == g' -> Just ()
        _ -> Nothing)
  in
    Parser (\retry tokens ->
      fmap
        (\(x, ts) -> (either (\(e, _) -> Left e) (\(r, _) -> Right r) x, ts))
        (runParser' p retry tokens))

keyword :: Keyword -> Parser (Either Diagnostic Range)
keyword = glyph . Keyword

identifier :: Parser (Either (Diagnostic, Maybe (Range, Identifier)) (Range, Identifier))
identifier = terminal ExpectedIdentifier (\token ->
  case token of
    IdentifierToken ident -> Just ident
    _ -> Nothing)

terminal :: ExpectedToken -> (Token -> Maybe a) -> Parser (Either (Diagnostic, Maybe (Range, a)) (Range, a))
terminal expected parse = Parser (loop Nothing)
  where
    loop err retry tokens =
      case tokens of
        NextToken range token tokens' ->
          case parse token of
            Just a -> return
              ( maybe (Right (range, a)) (\e -> Left (e, Just (range, a))) err
              , tokens'
              )

            Nothing ->
              if shouldRetry token retry then do
                err' <- unexpectedToken range token expected
                loop (Just (fromMaybe err' err)) retry tokens'
              else do
                err' <- maybe (unexpectedToken range token expected) return err
                return (Left (err', Nothing), tokens)

        EndToken position -> do
          err' <- maybe (unexpectedEnding (Range position position) expected) return err
          return (Left (err', Nothing), tokens)

sequence :: Parser (a -> b) -> Parser a -> Parser b
sequence p1 p2 = Parser $ \retry ts0 -> do
  (f, ts1) <- runParser' p1 retry ts0
  (a, ts2) <- runParser' p2 retry ts1
  return (f a, ts2)

data TokenKind = GlyphTokenKind Glyph | IdentifierTokenKind
  deriving (Eq, Generic)

instance Hashable TokenKind

shouldRetry :: Token -> Set.HashSet TokenKind -> Bool
shouldRetry (Glyph g) retry = Set.member (GlyphTokenKind g) retry
shouldRetry (IdentifierToken _) retry = Set.member IdentifierTokenKind retry
shouldRetry (UnexpectedChar _) _ = False
