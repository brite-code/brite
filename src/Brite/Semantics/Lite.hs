-- Welcome to the home of Brite Lite! Lite is a syntax inspired by academic tradition which we use
-- for testing the type checker. Specifically the syntax of the [MLF thesis][1] and related papers.
--
-- No Brite programmer should ever end seeing Lite. It is only a useful tool for compiler hackers
-- and academics to describe the type checking properties of Brite. Lite directly parses an AST
-- avoiding the need to go through the CST.
--
-- There may be features in Brite that we don’t support in Lite.
--
-- And yes, we use Parsec, even though we have our own industrial grade parser which supports error
-- recovery in `Brite.Syntax.ParserFramework`. We don’t care about error recovery for this parser
-- since a Brite programmer will never right in Lite.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf

module Brite.Semantics.Lite () where

import Brite.Semantics.AST
import Brite.Syntax.Tokens (Position(..))
import Data.Functor.Identity
import Text.Parsec hiding (Parsec)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

type Parsec a = ParsecT String () Identity a

range :: Parsec a -> Parsec (Range, a)
range ma = do
  start <- getPosition
  a <- ma
  end <- getPosition
  return
    ( Range
        (Position (sourceLine start - 1) (sourceColumn start - 1))
        (Position (sourceLine end - 1) (sourceColumn end - 1))
    , a
    )

constant :: Parsec Constant
constant =
  pure (BooleanConstant True) <* reserved "true" <|>
  pure (BooleanConstant False) <* reserved "false"

expression :: Parsec Expression
expression = fmap (uncurry Expression) . range $
  ConstantExpression <$> constant

lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ emptyDef
  { P.commentStart = "/*"
  , P.commentEnd = "*/"
  , P.commentLine = "//"
  , P.reservedNames = ["true", "false"]
  }

reserved :: String -> Parsec ()
reserved = P.reserved lexer
