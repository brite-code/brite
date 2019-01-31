-- Responsible for turning a Brite source text into a format which may be parsed. This includes
-- tokenizing the document and determining positions.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Token
  ( Position(..)
  , initialPosition
  , nextPosition
  , nextPositionLine
  , utf16Length
  , Range(..)
  , rangeBetween
  , rangeContains
  , Identifier
  , unsafeIdentifier
  , identifierText
  , isIdentifierStart
  , isIdentifierContinue
  , Keyword(..)
  , textKeyword
  , keywordText
  , Token(..)
  , TokenKind(..)
  , EndToken(..)
  , endTokenRange
  , Glyph(..)
  , glyphText
  , Trivia(..)
  , Newline(..)
  , Comment(..)
  , TriviaSide(..)
  , isTriviaWhitespace
  , tokenSource
  , endTokenSource
  , tokensTrimmedSource
  , removeTrailingSpaces
  , debugPosition
  , debugRange
  , debugTokens
  ) where

import Data.Bits ((.&.))
import Data.Char
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Text.ICU.Char (property, Bool_(XidStart, XidContinue))

-- A position between two characters in a Brite source code document. The encoding of a position is
-- based on the [Language Server Protocol][1].
--
-- * Positions are expressed with a zero-based line and character offset.
-- * A new line is started at one of the sequences `\n`, `\r\n`, and `\r`.
-- * Character offsets are determined by the number of code units in the characters UTF-16 encoding.
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
data Position = Position { positionLine :: !Int, positionCharacter :: !Int }
  deriving (Eq, Ord)

instance Show Position where
  show = Text.Lazy.unpack . B.toLazyText . debugPosition

-- The position at the start of a document.
initialPosition :: Position
initialPosition = Position 0 0

-- Adds the provided integer to the position character.
nextPosition :: Int -> Position -> Position
nextPosition n p = p { positionCharacter = positionCharacter p + n }

-- Adds the provided integer to the position line.
nextPositionLine :: Int -> Position -> Position
nextPositionLine n p = p { positionCharacter = 0, positionLine = positionLine p + n }

-- Get the number of UTF-16 code units in one Unicode code point.
--
-- [Description of UTF-16 encoding.](https://en.wikipedia.org/wiki/UTF-16)
utf16Length :: Char -> Int
utf16Length c = if n .&. 0xFFFF == n then 1 else 2
  where n = fromEnum c

-- A range in a Brite source code document represents the space between two characters. The end
-- position is exclusive. A `Range` is comparable to a selection in an editor while a `Position` is
-- comparable to a cursor in an editor.
--
-- Ranges are also based on the [Language Server Protocol][1].
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
data Range = Range { rangeStart :: Position, rangeEnd :: Position }

instance Show Range where
  show = Text.Lazy.unpack . B.toLazyText . debugRange

-- Gets the range between two other ranges. Takes the first position from the first range and the
-- second position from the second range.
rangeBetween :: Range -> Range -> Range
rangeBetween (Range p1 _) (Range _ p2) = Range p1 p2
{-# INLINE rangeBetween #-}

-- True if the first range completely contains the second.
rangeContains :: Range -> Range -> Bool
rangeContains (Range p1 p2) (Range p3 p4) = p1 <= p2 && p3 <= p4
{-# INLINE rangeContains #-}

-- A name written in a Brite program. Brite identifiers follow the [Unicode Identifier
-- Specification][1] including the optional underscore (`_`) character.
--
-- Some strings which are valid identifier syntax are reserved as keywords to disambiguate parsing.
-- We try to reserve the minimum number of keywords possible.
--
-- We could only have keywords in certain positions. For instance, only reserve the keyword `fun`
-- when in an expression context. However, this introduces a potentially confusing rule. It also
-- means, in this example, code transformations could not easily make expression identifiers out of
-- pattern identifiers.
--
-- [1]: http://www.unicode.org/reports/tr31
newtype Identifier = Identifier Text
  deriving (Eq, Ord, Hashable)

instance Show Identifier where
  show (Identifier t) = T.unpack t

-- Unsafely creates an identifier without checking if it is in the correct format.
unsafeIdentifier :: Text -> Identifier
unsafeIdentifier = Identifier

-- Get the text representation of an identifier.
identifierText :: Identifier -> Text
identifierText (Identifier t) = t

-- Is this character the start of an identifier?
isIdentifierStart :: Char -> Bool
isIdentifierStart c =
  isAsciiLower c || isAsciiUpper c || c == '_'
    || (c > '\x7f' && property XidStart c)

-- Does this character continue an identifier?
isIdentifierContinue :: Char -> Bool
isIdentifierContinue c =
  isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'
    || (c > '\x7f' && property XidContinue c)

-- Some word that is valid identifier syntax but we reserve for the purpose of parsing.
data Keyword
  = Hole -- `_`
  | True'
  | False'
  | Void
  | Let
  | If
  | Else
  | Do
  | Fun
  | Return
  | Loop
  | Break
  deriving (Eq, Show)

-- Tries to convert a text value into a keyword. Returns `Just` if the text value is a keyword.
-- Returns `Nothing` if the text value is not a keyword.
textKeyword :: Text -> Maybe Keyword
textKeyword t =
  case t of
    "_" -> Just Hole
    "true" -> Just True'
    "false" -> Just False'
    "void" -> Just Void
    "let" -> Just Let
    "if" -> Just If
    "else" -> Just Else
    "do" -> Just Do
    "fun" -> Just Fun
    "return" -> Just Return
    "loop" -> Just Loop
    "break" -> Just Break
    _ -> Nothing

-- Gets the raw text for a keyword.
keywordText :: Keyword -> Text
keywordText Hole = "_"
keywordText True' = "true"
keywordText False' = "false"
keywordText Void = "void"
keywordText Let = "let"
keywordText If = "if"
keywordText Else = "else"
keywordText Do = "do"
keywordText Fun = "fun"
keywordText Return = "return"
keywordText Loop = "loop"
keywordText Break = "break"

-- A token is a more semantic unit for describing Brite source code documents than a character.
-- Through the tokenization of a document we add meaning by parsing low-level code elements like
-- identifiers, numbers, strings, comments, and glyphs.
--
-- * The leading trivia of a token is all of the trivia which comes before the token which has not
--   already been parsed. Comments, spaces, and newlines.
--
-- * The trailing trivia of a token is all of the trivia after a token _up until the first new
--   line_. If a token is trailed by a line comment then that is part of the trailing trivia along
--   with the newline which immediately follows but nothing else!
data Token = Token
  { tokenRange :: Range
  , tokenKind :: TokenKind
  , tokenLeadingTrivia :: [Trivia]
  , tokenTrailingTrivia :: [Trivia]
  }
  deriving (Show)

-- The kind of a token.
data TokenKind
  = Glyph Glyph
  | IdentifierToken Identifier
  | UnexpectedChar Char
  deriving (Show)

-- The last token in a document. An end token has the position at which the document ended and all
-- the trivia between the last token and the ending.
data EndToken = EndToken
  { endTokenPosition :: Position
  , endTokenTrivia :: [Trivia]
  }

-- Gets the range covered by the end token. Starts and ends at the end token position.
endTokenRange :: EndToken -> Range
endTokenRange (EndToken { endTokenPosition = p }) = Range p p

-- A glyph represents some constant sequence of characters that is used in Brite syntax.
data Glyph
  = Keyword Keyword
  -- `&`
  | Ampersand
  -- `&&`
  | AmpersandDouble
  -- `->`
  | Arrow
  -- `*`
  | Asterisk
  -- `!`
  | Bang
  -- `|`
  | Bar
  -- `||`
  | BarDouble
  -- `{`
  | BraceLeft
  -- `}`
  | BraceRight
  -- `[`
  | BracketLeft
  -- `]`
  | BracketRight
  -- `^`
  | Caret
  -- `:`
  | Colon
  -- `,`
  | Comma
  -- `.`
  | Dot
  -- `=`
  | Equals_
  -- `==`
  | EqualsDouble
  -- `!=`
  | EqualsNot
  -- `>`
  | GreaterThan_
  -- `>=`
  | GreaterThanOrEqual_
  -- `<`
  | LessThan_
  -- `<=`
  | LessThanOrEqual_
  -- `-`
  | Minus
  -- `(`
  | ParenLeft
  -- `)`
  | ParenRight
  -- `%`
  | Percent
  -- `+`
  | Plus
  -- `;`
  | Semicolon
  -- `/`
  | Slash
  deriving (Eq, Show)

-- Gets the text representation of a glyph.
glyphText :: Glyph -> Text
glyphText (Keyword k) = keywordText k
glyphText Ampersand = "&"
glyphText AmpersandDouble = "&&"
glyphText Arrow = "->"
glyphText Asterisk = "*"
glyphText Bang = "!"
glyphText Bar = "|"
glyphText BarDouble = "||"
glyphText BraceLeft = "{"
glyphText BraceRight = "}"
glyphText BracketLeft = "["
glyphText BracketRight = "]"
glyphText Caret = "^"
glyphText Colon = ":"
glyphText Comma = ","
glyphText Dot = "."
glyphText Equals_ = "="
glyphText EqualsDouble = "=="
glyphText EqualsNot = "!="
glyphText GreaterThan_ = ">"
glyphText GreaterThanOrEqual_ = ">="
glyphText LessThan_ = "<"
glyphText LessThanOrEqual_ = "<="
glyphText Minus = "-"
glyphText ParenLeft = "("
glyphText ParenRight = ")"
glyphText Percent = "%"
glyphText Plus = "+"
glyphText Semicolon = ";"
glyphText Slash = "/"

-- Pieces of Brite syntax which (usually) don’t affect program behavior. Like comments or spaces.
data Trivia
  -- Contiguous space characters (` `).
  = Spaces Int
  -- Contiguous tab characters (`\t`). The Brite formatter prefers spaces to tabs, but since tabs
  -- are common enough we’ll add a special case for them in `Trivia`.
  | Tabs Int
  -- Contiguous newlines. The Brite formatter prefers line feeds (`\n`) to other forms
  -- of newlines.
  | Newlines Newline Int
  -- Some developer comment about the source code.
  | Comment Comment
  -- Other whitespace characters which we optimize such as obscure Unicode whitespace characters
  -- like U+00A0.
  | OtherWhitespace Char
  deriving (Show)

-- `\n`, `\r`, and `\r\n`
data Newline = LF | CR | CRLF deriving (Show)

data Comment
  -- `// ...` does not include the newline that ends the comment. Does include the `//` characters.
  = LineComment Text
  -- `/* ... */` does include the `/*` and `*/` characters.
  --
  -- If the boolean is true then we reached the end of the file before finding `*/`.
  --
  -- NOTE: Maybe we should remove support for block comments? They complicate printing and parsing
  -- and are not used for documentation. At least warn when we see a block comment?
  | BlockComment Text Bool
  deriving (Show)

-- The side on which trivia is attached to a token. Either the leading or trailing side.
data TriviaSide = Leading | Trailing

-- Is this trivia whitespace?
isTriviaWhitespace :: Trivia -> Bool
isTriviaWhitespace (Spaces _) = True
isTriviaWhitespace (Tabs _) = True
isTriviaWhitespace (Newlines _ _) = True
isTriviaWhitespace (Comment _) = False
isTriviaWhitespace (OtherWhitespace _) = True

-- Gets the source code that a token was parsed from.
tokenSource :: Token -> Text.Builder
tokenSource token =
  mconcat (map triviaSource (tokenLeadingTrivia token))
    <> B.fromText (tokenKindSource (tokenKind token))
    <> mconcat (map triviaSource (tokenTrailingTrivia token))

-- Gets the source code that a token kind was parsed from.
tokenKindSource :: TokenKind -> Text
tokenKindSource (Glyph g) = glyphText g
tokenKindSource (IdentifierToken i) = identifierText i
tokenKindSource (UnexpectedChar c) = T.singleton c

-- Gets the source code that an end token was parsed from.
endTokenSource :: EndToken -> Text.Builder
endTokenSource endToken = mconcat (map triviaSource (endTokenTrivia endToken))

-- Gets the source code that a trivia was parsed from.
triviaSource :: Trivia -> Text.Builder
triviaSource (Spaces n) = B.fromText (T.replicate n " ")
triviaSource (Tabs n) = B.fromText (T.replicate n "\t")
triviaSource (Newlines LF n) = B.fromText (T.replicate n "\n")
triviaSource (Newlines CR n) = B.fromText (T.replicate n "\r")
triviaSource (Newlines CRLF n) = B.fromText (T.replicate n "\r\n")
triviaSource (Comment (LineComment comment)) = B.fromText "//" <> B.fromText comment
triviaSource (Comment (BlockComment comment True)) = B.fromText "/*" <> B.fromText comment <> B.fromText "*/"
triviaSource (Comment (BlockComment comment False)) = B.fromText "/*" <> B.fromText comment
triviaSource (OtherWhitespace c) = B.singleton c

-- Builds source code from from the provided tokens removing whitespace at the beginning of the
-- code, ending of the code, and immediately before a new line (trailing whitespace).
tokensTrimmedSource :: [Token] -> Text.Builder
tokensTrimmedSource = sourceStart
  where
    trimStart t =
      let trivia = dropWhile isTriviaWhitespace (tokenLeadingTrivia t) in
        t { tokenLeadingTrivia = trivia }

    trimEnd t =
      let trivia = reverse (dropWhile isTriviaWhitespace (reverse (tokenTrailingTrivia t))) in
        t { tokenTrailingTrivia = trivia }

    sourceStart [] = source []
    sourceStart (t : ts) = source (trimStart t : ts)

    source [] = mempty
    source [t] = tokenTrimmedSource (trimEnd t)
    source (t : ts) = tokenTrimmedSource t <> source ts

-- Gets the source code that a token was parsed from. Trimming all whitespace that comes
-- immediately before a new line.
tokenTrimmedSource :: Token -> Text.Builder
tokenTrimmedSource token =
  triviaTrimmedSource (tokenLeadingTrivia token)
    <> B.fromText (tokenKindTrimmedSource (tokenKind token))
    <> triviaTrimmedSource (tokenTrailingTrivia token)

-- Gets the source code that a token kind was parsed from. Trimming all whitespace that comes
-- immediately before a new line.
--
-- NOTE: This function is basically the same as `tokenKindSource`. However, in the future when we
-- add string literals then we will need to trim trailing whitespace inside string literals.
tokenKindTrimmedSource :: TokenKind -> Text
tokenKindTrimmedSource (t@(Glyph _)) = tokenKindSource t
tokenKindTrimmedSource (t@(IdentifierToken _)) = tokenKindSource t
tokenKindTrimmedSource (t@(UnexpectedChar _)) = tokenKindSource t

-- Gets the source code that a list of trivia was parsed from. Trimming all whitespace that comes
-- immediately before a new line.
triviaTrimmedSource :: [Trivia] -> Text.Builder
triviaTrimmedSource = loop mempty mempty
  where
    loop source space [] = source <> space

    -- Add whitespace to the second accumulator argument.
    loop source space (t@(Spaces _) : ts) = loop source (space <> triviaSource t) ts
    loop source space (t@(Tabs _) : ts) = loop source (space <> triviaSource t) ts
    loop source space (t@(OtherWhitespace _) : ts) = loop source (space <> triviaSource t) ts

    -- Don’t add the space trivia immediately preceding a new line.
    loop source _ (t@(Newlines _ _) : ts) = loop (source <> triviaSource t) mempty ts

    -- Trim trailing whitespace from line comments.
    loop source space (Comment (LineComment comment) : ts) =
      let
        newSource =
          source <> space <> B.fromText "//"
            <> B.fromText (T.dropWhileEnd isSpace comment)
      in
        loop newSource mempty ts

    -- Trim trailing whitespace from block comments.
    --
    -- We also automatically fix missing block comment endings. Even though this isn’t explicitly
    -- a part of what `triviaTrimmedSource` is described as doing.
    loop source space (Comment (BlockComment comment _) : ts) =
      let
        newSource =
          source <> space <> B.fromText "/*" <> removeTrailingSpaces comment <> B.fromText "*/"
      in
        loop newSource mempty ts

-- Removes trailing spaces which come before a new line. Trailing spaces at the end of a string are
-- left in place.
removeTrailingSpaces :: Text -> Text.Builder
removeTrailingSpaces t =
  let (t1, t2) = T.span (\c -> c /= '\n' && c /= '\r') t in
    if T.null t2 then
      B.fromText t
    else
      B.fromText (T.dropWhileEnd isSpace t1)
        <> B.singleton (T.head t2)
        <> removeTrailingSpaces (T.tail t2)

-- Debug a position.
debugPosition :: Position -> Text.Builder
debugPosition (Position line character) =
  B.decimal line <> B.singleton ':' <> B.decimal character

-- Debug a range of characters.
debugRange :: Range -> Text.Builder
debugRange (Range start end) =
  debugPosition start <> B.singleton '-' <> debugPosition end

-- Debug a stream of tokens.
debugTokens :: [Token] -> EndToken -> Text.Builder
debugTokens tokens endToken = mconcat (map debugToken tokens) <> debugEndToken endToken

debugToken :: Token -> Text.Builder
debugToken (Token r k lt tt) =
  mconcat (map (debugTrivia Leading) lt)
    <> B.fromLazyText (Text.Lazy.justifyLeft 10 ' ' (B.toLazyText (debugRange r)))
    <> B.fromText "| "
    <> B.fromText content
    <> B.singleton '\n'
    <> mconcat (map (debugTrivia Trailing) tt)
  where
    content = case k of
      Glyph glyph -> T.snoc (T.append "Glyph `" (glyphText glyph)) '`'
      IdentifierToken (Identifier identifier) -> T.snoc (T.append "Identifier `" identifier) '`'
      UnexpectedChar c -> T.snoc (T.snoc "Unexpected `" c) '`'

debugEndToken :: EndToken -> Text.Builder
debugEndToken (EndToken p lt) =
  mconcat (map (debugTrivia Leading) lt)
    <> B.fromLazyText (Text.Lazy.justifyLeft 10 ' ' (B.toLazyText (debugPosition p)))
    <> B.fromText "| End\n"

debugTrivia :: TriviaSide -> Trivia -> Text.Builder
debugTrivia side trivia =
  (case side of { Leading -> "+"; Trailing -> "-" })
    <> B.fromText (T.replicate 9 " ")
    <> B.fromText "| "
    <> content
    <> B.singleton '\n'
  where
    content =
      case trivia of
        Spaces n -> B.fromText "Spaces " <> B.decimal n
        Tabs n -> B.fromText "Tabs " <> B.decimal n
        Newlines LF n -> B.fromText "Newlines LF " <> B.decimal n
        Newlines CR n -> B.fromText "Newlines CR " <> B.decimal n
        Newlines CRLF n -> B.fromText "Newlines CRLF " <> B.decimal n
        Comment (LineComment _) -> B.fromText "LineComment"
        Comment (BlockComment _ True) -> B.fromText "BlockComment"
        Comment (BlockComment _ False) -> B.fromText "BlockComment (unterminated)"
        OtherWhitespace c -> B.fromText "OtherWhitespace `" <> B.singleton c <> B.singleton '`'
