-- Responsible for turning a Brite source text into a format which may be parsed. This includes
-- tokenizing the document and determining positions.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.Tokens
  ( Position(..)
  , initialPosition
  , utf16Length
  , Range(..)
  , rangeBetween
  , Identifier
  , identifierText
  , Keyword(..)
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
  , isTriviaWhitespace
  , isUnterminatedBlockComment
  , TokenStream
  , tokenStreamPosition
  , tokenStreamText
  , TokenStreamStep
  , tokenStreamStepPosition
  , tokenize
  , tokenStreamToList
  , nextToken
  , tokenSource
  , tokenKindSource
  , endTokenSource
  , triviaSource
  , debugPosition
  , debugRange
  , debugTokens
  ) where

import Data.Bits ((.&.))
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Custom as T
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
  deriving (Eq)

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

-- Gets the range between two other ranges. Takes the first position from the first range and the
-- second position from the second range.
rangeBetween :: Range -> Range -> Range
rangeBetween (Range p1 _) (Range _ p2) = Range p1 p2

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
  | True_
  | False_
  | Let
  | If
  | Else
  | Do
  | Fun
  | Return
  | Loop
  | Break
  deriving (Eq)

-- Tries to convert a text value into a keyword. Returns `Just` if the text value is a keyword.
-- Returns `Nothing` if the text value is not a keyword.
keyword :: Text -> Maybe Keyword
keyword t =
  case t of
    "_" -> Just Hole
    "true" -> Just True_
    "false" -> Just False_
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
keywordText True_ = "true"
keywordText False_ = "false"
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

-- The kind of a token.
data TokenKind
  = Glyph Glyph
  | IdentifierToken Identifier
  | UnexpectedChar Char

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
  deriving (Eq)

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

-- `\n`, `\r`, and `\r\n`
data Newline = LF | CR | CRLF

data Comment
  -- `// ...` does not include the newline that ends the comment. Does include the `//` characters.
  = LineComment Text
  -- `/* ... */` does include the `/*` and `*/` characters.
  --
  -- If the boolean is true then we reached the end of the file before finding `*/`.
  --
  -- NOTE: Maybe we should remove support for block comments? They complicate printing and parsing
  -- and are not used for documentation. At least warn when we see a block comment?
  --
  -- TODO: Make a decision on keeping or removing block comments.
  | BlockComment Text Bool

-- Is this trivia whitespace?
isTriviaWhitespace :: Trivia -> Bool
isTriviaWhitespace (Spaces _) = True
isTriviaWhitespace (Tabs _) = True
isTriviaWhitespace (Newlines _ _) = True
isTriviaWhitespace (Comment _) = False
isTriviaWhitespace (OtherWhitespace _) = True

-- Is this trivia an unterminated block comment?
isUnterminatedBlockComment :: Trivia -> Bool
isUnterminatedBlockComment (Comment (BlockComment _ False)) = True
isUnterminatedBlockComment _ = False

-- A stream of tokens. Call `nextToken` to advance the stream.
data TokenStream = TokenStream
  { tokenStreamPosition :: Position
  , tokenStreamText :: Text
  }

-- One step in the `TokenStream`. Created by calling `nextToken`.
type TokenStreamStep = Either EndToken (Token, TokenStream)

-- Gets the starting position of a token stream step. Like `tokenStreamPosition` but operates on
-- a `TokenStreamStep`.
tokenStreamStepPosition :: TokenStreamStep -> Position
tokenStreamStepPosition (Right (t, _)) = rangeStart (tokenRange t)
tokenStreamStepPosition (Left t) = endTokenPosition t

-- Creates a token stream from a text document.
tokenize :: Text -> TokenStream
tokenize text = TokenStream initialPosition text

-- Converts a token stream to a list of tokens and an end token.
tokenStreamToList :: TokenStream -> ([Token], EndToken)
tokenStreamToList = loop []
  where
    loop acc ts =
      case nextToken ts of
        Right (t, ts') -> loop (t : acc) ts'
        Left t -> (reverse acc, t)

-- Advances the token stream. Either returns a token and the remainder of the token stream or
-- returns the ending token in the stream.
nextToken :: TokenStream -> TokenStreamStep
nextToken (TokenStream p0 t0) =
  case T.uncons t1 of
    -- End token
    Nothing -> Left (EndToken p1 leadingTrivia)

    -- Single character glyphs
    Just ('*', t2) -> token (Glyph Asterisk) 1 t2
    Just ('{', t2) -> token (Glyph BraceLeft) 1 t2
    Just ('}', t2) -> token (Glyph BraceRight) 1 t2
    Just ('[', t2) -> token (Glyph BracketLeft) 1 t2
    Just (']', t2) -> token (Glyph BracketRight) 1 t2
    Just ('^', t2) -> token (Glyph Caret) 1 t2
    Just (':', t2) -> token (Glyph Colon) 1 t2
    Just (',', t2) -> token (Glyph Comma) 1 t2
    Just ('.', t2) -> token (Glyph Dot) 1 t2
    Just ('(', t2) -> token (Glyph ParenLeft) 1 t2
    Just (')', t2) -> token (Glyph ParenRight) 1 t2
    Just ('%', t2) -> token (Glyph Percent) 1 t2
    Just ('+', t2) -> token (Glyph Plus) 1 t2
    Just (';', t2) -> token (Glyph Semicolon) 1 t2
    Just ('/', t2) -> token (Glyph Slash) 1 t2

    -- Multi-character glyphs.
    Just ('&', t2) | not (T.null t2) && T.head t2 == '&' -> token (Glyph AmpersandDouble) 2 (T.tail t2)
    Just ('&', t2) -> token (Glyph Ampersand) 1 t2
    Just ('|', t2) | not (T.null t2) && T.head t2 == '|' -> token (Glyph BarDouble) 2 (T.tail t2)
    Just ('|', t2) -> token (Glyph Bar) 1 t2
    Just ('=', t2) | not (T.null t2) && T.head t2 == '=' -> token (Glyph EqualsDouble) 2 (T.tail t2)
    Just ('=', t2) -> token (Glyph Equals_) 1 t2
    Just ('!', t2) | not (T.null t2) && T.head t2 == '=' -> token (Glyph EqualsNot) 2 (T.tail t2)
    Just ('!', t2) -> token (Glyph Bang) 1 t2
    Just ('>', t2) | not (T.null t2) && T.head t2 == '=' -> token (Glyph GreaterThanOrEqual_) 2 (T.tail t2)
    Just ('>', t2) -> token (Glyph GreaterThan_) 1 t2
    Just ('<', t2) | not (T.null t2) && T.head t2 == '=' -> token (Glyph LessThanOrEqual_) 2 (T.tail t2)
    Just ('<', t2) -> token (Glyph LessThan_) 1 t2
    Just ('-', t2) | not (T.null t2) && T.head t2 == '>' -> token (Glyph Arrow) 2 (T.tail t2)
    Just ('-', t2) -> token (Glyph Minus) 1 t2

    -- Identifier
    Just (c, _) | isIdentifierStart c ->
      let
        (ident, n, t2) =
          T.spanWithState
            (\n' c' -> if isIdentifierContinue c' then Just (n' + utf16Length c') else Nothing) 0
            t1
      in
        case keyword ident of
          Just k -> token (Glyph (Keyword k)) n t2
          Nothing -> token (IdentifierToken (Identifier ident)) n t2

    -- Unexpected character
    Just (c, t2) -> token (UnexpectedChar c) (utf16Length c) t2
  where
    -- Leading trivia
    (leadingTrivia, p1, t1) = nextTrivia Leading [] p0 t0

    -- Creates a token with trailing trivia
    token k n t2 =
      let
        p2 = nextPosition n p1
        (trailingTrivia, p3, t3) = nextTrivia Trailing [] p2 t2
        tk = Token (Range p1 p2) k leadingTrivia trailingTrivia
      in
        Right (tk, TokenStream p3 t3)

-- The side on which trivia is attached to a token. Either the leading or trailing side.
data TriviaSide = Leading | Trailing

-- Parses some trivia!
nextTrivia :: TriviaSide -> [Trivia] -> Position -> Text -> ([Trivia], Position, Text)
nextTrivia side acc p0 t0 =
  case T.uncons t0 of
    -- Spaces
    Just (' ', t1) ->
      let p1 = nextPosition 1 p0 in
      case acc of
        Spaces n : acc' -> nextTrivia side (Spaces (n + 1) : acc') p1 t1
        acc' -> nextTrivia side (Spaces 1 : acc') p1 t1

    -- Tabs
    Just ('\t', t1) ->
      let p1 = nextPosition 1 p0 in
      case acc of
        Tabs n : acc' -> nextTrivia side (Tabs (n + 1) : acc') p1 t1
        acc' -> nextTrivia side (Tabs 1 : acc') p1 t1

    -- Newlines (LF)
    Just ('\n', t1) ->
      let
        p1 = nextPositionLine 1 p0
        acc1 =
          case acc of
            Newlines LF n : acc' -> Newlines LF (n + 1) : acc'
            acc' -> Newlines LF 1 : acc'
      in
        case side of
          Leading -> nextTrivia Leading acc1 p1 t1
          Trailing -> (reverse acc1, p1, t1)

    -- Newlines (CRLF)
    Just ('\r', t1) | not (T.null t1) && T.head t1 == '\n' ->
      let
        p2 = nextPositionLine 1 p0
        t2 = T.tail t1
        acc2 =
          case acc of
            Newlines CRLF n : acc' -> Newlines CRLF (n + 1) : acc'
            acc' -> Newlines CRLF 1 : acc'
      in
        case side of
          Leading -> nextTrivia Leading acc2 p2 t2
          Trailing -> (reverse acc2, p2, t2)

    -- Newlines (CR)
    Just ('\r', t1) ->
      let
        p1 = nextPositionLine 1 p0
        acc1 =
          case acc of
            Newlines CR n : acc' -> Newlines CR (n + 1) : acc'
            acc' -> Newlines CR 1 : acc'
      in
        case side of
          Leading -> nextTrivia Leading acc1 p1 t1
          Trailing -> (reverse acc1, p1, t1)

    -- Line comments
    Just ('/', t1) | not (T.null t1) && T.head t1 == '/' ->
      let
        -- Collect the comment and number of characters captured.
        (comment, n, t2) =
          T.spanWithState
            (\n' c -> if c == '\n' || c == '\r' then Nothing else Just (n' + utf16Length c)) 2
            (T.tail t1)

        p2 = nextPosition n p0
      in
        nextTrivia side (Comment (LineComment comment) : acc) p2 t2

    -- Block comments
    Just ('/', t1) | not (T.null t1) && T.head t1 == '*' ->
      let
        -- Collect the comment and the position after the comment.
        (comment, (finalState, p2), t2) =
          T.spanWithState
            (\(state, p1) c ->
              case c of
                -- If we found the block comment end sequence then return `Nothing`.
                '*' -> Just (1, nextPosition 1 p1)
                '/' | state == 1 -> Just (2, nextPosition 1 p1)
                _ | state == 2 -> Nothing

                -- Count newlines. LF, CR, and CRLF. We use state to count CRLF.
                '\n' | state == 3 -> Just (0, p1)
                '\n' -> Just (0, nextPositionLine 1 p1)
                '\r' -> Just (3, nextPositionLine 1 p1)

                -- Add the character’s UTF-16 length to the position and continue.
                _ -> Just (0, nextPosition (utf16Length c) p1))
            ((0 :: Int), nextPosition 2 p0)
            (T.tail t1)

        -- Drop `*/` from the comment text.
        blockComment =
          case finalState of
            2 -> BlockComment (T.dropEnd 2 comment) True
            _ -> BlockComment comment False

        -- Create the new `acc` value.
        acc2 = Comment blockComment : acc
      in
        -- If we are on the trailing side and the block comment spans multiple lines then stop
        -- parsing trivia.
        case side of
          Leading -> nextTrivia Leading acc2 p2 t2
          Trailing | positionLine p0 /= positionLine p2 -> (reverse acc2, p2, t2)
          Trailing -> nextTrivia Trailing acc2 p2 t2

    -- Other whitespace
    Just (c, t1) | isSpace c ->
      nextTrivia side (OtherWhitespace c : acc) (nextPosition (utf16Length c) p0) t1

    -- Return trivia if there isn’t more.
    _ -> (reverse acc, p0, t0)

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
