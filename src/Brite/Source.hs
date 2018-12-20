-- Responsible for turning a Brite source text into a format which may be parsed. This includes
-- tokenizing the document and determining positions.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Source
  ( Position(..)
  , initialPosition
  , Range(..)
  , Identifier
  , identifierText
  , Keyword(..)
  , keywordText
  , Token(..)
  , TokenKind(..)
  , EndToken(..)
  , Glyph(..)
  , glyphText
  , Trivia(..)
  , Newline(..)
  , Comment(..)
  , TokenList(..)
  , tokenize
  , TokenStream
  , tokenize'
  , nextToken
  , rebuildSource
  , debugPosition
  , debugRange
  , debugTokens
  ) where

import Data.Bits ((.&.))
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Custom as T
import qualified Data.Text.Lazy as L
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
newtype Identifier = Identifier T.Text

-- Get the text representation of an identifier.
identifierText :: Identifier -> T.Text
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
  deriving (Eq)

-- Tries to convert a text value into a keyword. Returns `Just` if the text value is a keyword.
-- Returns `Nothing` if the text value is not a keyword.
keyword :: T.Text -> Maybe Keyword
keyword t =
  case t of
    "_" -> Just Hole
    "true" -> Just True_
    "false" -> Just False_
    "let" -> Just Let
    "if" -> Just If
    "else" -> Just Else
    "do" -> Just Do
    _ -> Nothing

-- Gets the raw text for a keyword.
keywordText :: Keyword -> T.Text
keywordText Hole = "_"
keywordText True_ = "true"
keywordText False_ = "false"
keywordText Let = "let"
keywordText If = "if"
keywordText Else = "else"
keywordText Do = "do"

-- A token is a more semantic unit for describing Brite source code documents than a character.
-- Through the tokenization of a document we add meaning by parsing low-level code elements like
-- identifiers, numbers, strings, comments, and glyphs.
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
data EndToken = EndToken'
  { endTokenPosition :: Position
  , endTokenTrivia :: [Trivia]
  }

-- A glyph represents some constant sequence of characters that is used in Brite syntax.
data Glyph
  = Keyword Keyword
  -- `{`
  | BraceLeft
  -- `}`
  | BraceRight
  -- `,`
  | Comma
  -- `.`
  | Dot
  -- `=`
  | Equals
  -- `(`
  | ParenLeft
  -- `)`
  | ParenRight
  -- `;`
  | Semicolon
  -- `/`
  | Slash
  deriving (Eq)

-- Gets the text representation of a glyph.
glyphText :: Glyph -> T.Text
glyphText (Keyword k) = keywordText k
glyphText BraceLeft = "{"
glyphText BraceRight = "}"
glyphText Comma = ","
glyphText Dot = "."
glyphText Equals = "="
glyphText ParenLeft = "("
glyphText ParenRight = ")"
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
  = LineComment T.Text
  -- `/* ... */` does include the `/*` and `*/` characters.
  --
  -- NOTE: `/*/` is an acceptable block comment. Also, we don’t report a syntax error for
  -- unterminated block comments.
  | BlockComment T.Text

-- A lazy list of tokens. Customized to include some extra token and end data.
data TokenList
  -- The next token in the list along with the token’s range.
  = NextToken Token TokenList
  -- The end of the token list. Includes the position of the source text’s end.
  | EndToken Position

-- Turn Unicode text into a lazy list of tokens and their associated ranges.
tokenize :: Position -> T.Text -> TokenList
tokenize position0 text0 =
  if T.null text0 then EndToken position0 else
  case T.head text0 of
    -- Ignore whitespace.
    ' ' -> tokenize position1 text1

    -- Parse some glyphs.
    '{' -> NextToken (Token range1 (Glyph BraceLeft) [] []) (tokenize position1 text1)
    '}' -> NextToken (Token range1 (Glyph BraceRight) [] []) (tokenize position1 text1)
    ',' -> NextToken (Token range1 (Glyph Comma) [] []) (tokenize position1 text1)
    '.' -> NextToken (Token range1 (Glyph Dot) [] []) (tokenize position1 text1)
    '=' -> NextToken (Token range1 (Glyph Equals) [] []) (tokenize position1 text1)
    '(' -> NextToken (Token range1 (Glyph ParenLeft) [] []) (tokenize position1 text1)
    ')' -> NextToken (Token range1 (Glyph ParenRight) [] []) (tokenize position1 text1)
    ';' -> NextToken (Token range1 (Glyph Semicolon) [] []) (tokenize position1 text1)

    -- Ignore newlines (`\n`).
    '\n' ->
      let
        position2 = position0 { positionCharacter = 0, positionLine = positionLine position0 + 1 }
      in
        tokenize position2 text1

    -- Ignore newlines (`\r` and `\r\n`). If we have the sequence `\r\n` we only want to count one
    -- newline and not two.
    '\r' ->
      let
        position2 = position0 { positionCharacter = 0, positionLine = positionLine position0 + 1 }
        text2 = if T.null text1 || T.head text1 /= '\n' then text1 else T.tail text1
      in
        tokenize position2 text2

    -- Parse an identifier.
    --
    -- The identifier we create is represented as a `T.Text`. This text value is linked to the
    -- source document it was created from. This means we don’t need to allocate new memory for
    -- every identifier but also means our source document may live longer than expected.
    c | isIdentifierStart c ->
      let
        (identifier, text2) = T.span isIdentifierContinue text0
        n = T.foldl (\acc c' -> acc + utf16Length c') 0 identifier
        position2 = position0 { positionCharacter = positionCharacter position0 + n }
        range2 = Range position0 position2
        kind = case keyword identifier of
          Just k -> Glyph (Keyword k)
          Nothing -> IdentifierToken (Identifier identifier)
      in
        NextToken (Token range2 kind [] []) (tokenize position2 text2)

    -- Parse a single line comment. Single line comments ignore all characters until the
    -- next newline.
    '/' | not (T.null text1) && T.head text1 == '/' ->
      let
        (n, text2) = loop 2 (T.tail text1)
        position2 = position0 { positionCharacter = positionCharacter position0 + n }
      in
        tokenize position2 text2
      where
        loop n t =
          case T.uncons t of
            Nothing -> (n, t)
            Just ('\n', _) -> (n, t)
            Just ('\r', _) -> (n, t)
            Just (c, t') -> loop (n + utf16Length c) t'

    -- Parse a multi-line comment. Multi-line comments ignore all characters until the character
    -- sequence `*/`.
    '/' | not (T.null text1) && T.head text1 == '*' ->
      let
        position2 = position0 { positionCharacter = positionCharacter position0 + 2 }
      in
        uncurry tokenize (loop position2 (T.tail text1))
      where
        loop p t =
          case T.uncons t of
            Nothing -> (p, t)
            Just ('*', t') | not (T.null t') && T.head t' == '/' ->
              let p' = p { positionCharacter = positionCharacter p + 2 } in
              (p', T.tail t')
            Just ('\n', t') ->
              let p' = p { positionCharacter = 0, positionLine = positionLine p + 1 } in
              loop p' t'
            Just ('\r', t') ->
              let p' = p { positionCharacter = 0, positionLine = positionLine p + 1 } in
              loop p' (if T.null t' || T.head t' /= '\n' then t' else T.tail t')
            Just (c, t') ->
              let p' = p { positionCharacter = positionCharacter p + utf16Length c } in
              loop p' t'

    -- Parse the slash glyph.
    '/' -> NextToken (Token range1 (Glyph Slash) [] []) (tokenize position1 text1)

    -- Ignore whitespace.
    c | isSpace c -> tokenize position1 text1

    -- Unexpected character.
    c ->
      let
        position2 = position0 { positionCharacter = positionCharacter position0 + utf16Length c }
        range2 = Range position0 position2
      in
        NextToken (Token range2 (UnexpectedChar c) [] []) (tokenize position2 text1)

    where
      position1 = position0 { positionCharacter = positionCharacter position0 + 1 }
      range1 = Range position0 position1
      text1 = T.tail text0

-- A stream of tokens. Call `nextToken` to advance the stream.
data TokenStream = TokenStream Position T.Text

-- Creates a token stream from a text document.
tokenize' :: T.Text -> TokenStream
tokenize' text = TokenStream initialPosition text

-- Advances the token stream. Either returns a token and the remainder of the token stream or
-- returns the ending token in the stream.
nextToken :: TokenStream -> Either EndToken (Token, TokenStream)
nextToken (TokenStream p0 t0) =
  case T.uncons t1 of
    -- End token
    Nothing -> Left (EndToken' p1 leadingTrivia)

    -- Single character glyphs
    Just ('{', t2) -> token (Glyph BraceLeft) 1 t2
    Just ('}', t2) -> token (Glyph BraceRight) 1 t2
    Just (',', t2) -> token (Glyph Comma) 1 t2
    Just ('.', t2) -> token (Glyph Dot) 1 t2
    Just ('=', t2) -> token (Glyph Equals) 1 t2
    Just ('(', t2) -> token (Glyph ParenLeft) 1 t2
    Just (')', t2) -> token (Glyph ParenRight) 1 t2
    Just (';', t2) -> token (Glyph Semicolon) 1 t2
    Just ('/', t2) -> token (Glyph Slash) 1 t2

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
nextTrivia :: TriviaSide -> [Trivia] -> Position -> T.Text -> ([Trivia], Position, T.Text)
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
            (\n' c -> if c == '\n' || c == '\r' then Nothing else Just (n' + utf16Length c)) 0
            t0

        p2 = nextPosition n p0
      in
        nextTrivia side (Comment (LineComment comment) : acc) p2 t2

    -- Block comments
    Just ('/', t1) | not (T.null t1) && T.head t1 == '*' ->
      let
        -- Collect the comment and the position after the comment.
        (comment, (_, p2), t2) =
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
            ((0 :: Int), p0)
            t0

        -- Create the new `acc` value.
        acc2 = Comment (BlockComment comment) : acc
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

-- Takes a token stream, iterates through all the tokens, and rebuilds the source document. This
-- function exercises the invariant that we must always be able to rebuild the source document from
-- our token stream.
rebuildSource :: TokenStream -> B.Builder
rebuildSource tokens = loop mempty tokens
  where
    loop acc ts =
      case nextToken ts of
        Right (token, ts') -> loop (acc <> printToken token) ts'
        Left endToken -> acc <> mconcat (map printTrivia (endTokenTrivia endToken))

-- Prints a token into the source code it was parsed from.
printToken :: Token -> B.Builder
printToken token =
  let
    content = case tokenKind token of
      Glyph g -> B.fromText (glyphText g)
      IdentifierToken (Identifier ident) -> B.fromText ident
      UnexpectedChar c -> B.singleton c
  in
    mconcat (map printTrivia (tokenLeadingTrivia token))
      <> content
      <> mconcat (map printTrivia (tokenTrailingTrivia token))

-- Prints some trivia into the source code it was parsed from.
printTrivia :: Trivia -> B.Builder
printTrivia (Spaces n) = B.fromText (T.replicate n " ")
printTrivia (Tabs n) = B.fromText (T.replicate n "\t")
printTrivia (Newlines LF n) = B.fromText (T.replicate n "\n")
printTrivia (Newlines CR n) = B.fromText (T.replicate n "\r")
printTrivia (Newlines CRLF n) = B.fromText (T.replicate n "\r\n")
printTrivia (Comment (LineComment comment)) = B.fromText comment
printTrivia (Comment (BlockComment comment)) = B.fromText comment
printTrivia (OtherWhitespace c) = B.singleton c

-- Debug a position.
debugPosition :: Position -> B.Builder
debugPosition (Position line character) =
  B.decimal line <> B.singleton ':' <> B.decimal character

-- Debug a range of characters.
debugRange :: Range -> B.Builder
debugRange (Range start end) =
  debugPosition start <> B.singleton '-' <> debugPosition end

-- Debug a stream of tokens.
debugTokens :: TokenStream -> B.Builder
debugTokens ts = debugTokens' (nextToken ts)

debugTokens' :: Either EndToken (Token, TokenStream) -> B.Builder
debugTokens' (Right (Token r k leadingTrivia trailingTrivia, ts)) =
  mconcat (map (debugTrivia Leading) leadingTrivia)
    <> B.fromLazyText (L.justifyLeft 10 ' ' (B.toLazyText (debugRange r)))
    <> B.fromText "| "
    <> B.fromText content
    <> B.singleton '\n'
    <> mconcat (map (debugTrivia Trailing) trailingTrivia)
    <> debugTokens' (nextToken ts)
  where
    content = case k of
      Glyph glyph -> T.snoc (T.append "Glyph `" (glyphText glyph)) '`'
      IdentifierToken (Identifier identifier) -> T.snoc (T.append "Identifier `" identifier) '`'
      UnexpectedChar c -> T.snoc (T.snoc "Unexpected `" c) '`'

debugTokens' (Left (EndToken' p leadingTrivia)) =
  mconcat (map (debugTrivia Leading) leadingTrivia)
    <> B.fromLazyText (L.justifyLeft 10 ' ' (B.toLazyText (debugPosition p)))
    <> B.fromText "| End\n"

debugTrivia :: TriviaSide -> Trivia -> B.Builder
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
        Comment (BlockComment _) -> B.fromText "BlockComment"
        OtherWhitespace c -> B.fromText "OtherWhitespace `" <> B.singleton c <> B.singleton '`'
