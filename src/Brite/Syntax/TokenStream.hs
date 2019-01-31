-- Responsible for turning a Brite source text into a format which may be parsed. This includes
-- tokenizing the document and determining positions.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Syntax.TokenStream
  ( TokenStream
  , tokenStreamPosition
  , tokenStreamText
  , TokenStreamStep
  , tokenStreamStepPosition
  , tokenize
  , tokenStreamToList
  , nextToken
  ) where

import Brite.Diagnostic
import Brite.Syntax.Range
import Brite.Syntax.Token
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Custom as T

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
tokenStreamToList :: TokenStream -> DiagnosticWriter ([Token], EndToken)
tokenStreamToList = loop []
  where
    loop acc ts = do
      step <- nextToken ts
      case step of
        Right (t, ts') -> loop (t : acc) ts'
        Left t -> return (reverse acc, t)

-- Advances the token stream. Either returns a token and the remainder of the token stream or
-- returns the ending token in the stream.
nextToken :: TokenStream -> DiagnosticWriter TokenStreamStep
nextToken stream = do
  -- Leading trivia
  (leadingTrivia, p1, t1) <-
    nextTrivia Leading [] (tokenStreamPosition stream) (tokenStreamText stream)

  let
    -- Creates a token with trailing trivia
    token k n t2 = do
      let p2 = nextPosition n p1
      (trailingTrivia, p3, t3) <- nextTrivia Trailing [] p2 t2
      let tk = Token (Range p1 p2) k leadingTrivia trailingTrivia
      return (Right (tk, TokenStream p3 t3))

  case T.uncons t1 of
    -- End token
    Nothing -> return (Left (EndToken p1 leadingTrivia))

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
    --
    -- TODO: Must not be followed by a number.
    Just (c, _) | isIdentifierStart c ->
      let
        (ident, n, t2) =
          T.spanWithState
            (\n' c' -> if isIdentifierContinue c' then Just (n' + utf16Length c') else Nothing) 0
            t1
      in
        case textKeyword ident of
          Just k -> token (Glyph (Keyword k)) n t2
          Nothing -> token (IdentifierToken (unsafeIdentifier ident)) n t2

    -- Number
    --
    -- TODO: Must not be followed by an identifier.
    -- TODO: Binary and hexadecimal must be followed by at least one digit.
    Just (c0, t2) | isDigit c0 ->
      case (c0, T.uncons t2) of
        -- Parse a binary integer
        ('0', Just (c1, t3)) | c1 == 'b' || c1 == 'B' ->
          let
            -- Parse a binary number. Build up the value and count the number of digits.
            (raw, (finalValue, finalDigits), t4) =
              T.spanWithState
                (\(value, digits) c2 ->
                  case c2 of
                    '0' -> Just (value * 2 + 0, digits + 1)
                    '1' -> Just (value * 2 + 1, digits + 1)
                    _ -> Nothing)
                (0, 0)
                t3
          in
            -- If we didn’t parse any digits then report an error.
            if finalDigits == 0 then do
              let actualRaw = T.singleton '0' `T.snoc` c1
              let p2 = nextPosition 2 p1
              diagnostic <- case T.uncons t4 of
                Nothing -> unexpectedEnding p2 ExpectedBinaryDigit
                Just (c2, _) -> unexpectedCharacter p2 c2 ExpectedBinaryDigit
              error "TODO: unimplemented"
              -- token (Number (ErrorNumber diagnostic actualRaw)) 2 t4
            else
              error "TODO: unimplemented"

        ('0', Just (c1, t3)) | c1 == 'x' || c1 == 'X' ->
          error "TODO: unimplemented"

    -- Unexpected character
    Just (c, t2) -> token (UnexpectedChar c) (utf16Length c) t2

-- Parses some trivia!
nextTrivia :: TriviaSide -> [Trivia] -> Position -> Text -> DiagnosticWriter ([Trivia], Position, Text)
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
          Trailing -> return (reverse acc1, p1, t1)

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
          Trailing -> return (reverse acc2, p2, t2)

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
          Trailing -> return (reverse acc1, p1, t1)

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
    Just ('/', t1) | not (T.null t1) && T.head t1 == '*' -> do
      let
        -- Collect the comment and the position after the comment.
        (comment, ((_, finalState), p2), t2) =
          T.spanWithState
            (\((depth, state), p1) c ->
              case c of
                -- If we found the block comment end sequence then return `Nothing`.
                _ | state == FoundAsteriskSlash && depth == 0 -> Nothing
                '*' | state == FoundSlash -> Just ((depth + 1, Normal), nextPosition 1 p1)
                '*' -> Just ((depth, FoundAsterisk), nextPosition 1 p1)
                '/' | state == FoundAsterisk -> Just ((depth - 1, FoundAsteriskSlash), nextPosition 1 p1)
                '/' -> Just ((depth, FoundSlash), nextPosition 1 p1)

                -- Count newlines. LF, CR, and CRLF. We use state to count CRLF.
                '\n' | state == FoundCarriageReturn -> Just ((depth, Normal), p1)
                '\n' -> Just ((depth, Normal), nextPositionLine 1 p1)
                '\r' -> Just ((depth, FoundCarriageReturn), nextPositionLine 1 p1)

                -- Add the character’s UTF-16 length to the position and continue.
                _ -> Just ((depth, Normal), nextPosition (utf16Length c) p1))
            (((1 :: Int), Normal), nextPosition 2 p0)
            (T.tail t1)

      -- Drop `*/` from the comment text if we successfully parsed a block comment.
      --
      -- If we failed to successfully parse a block comment then that means we reached the end of
      -- the file! Report an error diagnostic to let the user know we expected the block comment
      -- to end before the file.
      blockComment <-
        case finalState of
          FoundAsteriskSlash -> return (BlockComment (T.dropEnd 2 comment) True)
          _ -> do
            _ <- unexpectedEnding p2 ExpectedBlockCommentEnd
            return (BlockComment comment False)

        -- Create the new `acc` value.
      let acc2 = Comment blockComment : acc

      -- If we are on the trailing side and the block comment spans multiple lines then stop
      -- parsing trivia.
      case side of
        Leading -> nextTrivia Leading acc2 p2 t2
        Trailing | positionLine p0 /= positionLine p2 -> return (reverse acc2, p2, t2)
        Trailing -> nextTrivia Trailing acc2 p2 t2

    -- Other whitespace
    Just (c, t1) | isSpace c ->
      nextTrivia side (OtherWhitespace c : acc) (nextPosition (utf16Length c) p0) t1

    -- Return trivia if there isn’t more.
    _ -> return (reverse acc, p0, t0)

data BlockCommentState
  = Normal
  | FoundAsterisk
  | FoundAsteriskSlash
  | FoundSlash
  | FoundCarriageReturn
  deriving (Eq)
