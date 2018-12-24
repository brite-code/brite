-- Diagnostic messages are our primary way to communicate with the user. We better be really sure
-- our messages are good.
--
-- In the future, we should consider consulting a copy editor to professionalize our error message
-- style. We may also want to consider running A/B tests.
--
-- # Style Guide
--
-- What makes a good error message? These guides are designed to produce the clearest message
-- possible. Follow these guides to create good, consistent, error messages.
--
-- * Keep diagnostic messages short. Preferably a single, clear, sentence. This format works
--   best for all our target editors. Consider VSCode which uses a hover dialog for viewing
--   errors inline and a “problems” panel to view all errors in a project. Short single-line
--   messages work best in both of these locations.
--
-- * Use correct English grammar. It can be hard to make a program which produces correct English
--   grammar. If you must, consult a spellchecker.
--
-- * Write messages in first-person plural. That is, use “we”. For example “we found an error”.
--   This personifies our type checker as a team of people looking for bugs in the programmer’s
--   code. By personifying our type checker error messages feel like a dialogue. Elm’s error
--   messages are famous for using first-person tense. I (Caleb) always found messages like “I
--   found an error” to be a bit annoying since the type checker is certainly not a person nor is
--   it built by a single person. Hopefully “we” will be a nice compromise.
--
-- * Use language the programmer will understand. Not language the compiler understands. Words
--   like “identifier”, “token”, and “expression” are compiler speak. Instead of compiler speak
--   like “identifier” use a phrase like “variable name”.
--
-- * Any text that might be written in code should use inline code markup formatting from the
--   `Markup` object. This should then be rendered as inline code blocks by diagnostic
--   message renderers.
--
-- * If you use quotes, make sure they are curly quotes. For instance “phrase” instead
--   of "phrase". Same for single quotes. For instance ‘phrase’ instead of 'phrase'. Unless you
--   are talking about quotes inside of code.
--
-- ## Helpful Tools
--
-- Some tools we find are helpful when designing on an error message.
--
-- * [Grammarly](https://www.grammarly.com) for confirming your grammar is correct.
-- * [Hemingway Editor](http://www.hemingwayapp.com) for reducing the complexity of your writing.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Diagnostics
  ( Diagnostic
  , diagnosticRange
  , DiagnosticMonad
  , DiagnosticWriter
  , runDiagnosticWriter
  , ExpectedToken(..)
  , unexpectedToken
  , unexpectedEnding
  , diagnosticMessage
  , debugDiagnostic
  ) where

import qualified Brite.Diagnostics.Markup as M
import Brite.Source
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

-- A diagnostic is some message presented to the user about their program. Diagnostics contain a
-- range of characters which the diagnostic points to. Diagnostics are only valid in the scope of
-- some resource since while they contain a range they do not contain the document that range
-- applies to.
--
-- Our diagnostic format is based on the [Language Server Protocol][1].
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
data Diagnostic = Diagnostic
  { diagnosticRange :: Range
  , diagnosticRawMessage :: DiagnosticMessage
  }

-- The diagnostic message. Includes the severity of the message. Each diagnostic may have some
-- related information.
--
-- Diagnostic messages may not be constructed outside of this module. We always construct and report
-- a diagnostic at the same time as well.
data DiagnosticMessage
  -- Error diagnostics must be resolved by the programmer. Error diagnostics will prevent the
  -- program from being deployed. However, the program may still run in development, but
  -- executing a program with errors will result in Undefined Behavior.
  = Error ErrorDiagnosticMessage
  -- Warning diagnostics may optionally be resolved by the programmer. They exist to highlight
  -- code which is technically correct but might be suboptimal. Warnings will not block
  -- deployment of a program by default. However, it is strongly recommended that warnings
  -- be fixed.
  | Warning WarningDiagnosticMessage
  -- Useful information about a user’s program that does not need to be changed. Unlike a warning
  -- where we are recommending a code change.
  | Info InfoDiagnosticMessage

data ErrorDiagnosticMessage
  -- The parser ran into a token it did not recognize.
  = UnexpectedToken TokenKind ExpectedToken
  -- The parser ran into the end of the source document unexpectedly.
  | UnexpectedEnding ExpectedToken

data WarningDiagnosticMessage

data InfoDiagnosticMessage

-- A diagnostic monad will allow us to report diagnostics. The diagnostics we report will go into
-- a list where they may be read from later.
class Monad m => DiagnosticMonad m where
  report :: Diagnostic -> m Diagnostic

-- A simple diagnostic writer monad which writes diagnostics to a list.
--
-- Uses a function to build up the diagnostics list so that we can use cons (`:`) instead of append
-- (`++`). This is basically the same as using `WriterT` with the [`Endo`][1] monoid.
--
-- [1]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Endo
data DiagnosticWriter a = DiagnosticWriter a ([Diagnostic] -> [Diagnostic])

instance Functor DiagnosticWriter where
  fmap f (DiagnosticWriter a ds) = DiagnosticWriter (f a) ds

instance Applicative DiagnosticWriter where
  pure a = DiagnosticWriter a id
  (DiagnosticWriter f x) <*> (DiagnosticWriter a y) = DiagnosticWriter (f a) (x . y)

instance Monad DiagnosticWriter where
  (DiagnosticWriter a x) >>= f =
    let
      (DiagnosticWriter b y) = f a
    in
      DiagnosticWriter b (x . y)

instance DiagnosticMonad DiagnosticWriter where
  report d = DiagnosticWriter d (d :)

-- Runs a `DiagnosticWriter` monad.
runDiagnosticWriter :: DiagnosticWriter a -> (a, [Diagnostic])
runDiagnosticWriter (DiagnosticWriter a ds) = (a, ds [])

-- What token did we expect?
data ExpectedToken
  = ExpectedGlyph Glyph
  | ExpectedIdentifier
  | ExpectedEnd
  | ExpectedExpression
  | ExpectedPattern

-- The parser ran into a token it did not recognize.
unexpectedToken :: DiagnosticMonad m => Range -> TokenKind -> ExpectedToken -> m Diagnostic
unexpectedToken range unexpected expected = report $ Diagnostic range $ Error $
  UnexpectedToken unexpected expected

-- The parser ran into the end of the source document unexpectedly.
unexpectedEnding :: DiagnosticMonad m => Range -> ExpectedToken -> m Diagnostic
unexpectedEnding range expected = report $ Diagnostic range $ Error $
  UnexpectedEnding expected

-- Creates the human readable diagnostic message for a given diagnostic. Remember that this
-- generates a new message every time it is called instead of fetching a pre-generated message.
diagnosticMessage :: Diagnostic -> M.Markup
diagnosticMessage diagnostic =
  case diagnosticRawMessage diagnostic of
    Error message -> diagnosticErrorMessage message
    Warning _ -> error "unreachable"
    Info _ -> error "unreachable"

diagnosticErrorMessage :: ErrorDiagnosticMessage -> M.Markup

-- Thought and care that went into this error message:
--
-- * When designing this message we started with “Unexpected character `%`. Expected expression.”
--   and ended with the message “We found `%` when we wanted an expression.” The latter uses smaller
--   words. It isn’t abrupt. It personifies the type checker with “we”.
--
-- * The message starts with what we wanted and ends with what we found. Instead of saying
--   “We found `%` when we expected an expression.” the message reads “We wanted an expression but
--   we found `%`.” This gets to the resolution of the error message faster. In most cases the
--   programmer only really needs to see “We wanted an expression” to know the solution.
--
-- * Instead of “we found a `%` character” we print the message as “we found `%`”. The latter is
--   shorter. It is also very hard to choose correctly between “a” and “an” for arbitrary user
--   input. For example this is wrong “a `=` character” since `=` is pronounced “equals” which
--   starts with a vowel sound. It should be “an `=` character”. We are unaware of a way to
--   correctly guess the pronunciation people use for glyphs in general.
--
-- * For unexpected tokens when we expected a pattern we say “We found `=` when we wanted a variable
--   name.” because the word “pattern” is compiler speak. Even though patterns can be more than a
--   variable name, 80% of the time the programmer will write a variable name.
diagnosticErrorMessage (UnexpectedToken unexpected expected) =
  M.plain "We wanted "
    <> expectedTokenDescription expected
    <> M.plain " but we found "
    <> unexpectedDescription
    <> M.plain "."
  where
    unexpectedDescription = case unexpected of
      Glyph glyph -> M.code (glyphText glyph)
      IdentifierToken _ -> M.plain "a variable name"
      UnexpectedChar c -> M.code (T.singleton c)

-- Follows the same format as the unexpected token error. Except instead of saying “we found the end
-- of the file” we say “We wanted an expression but the file ended.” This is less abstract than
-- saying “we found the file’s end.” The end of a file is an abstract concept and so finding the end
-- of a file is a bit weird. It makes sense from the perspective of parsing but not from the user’s
-- perspective which we are designing for.
diagnosticErrorMessage (UnexpectedEnding expected) =
  M.plain "We wanted " <> expectedTokenDescription expected <> M.plain " but the file ended."

-- Get the description of an expected token.
expectedTokenDescription :: ExpectedToken -> M.Markup
expectedTokenDescription (ExpectedGlyph glyph) = M.code (glyphText glyph)
expectedTokenDescription ExpectedIdentifier = M.plain "a variable name"
expectedTokenDescription ExpectedEnd = M.plain "nothing more"
expectedTokenDescription ExpectedExpression = M.plain "an expression"
expectedTokenDescription ExpectedPattern = M.plain "a variable name"

-- Prints a diagnostic for debugging purposes.
debugDiagnostic :: Diagnostic -> B.Builder
debugDiagnostic diagnostic =
  B.singleton '('
    <> debugRange (diagnosticRange diagnostic)
    <> B.fromText ") "
    <> M.toBuilder (diagnosticMessage diagnostic)
    <> B.singleton '\n'
