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

module Brite.Diagnostic
  ( Diagnostic
  , diagnosticRange

  -- Diagnostic constructors.
  , ExpectedToken(..)
  , unexpectedToken
  , unexpectedEnding
  , unboundTypeVariable

  -- Diagnostic unification stacks.
  , UnifyStack
  , testStack
  , functionParameterFrame
  , functionBodyFrame

  -- Diagnostic message printers.
  , diagnosticMessage
  , diagnosticMessageText
  , debugDiagnostic

  -- Diagnostic reporting monad.
  , DiagnosticMonad(..)
  , DiagnosticWriter
  , runDiagnosticWriter
  , runDiagnosticWriterAdvanced
  ) where

import Brite.DiagnosticMarkup
import Brite.Syntax.Tokens
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

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
  = UnexpectedToken ActualToken ExpectedToken
  -- The parser ran into the end of the source document unexpectedly.
  | UnexpectedEnding ExpectedToken
  -- The type checker ran into a type variable which it could not find a binding for.
  | UnboundTypeVariable Identifier

data WarningDiagnosticMessage

data InfoDiagnosticMessage

-- What token did we actually get?
data ActualToken
  = ActualGlyph Glyph
  | ActualIdentifier
  | ActualChar Char

-- What token did we expect?
data ExpectedToken
  = ExpectedGlyph Glyph
  | ExpectedIdentifier
  | ExpectedEnd
  | ExpectedBlockCommentEnd
  | ExpectedStatement
  | ExpectedExpression
  | ExpectedPattern
  | ExpectedType

-- The parser ran into a token it did not recognize.
unexpectedToken :: DiagnosticMonad m => Token -> ExpectedToken -> m Diagnostic
unexpectedToken token expected = report $ Diagnostic (tokenRange token) $ Error $
  UnexpectedToken unexpected expected
  where
    unexpected = case tokenKind token of
      Glyph glyph -> ActualGlyph glyph
      IdentifierToken _ -> ActualIdentifier
      UnexpectedChar c -> ActualChar c

-- The parser ran into the end of the source document unexpectedly.
unexpectedEnding :: DiagnosticMonad m => Range -> ExpectedToken -> m Diagnostic
unexpectedEnding range expected = report $ Diagnostic range $ Error $
  UnexpectedEnding expected

-- The type checker ran into a type variable which it could not find a binding for.
unboundTypeVariable :: DiagnosticMonad m => Range -> Identifier -> m Diagnostic
unboundTypeVariable range name = report $ Diagnostic range $ Error $
  UnboundTypeVariable name

-- For error reporting we keep track of the unification “stack”. The unification stack has an
-- operation which represents _why_ the unification is happening. The unification stack also has a
-- list of stack frames which are used to describe _where_ unification went wrong.
--
-- The unify stack is entirely for error message presentation. It should contribute to type checking
-- in any way! To enforce this, we don’t expose the underlying `UnifyStack` data type. Which means
-- you can’t pattern match against it outside of this module. Instead we expose constructors.
data UnifyStack
  -- Each unification stack operation holds the range at which the operation occurs along with a
  -- description of the operation.
  = UnifyStackOperation Range UnifyStackOperation

  -- Each unification stack frame holds two ranges. Each range represents the left and right types
  -- of the unification respectively.
  --
  -- In error message we use the left type as the “actual” type and the right type as the “expected
  -- type. In the unification algorithm the order of the types doesn’t matter, but for error
  -- messages we do take the order into consideration since we can provide a better error message.
  -- Some frames will “flip” what types we think of as actual and expected. This is based on whether
  -- we are unifying in an input or output position.
  | UnifyStackFrame {- Range Range -} UnifyStackFrame UnifyStack

data UnifyStackOperation
  = UnifyTest

data UnifyStackFrame
  = UnifyFunctionParameter
  | UnifyFunctionBody

-- An operation we use in testing of Brite itself. We should never use this in release code!
testStack :: Range -> UnifyStack
testStack range = UnifyStackOperation range UnifyTest

-- Adds a function parameter frame to the unification stack.
functionParameterFrame :: UnifyStack -> UnifyStack
functionParameterFrame = UnifyStackFrame UnifyFunctionParameter

-- Adds a function body frame to the unification stack.
functionBodyFrame :: UnifyStack -> UnifyStack
functionBodyFrame = UnifyStackFrame UnifyFunctionBody

-- Creates the human readable diagnostic message for a given diagnostic. Remember that this
-- generates a new message every time it is called instead of fetching a pre-generated message.
diagnosticMessage :: Diagnostic -> Markup
diagnosticMessage diagnostic =
  case diagnosticRawMessage diagnostic of
    Error message -> diagnosticErrorMessage message
    Warning _ -> error "unreachable"
    Info _ -> error "unreachable"

-- The text of a diagnostic message.
diagnosticMessageText :: Diagnostic -> Text.Builder
diagnosticMessageText = toText . diagnosticMessage

diagnosticErrorMessage :: ErrorDiagnosticMessage -> Markup

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
  plain "We wanted "
    <> expectedTokenDescription expected
    <> plain " but we found "
    <> unexpectedDescription
    <> plain "."
  where
    unexpectedDescription = case unexpected of
      ActualGlyph glyph -> code (glyphText glyph)
      ActualIdentifier -> plain "a variable name"
      ActualChar c -> code (Text.singleton c)

-- Follows the same format as the unexpected token error. Except instead of saying “we found the end
-- of the file” we say “We wanted an expression but the file ended.” This is less abstract than
-- saying “we found the file’s end.” The end of a file is an abstract concept and so finding the end
-- of a file is a bit weird. It makes sense from the perspective of parsing but not from the user’s
-- perspective which we are designing for.
diagnosticErrorMessage (UnexpectedEnding expected) =
  plain "We wanted " <> expectedTokenDescription expected <> plain " but the file ended."

-- Other options considered include:
--
-- * “`T` does not exist.” This is too forceful and puts all the blame on the programmer. It also
--   might be wrong. `T` could exist, it just might be in another scope or file.
-- * “Could not find type `T`.” Adding “we” makes the message a bit more personal. It shifts blame
--   from the programmer to the compiler.
-- * “We cannot find type `T`.” I like the sound of “could not” better than “cannot” or “can’t”.
-- * “We could not find name `T`.” We use “type” instead of “name” to be more specific here.
diagnosticErrorMessage (UnboundTypeVariable name) =
  plain "We could not find type " <> code (identifierText name) <> plain "."

-- Get the description of an expected token.
expectedTokenDescription :: ExpectedToken -> Markup
expectedTokenDescription (ExpectedGlyph glyph) = code (glyphText glyph)
expectedTokenDescription ExpectedIdentifier = plain "a variable name"
expectedTokenDescription ExpectedEnd = plain "nothing more"
expectedTokenDescription ExpectedBlockCommentEnd = code "*/"
expectedTokenDescription ExpectedStatement = plain "a statement"
expectedTokenDescription ExpectedExpression = plain "an expression"
expectedTokenDescription ExpectedPattern = plain "a variable name"
expectedTokenDescription ExpectedType = plain "a type"

-- Prints a diagnostic for debugging purposes.
debugDiagnostic :: Diagnostic -> Text.Builder
debugDiagnostic diagnostic =
  Text.Builder.singleton '('
    <> debugRange (diagnosticRange diagnostic)
    <> Text.Builder.fromText ") "
    <> toText (diagnosticMessage diagnostic)

-- A diagnostic monad will allow us to report diagnostics. The diagnostics we report will go into
-- a list where they may be read from later.
class Monad m => DiagnosticMonad m where
  report :: Diagnostic -> m Diagnostic

-- A simple diagnostic writer monad which writes diagnostics to a sequence.
--
-- [1]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Endo
data DiagnosticWriter a = DiagnosticWriter
  { unDiagnosticWriter :: Seq Diagnostic -> (a, Seq Diagnostic)
  }

instance Functor DiagnosticWriter where
  fmap f wa = DiagnosticWriter $ \ds0 ->
    let (a, ds1) = unDiagnosticWriter wa ds0 in
      (f a, ds1)
  {-# INLINE fmap #-}

instance Applicative DiagnosticWriter where
  pure a = DiagnosticWriter (\ds -> (a, ds))
  {-# INLINE pure #-}

  wf <*> wa = DiagnosticWriter $ \ds0 ->
    let
      (f, ds1) = unDiagnosticWriter wf ds0
      (a, ds2) = unDiagnosticWriter wa ds1
    in
      (f a, ds2)
  {-# INLINE (<*>) #-}

instance Monad DiagnosticWriter where
  wa >>= f = DiagnosticWriter $ \ds0 ->
    let (a, ds1) = unDiagnosticWriter wa ds0 in
      unDiagnosticWriter (f a) ds1
  {-# INLINE (>>=) #-}

instance DiagnosticMonad DiagnosticWriter where
  report d = DiagnosticWriter (\ds -> (d, ds |> d))
  {-# INLINE report #-}

-- Runs a `DiagnosticWriter` monad.
runDiagnosticWriter :: DiagnosticWriter a -> (a, Seq Diagnostic)
runDiagnosticWriter wa = unDiagnosticWriter wa Seq.empty
{-# INLINE runDiagnosticWriter #-}

-- Runs a `DiagnosticWriter` monad while allowing an initial sequence of diagnostics to be provided.
runDiagnosticWriterAdvanced :: DiagnosticWriter a -> Seq Diagnostic -> (a, Seq Diagnostic)
runDiagnosticWriterAdvanced = unDiagnosticWriter
{-# INLINE runDiagnosticWriterAdvanced #-}
