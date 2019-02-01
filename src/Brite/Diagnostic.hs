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
-- * Write messages in first-person plural. That is, use “we”. For example “we see an error”.
--   This personifies our type checker as a team of people looking for bugs in the programmer’s
--   code. By personifying our type checker error messages feel like a dialogue. Elm’s error
--   messages are famous for using first-person. I (Caleb) always found messages like “I found an
--   error” to be a bit annoying since the type checker is certainly not a person nor is it built by
--   a single person. Hopefully “we” will be a nice compromise.
--
-- * When speaking, present tense instead of past tense. Instead of “we found” say “we see”. An
--   error in the programmer’s code is not a point in time, but rather a state in which the code
--   will remain until the bug is fixed. While yes, the type checker runs at discrete points in time
--   we want to give the user the perception that Brite is alive and reacting to their input. Not
--   spinning in a background thread and then spitting out errors every once in a while.
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
  , UnexpectedSyntax(..)
  , ExpectedSyntax(..)
  , TypeMessage(..)
  , unexpectedSyntax
  , unexpectedChar
  , unexpectedEnding
  , unboundVariable
  , unboundTypeVariable
  , incompatibleTypes
  , infiniteType
  , expectedTypeVariableToExist

  -- Diagnostic unification stacks.
  , UnifyStack
  , unifyTestStack
  , functionCallStack
  , expressionAnnotationStack
  , conditionalTestStack
  , conditionalBranchesStack
  , functionParameterFrame
  , functionBodyFrame

  -- Diagnostic message printers.
  , diagnosticMessage
  , diagnosticMessageText
  , debugDiagnostic

  -- Shared messages.
  , issueTrackerMessage

  -- Diagnostic reporting monad.
  , DiagnosticMonad(..)
  , DiagnosticWriter
  , runDiagnosticWriter
  , runDiagnosticWriterAdvanced
  ) where

import Brite.DiagnosticMarkup
import Brite.Semantics.Polarity
import Brite.Syntax.Glyph
import Brite.Syntax.Identifier
import Brite.Syntax.Range
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
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
  -- An internal error ocurred which should never happen in production code.
  = InternalError InternalErrorDiagnosticMessage
  -- The parser ran into syntax it did not recognize.
  | UnexpectedSyntax UnexpectedSyntax ExpectedSyntax
  -- The parser ran into the end of the source document unexpectedly.
  | UnexpectedEnding ExpectedSyntax
  -- The type checker ran into a variable which it could not find a binding for.
  | UnboundVariable Identifier
  -- The type checker ran into a type variable which it could not find a binding for.
  | UnboundTypeVariable Identifier
  -- We found two types that were incompatible with one another during unification.
  | IncompatibleTypes TypeMessage TypeMessage UnifyStack
  -- While trying to infer the type for some code we ran into an infinite type.
  | InfiniteType UnifyStack

data InternalErrorDiagnosticMessage
  -- We expected a type variable to exist in the prefix and it didn’t we throw this error.
  = ExpectedTypeVariableToExist Identifier

data WarningDiagnosticMessage

data InfoDiagnosticMessage

-- What syntax did we not expect?
data UnexpectedSyntax
  = UnexpectedGlyph Glyph
  | UnexpectedIdentifier
  | UnexpectedNumber
  | UnexpectedChar' Char

-- What syntax did we expect?
data ExpectedSyntax
  = ExpectedGlyph Glyph
  | ExpectedIdentifier
  | ExpectedNumber
  | ExpectedEnd
  | ExpectedBlockCommentEnd
  | ExpectedDecimalDigit
  | ExpectedBinaryDigit
  | ExpectedHexadecimalDigit
  | ExpectedStatement
  | ExpectedExpression
  | ExpectedPattern
  | ExpectedType

-- A short description of a type that is used in error messages. Designed to not reveal the
-- underlying implementation of the Brite type checker.
data TypeMessage
  = CodeMessage Text
  | VoidMessage
  | BooleanMessage
  | IntegerMessage
  | FunctionMessage

-- The parser ran into syntax it did not recognize.
unexpectedSyntax :: DiagnosticMonad m => Range -> UnexpectedSyntax -> ExpectedSyntax -> m Diagnostic
unexpectedSyntax range unexpected expected = report $ Diagnostic range $ Error $
  UnexpectedSyntax unexpected expected

-- The parser ran into a character it did not recognize.
unexpectedChar :: DiagnosticMonad m => Position -> Char -> ExpectedSyntax -> m Diagnostic
unexpectedChar position char expected = unexpectedSyntax range (UnexpectedChar' char) expected
  where range = Range position (nextPosition (utf16Length char) position)

-- The parser ran into the end of the source document unexpectedly.
unexpectedEnding :: DiagnosticMonad m => Position -> ExpectedSyntax -> m Diagnostic
unexpectedEnding position expected = report $ Diagnostic (Range position position) $ Error $
  UnexpectedEnding expected

-- The type checker ran into a variable which it could not find a binding for.
unboundVariable :: DiagnosticMonad m => Range -> Identifier -> m Diagnostic
unboundVariable range name = report $ Diagnostic range $ Error $
  UnboundVariable name

-- The type checker ran into a type variable which it could not find a binding for.
unboundTypeVariable :: DiagnosticMonad m => Range -> Identifier -> m Diagnostic
unboundTypeVariable range name = report $ Diagnostic range $ Error $
  UnboundTypeVariable name

-- We found two types that were incompatible with one another during unification.
--
-- We get the range for the diagnostic from our unification stack. The unification stack should hold
-- the range most relevant to the programmer.
incompatibleTypes :: DiagnosticMonad m => TypeMessage -> TypeMessage -> UnifyStack -> m Diagnostic
incompatibleTypes type1 type2 stack = report $ Diagnostic (unifyStackRange stack) $ Error $
  IncompatibleTypes type1 type2 stack

-- While trying to infer the type for some code we ran into an infinite type.
infiniteType :: DiagnosticMonad m => UnifyStack -> m Diagnostic
infiniteType stack = report $ Diagnostic (unifyStackRange stack) $ Error $
  InfiniteType stack

-- We expected a type variable to exist in the prefix and it didn’t we throw this error.
expectedTypeVariableToExist :: DiagnosticMonad m => Identifier -> UnifyStack -> m Diagnostic
expectedTypeVariableToExist name stack = report $ Diagnostic (unifyStackRange stack) $ Error $ InternalError $
  ExpectedTypeVariableToExist name

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

  -- Each unification stack frame holds a range in the programmer’s code of the type being unified.
  -- Each unification stack frame also carries the polarity of the current position we are unifying.
  -- If the polarity is positive then our range is the range of the _actual_ type being unified
  -- at this position. If the polarity is negative then our range is the range of the _expected_
  -- type being unified at this position.
  --
  -- Our unification algorithm never changes the order of the types being unified even though the
  -- unification algorithm doesn’t care about the order of the types. However, error reporting does
  -- care about the order.
  --
  -- For the purpose of error reporting, the first type in a unification is the “actual” type. The
  -- type of a value in source code that we are comparing against the second type. The “expected”
  -- type which could be some type annotation the programmer wrote.
  --
  -- We switch between reporting the actual and expected type based on polarity because an input
  -- type flips which type is _expected_ by the programmer to what type is _actually_ provided.
  | UnifyStackFrame Polarity Range UnifyStackFrame UnifyStack

data UnifyStackOperation
  = UnifyTest
  | UnifyFunctionCall (Maybe Text)
  | UnifyExpressionAnnotation
  | UnifyConditionalTest
  | UnifyConditionalBranches

data UnifyStackFrame
  = UnifyFunctionParameter
  | UnifyFunctionBody

-- Gets the range of a unification stack. We start with the range of the operation and select the
-- smallest range inside of the unification stack that is still contained by the operation range.
unifyStackRange :: UnifyStack -> Range
unifyStackRange (UnifyStackOperation range _) = range
unifyStackRange (UnifyStackFrame _ range1 _ stack) =
  let range2 = unifyStackRange stack in
    if rangeContains range2 range1 then range1 else range2

-- Gets the polarity of a unification stack.
unifyStackPolarity :: UnifyStack -> Polarity
unifyStackPolarity (UnifyStackOperation _ _) = Positive
unifyStackPolarity (UnifyStackFrame polarity _ _ _) = polarity

-- Adds a unification stack frame to the unification stack. Picks the appropriate range based on the
-- current polarity.
unifyStackFrame :: Range -> Range -> UnifyStackFrame -> UnifyStack -> UnifyStack
unifyStackFrame range1 range2 frame stack =
  UnifyStackFrame polarity range frame stack
  where
    polarity = unifyStackPolarity stack
    range = case polarity of { Positive -> range1; Negative -> range2 }

-- Adds a unification stack frame to the unification stack. Flips the polarity and picks the
-- appropriate range based on the new polarity.
unifyStackFrameFlipPolarity :: Range -> Range -> UnifyStackFrame -> UnifyStack -> UnifyStack
unifyStackFrameFlipPolarity range1 range2 frame stack =
  UnifyStackFrame polarity range frame stack
  where
    polarity = flipPolarity (unifyStackPolarity stack)
    range = case polarity of { Positive -> range1; Negative -> range2 }

-- An operation we use in testing of Brite itself. We should never use this in release code!
unifyTestStack :: Range -> UnifyStack
unifyTestStack range = UnifyStackOperation range UnifyTest

-- A function call operation: `f()`
functionCallStack :: Range -> Maybe Text -> UnifyStack
functionCallStack range name = UnifyStackOperation range (UnifyFunctionCall name)

-- An expression annotation operation: `(e: T)`
expressionAnnotationStack :: Range -> UnifyStack
expressionAnnotationStack range = UnifyStackOperation range UnifyExpressionAnnotation

-- A conditional expression test: `if E {}`
conditionalTestStack :: Range -> UnifyStack
conditionalTestStack range = UnifyStackOperation range UnifyConditionalTest

-- When we unify two conditional branches together: `if _ { E1 } else { E2 }`
conditionalBranchesStack :: Range -> UnifyStack
conditionalBranchesStack range = UnifyStackOperation range UnifyConditionalBranches

-- Adds a function parameter frame to the unification stack.
--
-- The range provided should be the range of the _actual_ type in unification.
functionParameterFrame :: Range -> Range -> UnifyStack -> UnifyStack
functionParameterFrame range1 range2 stack =
  unifyStackFrameFlipPolarity range1 range2 UnifyFunctionParameter stack

-- Adds a function body frame to the unification stack.
--
-- The range provided should be the range of the _actual_ type in unification.
functionBodyFrame :: Range -> Range -> UnifyStack -> UnifyStack
functionBodyFrame range1 range2 stack =
  unifyStackFrame range1 range2 UnifyFunctionBody stack

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
--
-- NOTE: This message is written in past tense which disagrees with the tone we want to set. We
-- should change the message.
diagnosticErrorMessage (UnexpectedSyntax unexpected expected) =
  plain "We wanted "
    <> expectedSyntaxMessage expected
    <> plain " but we found "
    <> unexpectedDescription
    <> plain "."
  where
    unexpectedDescription = case unexpected of
      UnexpectedGlyph glyph -> code (glyphText glyph)
      UnexpectedIdentifier -> plain "a variable name"
      UnexpectedNumber -> plain "a number"
      UnexpectedChar' c -> code (Text.singleton c)

-- Follows the same format as the unexpected token error. Except instead of saying “we found the end
-- of the file” we say “We wanted an expression but the file ended.” This is less abstract than
-- saying “we found the file’s end.” The end of a file is an abstract concept and so finding the end
-- of a file is a bit weird. It makes sense from the perspective of parsing but not from the user’s
-- perspective which we are designing for.
--
-- NOTE: This message is written in past tense which disagrees with the tone we want to set. We
-- should change the message.
diagnosticErrorMessage (UnexpectedEnding expected) =
  plain "We wanted " <> expectedSyntaxMessage expected <> plain " but the file ended."

-- Instead of saying “we could not find value `X`” or “we could not find name `X`” we say “we could
-- not find `X`” since it should be pretty obvious to the programmer that we are referring to
-- a variable. We say “we could not find type `X`” for type error messages to be more clear since
-- types are generally more abstract.
--
-- See `UnboundTypeVariable` for more thought that went into this message.
--
-- NOTE: This message is written in past tense which disagrees with the tone we want to set. We
-- should change the message.
diagnosticErrorMessage (UnboundVariable name) =
  plain "We could not find " <> code (identifierText name) <> plain "."

-- Other options considered include:
--
-- * “`T` does not exist.” This is too forceful and puts all the blame on the programmer. It also
--   might be wrong. `T` could exist, it just might be in another scope or file.
-- * “Could not find type `T`.” Adding “we” makes the message a bit more personal. It shifts blame
--   from the programmer to the compiler.
-- * “We cannot find type `T`.” I like the sound of “could not” better than “cannot” or “can’t”.
-- * “We could not find name `T`.” We use “type” instead of “name” to be more specific here.
--
-- NOTE: This message is written in past tense which disagrees with the tone we want to set. We
-- should change the message.
diagnosticErrorMessage (UnboundTypeVariable name) =
  plain "We could not find type " <> code (identifierText name) <> plain "."

-- A Brite programmer will see this error message quite frequently so we need to take some time and
-- make sure it’s real good.
--
-- We get an incompatible types error message when during unification we find two types that are
-- incompatible. We will add those two types in an error message in addition to the unification
-- stack at that point. The unification stack allows us to keep track of where we are during
-- unification to provide the best possible error message.
--
-- We start our error message by referencing the unification operation. “Cannot call”,
-- “Cannot assign”, etc. This allows us to tie our incompatibility _to an actual place in the
-- programmer’s code_. We don’t just say “these two types are incompatible”, we say “you can’t do
-- this _because_ you didn’t provide the right types”.
--
-- We then tell the programmer the type we found and _then_ the type we expected. This order was
-- carefully thought of. The expected type is pretty static. It doesn’t change much over time.
-- However, the programmer is constantly changing which values should flow into the expected type.
-- Even if, say, the programmer changes the type of a function parameter (an example of an expected
-- type) they will then go to _all_ the code sites where that function was called and update the
-- values being passed.
--
-- Finally, there’s the matter of the unification stack frames. We currently don’t report the
-- unification stack frames. Mostly because I want to wait until I have some examples of when the
-- unification stack frames will be useful for debugging an error. For now we keep the diagnostic
-- error messages short, sweet, and to the point.
diagnosticErrorMessage (IncompatibleTypes type1 type2 stack) =
  operationMessage <> plain " because we have " <> typeMessage actualType <> plain " but we want " <>
  typeMessage expectedType <> plain "."
  where
    (actualType, expectedType) = case unifyStackPolarity stack of
      Positive -> (type1, type2)
      Negative -> (type2, type1)

    operationMessage = loop stack

    loop (UnifyStackOperation _ operation) = unifyStackOperationMessage operation
    loop (UnifyStackFrame _ _ _ nestedStack) = loop nestedStack

-- Infinite types are rare and tricky to understand. We don’t expect beginner programmers to see
-- this error. To see this error you need to be using both recursion and lots of polymorphism. Both
-- are features beginners tend to avoid.
--
-- We really only expect intermediate to advanced programmers to encounter this error. These
-- programmers will either be able to solve this error on their own or will be able to correctly
-- phrase a question on their favored Brite support forums. This is why we use explicit compiler-y
-- names like “type checker”, “inference”, and “infinite type”. An advanced user will be helped by
-- being explicit about which particular components are at fault.
--
-- The error message is intentionally vague. It is very unlikely that the compiler will be able to
-- spot the actual problem. Only a human will be able to spot it. If the compiler tries to guess and
-- it guesses wrong then the programmer will spend too much time trying to understand the compiler’s
-- output and not enough time trying to understand what in their code caused the error.
--
-- For instance, the error is triggered by an “occurs” check when updating a type variable. We check
-- to see if the type variable exists already in the type it is being updated to. Just because the
-- occurs check fails, we don’t know that it’s that type variable that is the problem. The type
-- variable we were updating just got unlucky. It could be any type variable in that type.
diagnosticErrorMessage (InfiniteType stack) =
  operationMessage <> plain " because the type checker infers an infinite type."
  where
    operationMessage = loop stack

    loop (UnifyStackOperation _ operation) = unifyStackOperationMessage operation
    loop (UnifyStackFrame _ _ _ nestedStack) = loop nestedStack

-- If a user sees an internal error diagnostic then there’s a bug in Brite. Refer users to the issue
-- tracker so they can report their problem.
diagnosticErrorMessage (InternalError x) =
  plain "Internal Error: " <> internalErrorDiagnosticMessage x <> plain " " <> issueTrackerMessage
  where
    internalErrorDiagnosticMessage (ExpectedTypeVariableToExist name) =
      plain "Expected type variable " <> code (identifierText name) <> plain " to exist in the prefix."

-- Get the message for an expected token.
expectedSyntaxMessage :: ExpectedSyntax -> Markup
expectedSyntaxMessage (ExpectedGlyph glyph) = code (glyphText glyph)
expectedSyntaxMessage ExpectedIdentifier = plain "a variable name"
expectedSyntaxMessage ExpectedNumber = plain "a number"
expectedSyntaxMessage ExpectedEnd = plain "nothing more"
expectedSyntaxMessage ExpectedBlockCommentEnd = code "*/"

-- If the user types `0b` or `0x` then, presumably, they know what they are doing and want a binary
-- or hexadecimal number. So using phrasing like “hexadecimal digit” will confuse them. If a
-- beginner stumbles upon the error message accidentally they have something clear to search for.
--
-- Otherwise, if the user types an incorrect number like `0px` we will say that we expect a _number_
-- instead of expected a “digit” because “number” is simpler vocabulary.
expectedSyntaxMessage ExpectedDecimalDigit = plain "a number"
expectedSyntaxMessage ExpectedBinaryDigit = plain "a binary digit"
expectedSyntaxMessage ExpectedHexadecimalDigit = plain "a hexadecimal digit"

expectedSyntaxMessage ExpectedStatement = plain "a statement"
expectedSyntaxMessage ExpectedExpression = plain "an expression"
expectedSyntaxMessage ExpectedPattern = plain "a variable name"
expectedSyntaxMessage ExpectedType = plain "a type"

-- Get the message for a type message.
typeMessage :: TypeMessage -> Markup
typeMessage (CodeMessage t) = code t
typeMessage VoidMessage = plain "void"
typeMessage BooleanMessage = plain "a boolean"
typeMessage IntegerMessage = plain "an integer"
typeMessage FunctionMessage = plain "a function"

-- Get the message for a unification stack operation.
unifyStackOperationMessage :: UnifyStackOperation -> Markup
unifyStackOperationMessage UnifyTest = plain "Test failed" -- NOTE: We should only see this during testing.

-- We use “Can not” instead of “Cannot” because the former is simpler to read.
unifyStackOperationMessage (UnifyFunctionCall Nothing) = plain "Can not call function"
unifyStackOperationMessage (UnifyFunctionCall (Just name)) = plain "Can not call " <> code name

-- We don’t want to use the word “cast” even though that’s pretty common programming language
-- terminology. Instead the simpler phrasing of “changing a type” will do.
--
-- TODO: Change “cannot change this type” to “Cannot change `x`’s type”
unifyStackOperationMessage UnifyExpressionAnnotation = plain "Can not change this type"

-- We avoid using the term “condition” or “conditional” as that might be confusing.
--
-- TODO: Change to “Can not test `x`”
unifyStackOperationMessage UnifyConditionalTest = plain "Can not test"

-- TODO: A message???
unifyStackOperationMessage UnifyConditionalBranches = plain "TODO"

-- A message for pushing people to our issue tracker when they encounter an unexpected error.
--
-- We use the word “issue” instead of the word “bug” because the meaning of “bug” is subjective. An
-- error might be expected behavior and we just haven’t created a better error message for it yet.
issueTrackerMessage :: Markup
issueTrackerMessage =
  plain "See if this issue was already reported: https://github.com/brite-code/brite/issues"

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
