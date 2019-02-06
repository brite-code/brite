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
-- * 80% of the time the fix is obvious to human and computer so provide a direct error message.
--   20% of the time the error won’t be obvious to the human so provide a message which is _not_
--   misleading and gives the human enough information to step through their program and find the
--   fix which might involve making tradeoffs a computer couldn’t understand in their program. Never
--   give a human a misleading an error message, they’ll spend more time on the message then on
--   their program.
--
-- * Trust that the programmer is clever unless shown otherwise. Prefer error messages which are
--   always short and true to error messages which are long and misleading/false. If the
--   programmer is clever and you’ve given them enough tools (error messages, extra references, IDE
--   tools like hover types) the clever programmer should be able to deduce the real problem in
--   their code. If it is shown the programmer is not clever enough to solve the error on their own
--   with the given error message then consider giving them a better error message.
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
  , unexpectedSyntax
  , unexpectedChar
  , unexpectedEnding
  , unboundVariable
  , unboundTypeVariable
  , incompatibleTypes
  , missingProperty
  , extraProperty
  , doesNotAbstract
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
  , objectPropertyFrame
  , objectExtensionFrame
  , isFunctionCall

  -- Diagnostic message printers.
  , diagnosticMessageText
  , diagnosticMessageCompact
  , diagnosticMessageMarkdown

  -- Shared messages.
  , issueTrackerMessage

  -- Diagnostic reporting monad.
  , DiagnosticMonad(..)
  , DiagnosticWriter
  , runDiagnosticWriter
  , runDiagnosticWriterAdvanced
  ) where

import Brite.DiagnosticMarkup
import Brite.Semantics.TypeNames
import Brite.Syntax.Glyph
import Brite.Syntax.Identifier
import Brite.Syntax.Range
import Brite.Syntax.Snippet
import Data.List (intersperse)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Custom as Text.Builder

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
  -- We found two type constructors that were incompatible with one another during unification.
  | IncompatibleTypes Range Range TypeConstructorSnippet TypeConstructorSnippet UnifyStack
  -- We expected an object to have a property with the provided name.
  | MissingProperty Range Identifier UnifyStack
  -- We have an extra property which we did not expect.
  | ExtraProperty Range Identifier UnifyStack
  -- We expected one type to abstract another but it does not.
  | DoesNotAbstract Range Range Text Text UnifyStack
  -- While trying to infer the type for some code we ran into an infinite type.
  | InfiniteType UnifyStack

data InternalErrorDiagnosticMessage
  -- We expected a type variable to exist in the prefix and it didn’t we throw this error.
  = ExpectedTypeVariableToExist Identifier

data WarningDiagnosticMessage

data InfoDiagnosticMessage

-- Represents a related information for a diagnostic in case the main information was not enough.
-- Most importantly, related information carries a location so we can point to source code which
-- contributed to an error.
--
-- See related information in the [Language Server Protocol (LSP) Specification][1].
--
-- See an example of [related information rendered in VSCode][2].
--
-- [1]: https://microsoft.github.io/language-server-protocol/specification
-- [2]: https://code.visualstudio.com/updates/v1_22#_related-information-in-errors-and-warnings
data DiagnosticRelatedInformation = DiagnosticRelatedInformation
  { diagnosticRelatedInformationRange :: Range
  , diagnosticRelatedInformationMessage :: Markup
  }

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

-- We found two type constructors that were incompatible with one another during unification.
incompatibleTypes :: DiagnosticMonad m => (Range, TypeConstructorSnippet) -> (Range, TypeConstructorSnippet) -> UnifyStack -> m Diagnostic
incompatibleTypes (actualRange, actual) (expectedRange, expected) stack = report $ Diagnostic range $ Error $
  IncompatibleTypes actualRange expectedRange actual expected stack
  where
    stackRange = unifyStackRange stack
    range = if rangeContains stackRange actualRange then actualRange else stackRange

-- We expected an object to have a property with the provided name.
missingProperty :: DiagnosticMonad m => Range -> (Range, Identifier) -> UnifyStack -> m Diagnostic
missingProperty objectRange (propertyRange, propertyName) stack = report $ Diagnostic range $ Error $
  MissingProperty propertyRange propertyName stack
  where
    stackRange = unifyStackRange stack
    range = if rangeContains stackRange objectRange then objectRange else stackRange

-- We have an extra property which we did not expect.
extraProperty :: DiagnosticMonad m => Range -> (Range, Identifier) -> UnifyStack -> m Diagnostic
extraProperty _ (propertyRange, propertyName) stack = report $ Diagnostic range $ Error $
  ExtraProperty propertyRange propertyName stack
  where
    stackRange = unifyStackRange stack
    range = if rangeContains stackRange propertyRange then propertyRange else stackRange

-- We expected one type to abstract another but it does not.
doesNotAbstract :: DiagnosticMonad m => (Range, Text) -> (Range, Text) -> UnifyStack -> m Diagnostic
doesNotAbstract (actualRange, actual) (expectedRange, expected) stack = report $ Diagnostic range $ Error $
  DoesNotAbstract actualRange expectedRange actual expected stack
  where
    stackRange = unifyStackRange stack
    range = if rangeContains stackRange actualRange then actualRange else stackRange

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
  | UnifyStackFrame Range UnifyStackFrame UnifyStack

data UnifyStackOperation
  = UnifyTest
  | UnifyFunctionCall ExpressionSnippet
  | UnifyExpressionAnnotation ExpressionSnippet
  | UnifyConditionalTest ExpressionSnippet
  | UnifyConditionalBranches ExpressionSnippet

data UnifyStackFrame
  = UnifyFunctionParameter
  | UnifyFunctionBody
  | UnifyObjectProperty Identifier
  | UnifyObjectExtension

-- Gets the range of a unification stack. We start with the range of the operation and select the
-- smallest range inside of the unification stack that is still contained by the operation range.
unifyStackRange :: UnifyStack -> Range
unifyStackRange (UnifyStackOperation range _) = range
unifyStackRange (UnifyStackFrame range1 _ stack) =
  let range2 = unifyStackRange stack in
    if rangeContains range2 range1 then range1 else range2

-- An operation we use in testing of Brite itself. We should never use this in release code!
unifyTestStack :: Range -> UnifyStack
unifyTestStack range = UnifyStackOperation range UnifyTest

-- A function call operation: `f()`
functionCallStack :: Range -> ExpressionSnippet -> UnifyStack
functionCallStack range snippet = UnifyStackOperation range (UnifyFunctionCall snippet)

-- An expression annotation operation: `(e: T)`
expressionAnnotationStack :: Range -> ExpressionSnippet -> UnifyStack
expressionAnnotationStack range snippet = UnifyStackOperation range (UnifyExpressionAnnotation snippet)

-- A conditional expression test: `if E {}`
conditionalTestStack :: Range -> ExpressionSnippet -> UnifyStack
conditionalTestStack range snippet = UnifyStackOperation range (UnifyConditionalTest snippet)

-- When we unify two conditional branches together: `if _ { E1 } else { E2 }`
--
-- The expression snippet should be from the _test_ expression.
conditionalBranchesStack :: Range -> ExpressionSnippet -> UnifyStack
conditionalBranchesStack range snippet = UnifyStackOperation range (UnifyConditionalBranches snippet)

-- Adds a function parameter frame to the unification stack.
--
-- The range provided should be the range of the _actual_ type in unification.
functionParameterFrame :: Range -> UnifyStack -> UnifyStack
functionParameterFrame range stack = UnifyStackFrame range UnifyFunctionParameter stack

-- Adds a function body frame to the unification stack.
--
-- The range provided should be the range of the _actual_ type in unification.
functionBodyFrame :: Range -> UnifyStack -> UnifyStack
functionBodyFrame range stack = UnifyStackFrame range UnifyFunctionBody stack

-- Adds an object property frame to the unification stack.
--
-- The range provided should be the range of the _actual_ type in unification.
objectPropertyFrame :: Identifier -> Range -> UnifyStack -> UnifyStack
objectPropertyFrame name range stack = UnifyStackFrame range (UnifyObjectProperty name) stack

-- Adds an object extension frame to the unification stack.
--
-- The range provided should be the range of the _actual_ type in unification.
objectExtensionFrame :: Range -> UnifyStack -> UnifyStack
objectExtensionFrame range stack = UnifyStackFrame range UnifyObjectExtension stack

-- Is this unification stack a function call operation? If a frame was added to a function call
-- stack then we will return false.
isFunctionCall :: UnifyStack -> Bool
isFunctionCall (UnifyStackOperation _ (UnifyFunctionCall _)) = True
isFunctionCall _ = False
{-# INLINE isFunctionCall #-}

-- Creates a human readable diagnostic message for a given diagnostic. Also may create some related
-- information regarding the error. Remember that this generates a new message every time it is
-- called instead of fetching a pre-generated message.
diagnosticMessage :: Diagnostic -> (Markup, [DiagnosticRelatedInformation])
diagnosticMessage diagnostic = (message, filteredRelatedInformation)
  where
    (message, relatedInformation) =
      case diagnosticRawMessage diagnostic of
        Error errorMessage -> diagnosticErrorMessage errorMessage
        Warning _ -> error "unreachable"
        Info _ -> error "unreachable"

    -- Remove related information that intersects with our diagnostic’s range. This information
    -- should be easily inferable by the programmer.
    filteredRelatedInformation =
      filter
        (not . rangeIntersects (diagnosticRange diagnostic) . diagnosticRelatedInformationRange)
        relatedInformation

-- The text of a diagnostic message. This will only return the text of a diagnostic! It will not
-- include the range or the related information.
diagnosticMessageText :: Diagnostic -> Text.Builder
diagnosticMessageText = toText . fst . diagnosticMessage

-- Prints a diagnostic compactly on a single line.
diagnosticMessageCompact :: Diagnostic -> Text.Builder
diagnosticMessageCompact diagnostic =
  Text.Builder.singleton '(' <>
  debugRange (diagnosticRange diagnostic) <>
  Text.Builder.fromText ") " <>
  toText message <>
  (if null relatedInformation then mempty else
    Text.Builder.fromText " [" <>
    mconcat (intersperse (Text.Builder.fromText ", ") (map
      (\info ->
        Text.Builder.singleton '(' <>
        debugRange (diagnosticRelatedInformationRange info) <>
        Text.Builder.fromText "): " <>
        toText (diagnosticRelatedInformationMessage info))
      relatedInformation)) <>
    Text.Builder.singleton ']')
  where
    (message, relatedInformation) = diagnosticMessage diagnostic

-- Prints a diagnostic to markdown format. The diagnostic is printed as a markdown bullet. All
-- related information is printed as nested bullets.
diagnosticMessageMarkdown :: Diagnostic -> Text.Builder
diagnosticMessageMarkdown diagnostic =
  Text.Builder.fromText "- (" <>
  debugRange (diagnosticRange diagnostic) <>
  Text.Builder.fromText ") " <>
  toText message <>
  Text.Builder.singleton '\n' <>
  (mconcat (map
    (\info ->
      Text.Builder.fromText "  - (" <>
      debugRange (diagnosticRelatedInformationRange info) <>
      Text.Builder.fromText "): " <>
      toText (diagnosticRelatedInformationMessage info) <>
      Text.Builder.singleton '\n')
    relatedInformation))
  where
    (message, relatedInformation) = diagnosticMessage diagnostic

diagnosticErrorMessage :: ErrorDiagnosticMessage -> (Markup, [DiagnosticRelatedInformation])

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
diagnosticErrorMessage (UnexpectedSyntax unexpected expected) = noRelatedInformation $
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
diagnosticErrorMessage (UnexpectedEnding expected) = noRelatedInformation $
  plain "We wanted " <> expectedSyntaxMessage expected <> plain " but the file ended."

-- We tell the user directly that the name they were looking for is missing. “does not exist” is a
-- bit harsh. It might also be untrue from the user’s point of view. The variable could exist in a
-- different scope or with a small mis-spelling. Instead we use “cannot find” which is simple and to
-- the point.
diagnosticErrorMessage (UnboundVariable name) = noRelatedInformation $
  plain "Can not find " <> code (identifierText name) <> plain "."

-- See `UnboundVariable` for more information on this message.
--
-- We don’t differentiate this message by specifically saying that a type is missing.
diagnosticErrorMessage (UnboundTypeVariable name) = noRelatedInformation $
  plain "Can not find " <> code (identifierText name) <> plain "."

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
-- We use the related information to tell the user the exact location of the expected type. We also
-- show them the exact location of the actual type, but only if our diagnostic wasn’t already
-- pointing to the actual type. If our diagnostic is pointing to the actual type we reduce clutter
-- by not including the extra information.
--
-- Finally, there’s the matter of the unification stack frames. We currently don’t report the
-- unification stack frames. Mostly because I want to wait until I have some examples of when the
-- unification stack frames will be useful for debugging an error. For now we keep the diagnostic
-- error messages short, sweet, and to the point.
diagnosticErrorMessage (IncompatibleTypes actualRange expectedRange actual expected stack) =
  -- Construct the incompatible types message.
  ( operationMessage <> plain " because " <> actualMessage actual <> plain " is not " <>
    expectedMessage expected <> plain "."

  -- The references which are inside the diagnostic range will be hidden.
  , [ DiagnosticRelatedInformation actualRange (referenceMessage actual)
    , DiagnosticRelatedInformation expectedRange (referenceMessage expected)
    ]
  )
  where
    operationMessage = loop stack

    loop (UnifyStackOperation _ operation) = unifyStackOperationMessage operation
    loop (UnifyStackFrame _ _ nestedStack) = loop nestedStack

    actualMessage VoidConstructorSnippet = plain "void"
    actualMessage BooleanConstructorSnippet = code (identifierText booleanTypeName)
    actualMessage IntegerConstructorSnippet = code (identifierText integerTypeName)
    actualMessage FunctionConstructorSnippet = plain "a function"
    actualMessage ObjectConstructorSnippet = plain "an object"

    expectedMessage VoidConstructorSnippet = plain "void"
    expectedMessage BooleanConstructorSnippet = plain "a " <> code (identifierText booleanTypeName)
    expectedMessage IntegerConstructorSnippet = plain "an " <> code (identifierText integerTypeName)
    expectedMessage FunctionConstructorSnippet = plain "a function"
    expectedMessage ObjectConstructorSnippet = plain "an object"

    referenceMessage VoidConstructorSnippet = plain "void"
    referenceMessage BooleanConstructorSnippet = code (identifierText booleanTypeName)
    referenceMessage IntegerConstructorSnippet = code (identifierText integerTypeName)
    referenceMessage FunctionConstructorSnippet = plain "function"
    referenceMessage ObjectConstructorSnippet = plain "object"

-- Missing property errors are a bit hard to write well. In the worst case, a missing property error
-- can point to _many_ lines of code. Like in the case of a large configuration object that is
-- missing a single property.
--
-- It’s also hard to write a good message that doesn’t use technical language. We at least need to
-- mention the object which is missing a property and the property name it is missing.
--
-- We’ve chosen to write this message as “Cannot call `f` because `p:` is missing.” This reduces
-- the number of words we need in our error message. Compared to other options like “object does not
-- have property `p`” or ”object needs property `p`”.
--
-- We don’t mention the word “object” since it should be obvious from context.
--
-- Instead of saying “property `p`” we instead say “`p:`” without the word “property”. We use a
-- colon as a visual cue that this identifier refers to a property name and not a variable name. We
-- believe that this should be a sufficient alternative to saying the word “property”.
--
-- By simplifying in this way we help the error message sound more like a normal sentence and less
-- like a sentence generated by a computer.
diagnosticErrorMessage (MissingProperty propertyRange propertyName stack) =
  ( operationMessage <> plain " because " <> code propertyNameText <> plain " is missing."
  , [ DiagnosticRelatedInformation propertyRange (code propertyNameText)
    ]
  )
  where
    operationMessage = loop stack

    loop (UnifyStackOperation _ operation) = unifyStackOperationMessage operation
    loop (UnifyStackFrame _ _ nestedStack) = loop nestedStack

    propertyNameText = Text.snoc (identifierText propertyName) ':'

-- The `ExtraProperty` error message is similar to the `MissingProperty` error message. The error
-- that’s reported depends on what side of unification an overflown property was on. If our actual
-- object is missing a property then we have a `MissingProperty` error. If our actual object has an
-- extra property then we have an `ExtraProperty` error.
--
-- We need to express that
diagnosticErrorMessage (ExtraProperty propertyRange propertyName stack) =
  ( operationMessage <> plain " because " <> code propertyNameText <> plain " is not needed."
  , [ DiagnosticRelatedInformation propertyRange (code propertyNameText)
    ]
  )
  where
    operationMessage = loop stack

    loop (UnifyStackOperation _ operation) = unifyStackOperationMessage operation
    loop (UnifyStackFrame _ _ nestedStack) = loop nestedStack

    propertyNameText = Text.snoc (identifierText propertyName) ':'

-- Here we attempt to explain to the programmer why we failed their program when encountering these
-- two types. Programmers shouldn’t see this message too often, it usually only comes up in
-- advanced scenarios.
--
-- What happened is that the programmer is trying to use a type without generics in a place that
-- expects a type with generics. A common example is:
--
-- ```ite
-- fun auto(f: fun<T>(T) -> T) { f(f) }
--
-- auto(fun(x: Int) -> Int { x + 1 });
-- ```
--
-- Here, the `auto` function is written to expect a function that is generic. It can take any `T`
-- and return the very same `T`. However, the `auto` function is called with a function that expects
-- an integer and returns an integer.
--
-- We show the programmer the type which is _more_ general first and then the type which is less
-- general. This means the programmer “loads” into their short term memory the more general type and
-- they can easily compare that to the less general type when they see concrete types (like `Int`)
-- instead of generic types.
diagnosticErrorMessage (DoesNotAbstract actualRange expectedRange actual expected stack) =
  -- Construct the error message.
  ( operationMessage <> plain " because " <> code expected <> plain " is more general than " <> code actual <> plain "."

  -- The references which are inside the diagnostic range will be hidden.
  , [ DiagnosticRelatedInformation expectedRange (code expected)
    , DiagnosticRelatedInformation actualRange (code actual)
    ]
  )
  where
    operationMessage = loop stack

    loop (UnifyStackOperation _ operation) = unifyStackOperationMessage operation
    loop (UnifyStackFrame _ _ nestedStack) = loop nestedStack

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
diagnosticErrorMessage (InfiniteType stack) = noRelatedInformation $
  operationMessage <> plain " because the type checker infers an infinite type."
  where
    operationMessage = loop stack

    loop (UnifyStackOperation _ operation) = unifyStackOperationMessage operation
    loop (UnifyStackFrame _ _ nestedStack) = loop nestedStack

-- If a user sees an internal error diagnostic then there’s a bug in Brite. Refer users to the issue
-- tracker so they can report their problem.
diagnosticErrorMessage (InternalError x) = noRelatedInformation $
  plain "Internal Error: " <> internalErrorDiagnosticMessage x <> plain " " <> issueTrackerMessage
  where
    internalErrorDiagnosticMessage (ExpectedTypeVariableToExist name) =
      plain "Expected type variable " <> code (identifierText name) <> plain " to exist in the prefix."

-- Utility for constructing an error message with no related information.
noRelatedInformation :: Markup -> (Markup, [DiagnosticRelatedInformation])
noRelatedInformation message = (message, [])

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

-- Get the message for a unification stack operation.
unifyStackOperationMessage :: UnifyStackOperation -> Markup
unifyStackOperationMessage UnifyTest = plain "Test failed" -- NOTE: We should only see this during testing.

-- We use “Can not” instead of “Cannot” because the former is simpler to read.
unifyStackOperationMessage (UnifyFunctionCall snippet) =
  plain "Can not call " <> code (Text.Builder.toStrictText (printExpressionSnippet snippet))

-- We don’t want to use the word “cast” even though that’s pretty common programming language
-- terminology. Instead the simpler phrasing of “changing a type” will do.
unifyStackOperationMessage (UnifyExpressionAnnotation snippet) =
  plain "Can not change type of " <> code (Text.Builder.toStrictText (printExpressionSnippet snippet))

-- We avoid using the term “condition” or “conditional” as that might be confusing.
unifyStackOperationMessage (UnifyConditionalTest snippet) =
  plain "Can not test " <> code (Text.Builder.toStrictText (printExpressionSnippet snippet))

-- We use an expression snippet for the branch’s test expression and say that we cannot test that
-- expression. The thing we can’t do when conditional branches are different types is we can’t
-- create a conditional expression. After all, we’d have different types. We want to express this
-- without resorting to a message which is too complex.
unifyStackOperationMessage (UnifyConditionalBranches snippet) =
  plain "Can not test " <> code (Text.Builder.toStrictText (printExpressionSnippet snippet))

-- A message for pushing people to our issue tracker when they encounter an unexpected error.
--
-- We use the word “issue” instead of the word “bug” because the meaning of “bug” is subjective. An
-- error might be expected behavior and we just haven’t created a better error message for it yet.
issueTrackerMessage :: Markup
issueTrackerMessage =
  plain "See if this issue was already reported: https://github.com/brite-code/brite/issues"

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
