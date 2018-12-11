module Brite.Diagnostics
  ( Diagnostic
  , DiagnosticMonad
  , DiagnosticWriter
  , ExpectedToken(..)
  , unexpectedToken
  , unexpectedEnding
  ) where

import Brite.Source

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
  , diagnosticMessage :: DiagnosticMessage
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
  = UnexpectedToken Token ExpectedToken
  -- The parser ran into the end of the source document unexpectedly.
  | UnexpectedEnding ExpectedToken

data WarningDiagnosticMessage

data InfoDiagnosticMessage

-- A diagnostic monad will allow us to report diagnostics. The diagnostics we report will go into
-- a list where they may be read from later.
class Monad m => DiagnosticMonad m where
  report :: Diagnostic -> m Diagnostic

-- A simple diagnostic writer monad which writes diagnostics to a list.
data DiagnosticWriter a = DiagnosticWriter a [Diagnostic]

instance Functor DiagnosticWriter where
  fmap f (DiagnosticWriter a ds) = DiagnosticWriter (f a) ds

instance Applicative DiagnosticWriter where
  pure a = DiagnosticWriter a []
  (DiagnosticWriter f x) <*> (DiagnosticWriter a y) = DiagnosticWriter (f a) (x ++ y)

instance Monad DiagnosticWriter where
  (DiagnosticWriter a x) >>= f =
    let
      (DiagnosticWriter b y) = f a
    in
      DiagnosticWriter b (x ++ y)

instance DiagnosticMonad DiagnosticWriter where
  report d = DiagnosticWriter d [d]

-- Runs a `DiagnosticWriter` monad.
runDiagnosticWriter :: DiagnosticWriter a -> (a, [Diagnostic])
runDiagnosticWriter (DiagnosticWriter a ds) = (a, ds)

-- What token did we expect?
data ExpectedToken
  = ExpectedGlyph Glyph
  | ExpectedIdentifier

-- The parser ran into a token it did not recognize.
unexpectedToken :: DiagnosticMonad m => Range -> Token -> ExpectedToken -> m Diagnostic
unexpectedToken range unexpected expected = report $ Diagnostic range $ Error $
  UnexpectedToken unexpected expected

-- The parser ran into the end of the source document unexpectedly.
unexpectedEnding :: DiagnosticMonad m => Range -> ExpectedToken -> m Diagnostic
unexpectedEnding range expected = report $ Diagnostic range $ Error $
  UnexpectedEnding expected
