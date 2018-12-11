module Brite.Diagnostics
  ( Diagnostic
  , ExpectedToken(..)
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

data WarningDiagnosticMessage

data InfoDiagnosticMessage

-- What token did we expect?
data ExpectedToken
  = ExpectedGlyph Glyph
  | ExpectedIdentifier

-- The parser ran into a token it did not recognize.
unexpectedToken :: Range -> Token -> ExpectedToken -> Diagnostic
unexpectedToken range unexpected expected =
  Diagnostic range (Error (UnexpectedToken unexpected expected))
