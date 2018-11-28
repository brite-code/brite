use crate::source::Range;

/// A diagnostic is some message presented to the user about their program. Diagnostics contain a
/// range of characters which the diagnostic points to. However, a diagnostic does not contain the
/// resource name being pointed to.
///
/// Our diagnostic format is based on the [Language Server Protocol][1].
///
/// [1]: https://microsoft.github.io/language-server-protocol/specification
pub struct Diagnostic {
    range: Range,
    message: DiagnosticMessage,
}

/// The diagnostic message. Includes the severity of the message. Each diagnostic may have some
/// related information.
enum DiagnosticMessage {
    /// Error diagnostics must be resolved by the programmer. Error diagnostics will prevent the
    /// program from being deployed. However, the program may still run in development, but
    /// executing a program with errors will result in Undefined Behavior.
    Error(ErrorDiagnosticMessage),
    /// Warning diagnostics may optionally be resolved by the programmer. They exist to highlight
    /// code which is technically correct but might be suboptimal. Warnings will not block
    /// deployment of a program by default. However, it is strongly recommended that warnings
    /// be fixed.
    Warning(WarningDiagnosticMessage),
    /// Useful information about a user’s program that does not need to be fixed like a warning.
    Info(InfoDiagnosticMessage),
}

enum ErrorDiagnosticMessage {
    /// The lexer ran into a string of characters it did not recognize.
    LexerUnexpectedChar { unexpected: char },
    /// The lexer tried to parse a number, but that number was in an invalid format.
    LexerInvalidNumber { invalid: String },
}

enum WarningDiagnosticMessage {}

enum InfoDiagnosticMessage {}

impl Diagnostic {
    fn new(range: Range, message: DiagnosticMessage) -> Self {
        Diagnostic { range, message }
    }

    fn error(range: Range, message: ErrorDiagnosticMessage) -> Self {
        Self::new(range, DiagnosticMessage::Error(message))
    }

    pub fn lexer_unexpected_char(range: Range, unexpected: char) -> Self {
        let message = ErrorDiagnosticMessage::LexerUnexpectedChar { unexpected };
        Self::error(range, message)
    }

    pub fn lexer_invalid_number(range: Range, invalid: String) -> Self {
        let message = ErrorDiagnosticMessage::LexerInvalidNumber { invalid };
        Self::error(range, message)
    }
}
