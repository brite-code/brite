use crate::source::Range;

/// A diagnostic is some message presented to the user about their program. Our diagnostic format is
/// based on the [Language Server Protocol][1].
///
/// [1]: https://microsoft.github.io/language-server-protocol/specification
struct Diagnostic {
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

/// A list of diagnostics for some resource.
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

/// An identifier for a diagnostic. Used to locate a diagnostic in a `Diagnostics` list.
pub struct DiagnosticID {
    index: usize,
}

impl Diagnostics {
    fn add(&mut self, range: Range, message: DiagnosticMessage) -> DiagnosticID {
        let diagnostic = Diagnostic { range, message };
        let index = self.diagnostics.len();
        self.diagnostics.push(diagnostic);
        DiagnosticID { index }
    }

    fn error(&mut self, range: Range, message: ErrorDiagnosticMessage) -> DiagnosticID {
        self.add(range, DiagnosticMessage::Error(message))
    }

    pub fn lexer_unexpected_char(&mut self, range: Range, unexpected: char) -> DiagnosticID {
        let message = ErrorDiagnosticMessage::LexerUnexpectedChar { unexpected };
        self.error(range, message)
    }

    pub fn lexer_invalid_number(&mut self, range: Range, invalid: String) -> DiagnosticID {
        let message = ErrorDiagnosticMessage::LexerInvalidNumber { invalid };
        self.error(range, message)
    }
}
