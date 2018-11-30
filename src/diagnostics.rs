use crate::source::{Range, Token};
use std::ops::Deref;
use std::rc::Rc;

/// A diagnostic is some message presented to the user about their program. Diagnostics contain a
/// range of characters which the diagnostic points to. However, a diagnostic does not contain the
/// resource name being pointed to.
///
/// Our diagnostic format is based on the [Language Server Protocol][1].
///
/// [1]: https://microsoft.github.io/language-server-protocol/specification
#[derive(Debug, PartialEq)]
pub struct Diagnostic {
    range: Range,
    message: DiagnosticMessage,
}

/// The diagnostic message. Includes the severity of the message. Each diagnostic may have some
/// related information.
#[derive(Debug, PartialEq)]
enum DiagnosticMessage {
    /// Error diagnostics must be resolved by the programmer. Error diagnostics will prevent the
    /// program from being deployed. However, the program may still run in development, but
    /// executing a program with errors will result in Undefined Behavior.
    Error(ErrorDiagnosticMessage),
    /// Warning diagnostics may optionally be resolved by the programmer. They exist to highlight
    /// code which is technically correct but might be suboptimal. Warnings will not block
    /// deployment of a program by default. However, it is strongly recommended that warnings
    /// be fixed.
    #[allow(dead_code)]
    Warning(WarningDiagnosticMessage),
    /// Useful information about a user’s program that does not need to be fixed like a warning.
    #[allow(dead_code)]
    Info(InfoDiagnosticMessage),
}

#[derive(Debug, PartialEq)]
enum ErrorDiagnosticMessage {
    /// The lexer ran into a character it did not recognize.
    UnexpectedChar { unexpected: char },
    /// The lexer tried to parse a number, but that number was in an invalid format.
    InvalidNumber { invalid: String },
    /// The parser ran into a token it did not recognize.
    UnexpectedToken { token: Token },
}

#[derive(Debug, PartialEq)]
enum WarningDiagnosticMessage {}

#[derive(Debug, PartialEq)]
enum InfoDiagnosticMessage {}

impl Diagnostic {
    fn new(range: Range, message: DiagnosticMessage) -> Self {
        Diagnostic { range, message }
    }

    fn error(range: Range, message: ErrorDiagnosticMessage) -> Self {
        Self::new(range, DiagnosticMessage::Error(message))
    }

    pub fn unexpected_char(range: Range, unexpected: char) -> Self {
        let message = ErrorDiagnosticMessage::UnexpectedChar { unexpected };
        Self::error(range, message)
    }

    pub fn invalid_number(range: Range, invalid: String) -> Self {
        let message = ErrorDiagnosticMessage::InvalidNumber { invalid };
        Self::error(range, message)
    }

    pub fn unexpected_token(range: Range, token: Token) -> Self {
        let message = ErrorDiagnosticMessage::UnexpectedToken { token };
        Self::error(range, message)
    }
}

/// A set of some diagnostics all associated with the same resource.
pub struct DiagnosticSet {
    diagnostics: Vec<DiagnosticRef>,
}

impl DiagnosticSet {
    /// Creates a new diagnostics collection.
    pub fn new() -> Self {
        let diagnostics = Vec::new();
        DiagnosticSet { diagnostics }
    }

    /// Adds a diagnostic to the set and return a reference to that diagnostic.
    pub fn report(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        let diagnostic = DiagnosticRef(Rc::new(diagnostic));
        self.diagnostics.push(diagnostic.clone());
        diagnostic
    }
}

/// A reference to a diagnostic. Since this struct is only a reference, cloning is relatively cheap.
/// May only be created by `DiagnosticSet`.
#[derive(Clone, Debug, PartialEq)]
pub struct DiagnosticRef(Rc<Diagnostic>);

impl Deref for DiagnosticRef {
    type Target = Diagnostic;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
