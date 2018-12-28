mod markup;
mod message;

use self::markup::Markup;
use self::message::{DiagnosticMessage, ErrorDiagnosticMessage};
use crate::source::{Document, Range, Token};
use std::fmt;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

pub use self::message::ParserExpected;

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

impl Diagnostic {
    fn new(range: Range, message: DiagnosticMessage) -> Self {
        Diagnostic { range, message }
    }

    fn error(range: Range, message: ErrorDiagnosticMessage) -> Self {
        Self::new(range, DiagnosticMessage::Error(message))
    }

    pub fn unexpected_token(range: Range, unexpected: Token, expected: ParserExpected) -> Self {
        Self::error(
            range,
            ErrorDiagnosticMessage::UnexpectedToken {
                unexpected,
                expected,
            },
        )
    }

    pub fn invalid_number(range: Range, raw: String) -> Self {
        Self::error(range, ErrorDiagnosticMessage::InvalidNumber { raw })
    }

    /// Returns the range of this diagnostic.
    pub fn range(&self) -> Range {
        self.range
    }

    /// Creates a diagnostic message to be displayed to the user. Diagnostic messages are built
    /// using `Markup` objects for some light formatting.
    pub fn message(&self) -> Markup {
        self.message.message()
    }

    /// Formats the diagnostic as a simple string. Usable for quickly showing an error to the user.
    /// Requires the document this diagnostic was created with to properly format the
    /// diagnostic’s range. Also accepts an optional path that we should print the document path
    /// relative to.
    pub fn to_simple_string(&self, document: &Document, relative_to: Option<&Path>) -> String {
        let path = document.path();
        let path = relative_to
            .and_then(|current_dir| path.strip_prefix(current_dir).ok())
            .unwrap_or(path);
        format!(
            "{}({},{}): {}",
            path.to_string_lossy(),
            self.range.start().line(&document) + 1,
            self.range.start().character(&document) + 1,
            self.message.message().to_simple_string()
        )
    }
}

/// A set of some diagnostics all associated with the same resource.
#[derive(Debug)]
pub struct DiagnosticSet {
    diagnostics: Vec<DiagnosticRef>,
}

impl DiagnosticSet {
    /// Creates a new diagnostics collection.
    pub fn new() -> Self {
        let diagnostics = Vec::new();
        DiagnosticSet { diagnostics }
    }

    /// Iterates over each diagnostic in the set.
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter().map(Deref::deref)
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
#[derive(Clone, PartialEq)]
pub struct DiagnosticRef(Rc<Diagnostic>);

impl Deref for DiagnosticRef {
    type Target = Diagnostic;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Debug for DiagnosticRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
