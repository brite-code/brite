//! Diagnostic messages are our primary way to communicate with the user. We better be really sure
//! our messages are good.
//!
//! In the future, we should consider consulting a copy editor to professionalize our error message
//! style. We may also want to consider running A/B tests.
//!
//! # Style Guide
//!
//! What makes a good error message? These guides are designed to produce the clearest message
//! possible. Follow these guides to create good, consistent, error messages.
//!
//! - Keep diagnostic messages short. Preferably a single, clear, sentence. This format works
//!   best for all our target editors. Consider VSCode which uses a hover dialog for viewing
//!   errors inline and a “problems” panel to view all errors in a project. Short single-line
//!   messages work best in both of these locations.
//!
//! - 80% of the time the fix is obvious to human and computer so provide a direct error message.
//!   20% of the time the error won’t be obvious to the human so provide a message which is _not_
//!   misleading and gives the human enough information to step through their program and find the
//!   fix which might involve making tradeoffs a computer couldn’t understand in their program.
//!   Never give a human a misleading an error message, they’ll spend more time on the message then
//!   on their program.
//!
//! - Trust that the programmer is clever unless shown otherwise. Prefer error messages which are
//!   always short and true to error messages which are long and misleading/false. If the
//!   programmer is clever and you’ve given them enough tools (error messages, extra references, IDE
//!   tools like hover types) the clever programmer should be able to deduce the real problem in
//!   their code. If it is shown the programmer is not clever enough to solve the error on their own
//!   with the given error message then consider giving them a better error message.
//!
//! - Use correct English grammar. It can be hard to make a program which produces correct English
//!   grammar. If you must, consult a spellchecker.
//!
//! - Write messages in first-person plural. That is, use “we”. For example “we see an error”.
//!   This personifies our type checker as a team of people looking for bugs in the programmer’s
//!   code. By personifying our type checker error messages feel like a dialogue. Elm’s error
//!   messages are famous for using first-person. I (Caleb) always found messages like “I found an
//!   error” to be a bit annoying since the type checker is certainly not a person nor is it built
//!   by a single person. Hopefully “we” will be a nice compromise.
//!
//! - When speaking, present tense instead of past tense. Instead of “we found” say “we see”. An
//!   error in the programmer’s code is not a point in time, but rather a state in which the code
//!   will remain until the bug is fixed. While yes, the type checker runs at discrete points in
//!   time we want to give the user the perception that Brite is alive and reacting to their input.
//!   Not spinning in a background thread and then spitting out errors every once in a while.
//!
//! - Use language the programmer will understand. Not language the compiler understands. Words
//!   like “identifier”, “token”, and “expression” are compiler speak. Instead of compiler speak
//!   like “identifier” use a phrase like “variable name”.
//!
//! - Any text that might be written in code should use inline code markup formatting from the
//!   `Markup` object. This should then be rendered as inline code blocks by diagnostic
//!   message renderers.
//!
//! - If you use quotes, make sure they are curly quotes. For instance “phrase” instead
//!   of "phrase". Same for single quotes. For instance ‘phrase’ instead of 'phrase'. Unless you
//!   are talking about quotes inside of code.
//!
//! ## Helpful Tools
//!
//! Some tools we find are helpful when designing on an error message.
//!
//! - [Grammarly](https://www.grammarly.com) for confirming your grammar is correct.
//! - [Hemingway Editor](http://www.hemingwayapp.com) for reducing the complexity of your writing.

use super::syntax::{Position, Range};
use std::rc::Rc;

/// A diagnostic is some message presented to the user about their program. Diagnostics contain a
/// range of characters which the diagnostic points to. Diagnostics are only valid in the scope of
/// some resource since while they contain a range they do not contain the document that range
/// applies to.
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
///
/// Diagnostic messages may not be constructed outside of this module. We always construct and
/// report a diagnostic at the same time as well.
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
    /// Useful information about a user’s program that does not need to be changed. Unlike a warning
    /// where we are recommending a code change.
    #[allow(dead_code)]
    Info(InfoDiagnosticMessage),
}

enum ErrorDiagnosticMessage {
    /// The parser ran into syntax it did not recognize.
    UnexpectedSyntax {
        unexpected: UnexpectedSyntax,
        expected: ExpectedSyntax,
    },
    /// The parser ran into the end of the source document unexpectedly.
    UnexpectedEnding { expected: ExpectedSyntax },
}

enum WarningDiagnosticMessage {}

enum InfoDiagnosticMessage {}

/// Some syntax the Brite parser did not expect.
pub enum UnexpectedSyntax {
    /// An unexpected character.
    Char(char),
}

/// Some syntax the Brite expected but did not receive.
pub enum ExpectedSyntax {
    /// Expected the end of a block comment.
    BlockCommentEnd,
    /// Expected a decimal digit.
    DecimalDigit,
    /// Expected a binary digit.
    BinaryDigit,
    /// Expected a hexadecimal digit.
    HexadecimalDigit,
}

impl Diagnostic {
    fn new(range: Range, message: DiagnosticMessage) -> Self {
        Diagnostic { range, message }
    }

    fn error(range: Range, message: ErrorDiagnosticMessage) -> Self {
        Self::new(range, DiagnosticMessage::Error(message))
    }

    /// The parser ran into syntax it did not recognize.
    pub fn unexpected_syntax(
        range: Range,
        unexpected: UnexpectedSyntax,
        expected: ExpectedSyntax,
    ) -> Self {
        Self::error(
            range,
            ErrorDiagnosticMessage::UnexpectedSyntax {
                unexpected,
                expected,
            },
        )
    }

    /// The parser ran into a character it did not recognize.
    pub fn unexpected_char(position: Position, unexpected: char, expected: ExpectedSyntax) -> Self {
        Self::unexpected_syntax(
            Range::new(position, unexpected.len_utf16() as u32),
            UnexpectedSyntax::Char(unexpected),
            expected,
        )
    }

    /// The parser ran into the end of the source document unexpectedly.
    pub fn unexpected_ending(position: Position, expected: ExpectedSyntax) -> Self {
        Self::error(
            Range::new(position, 0),
            ErrorDiagnosticMessage::UnexpectedEnding { expected },
        )
    }
}

/// A reference to a diagnostic. Can only be created by calling `DiagnosticsContext::report()` so
/// it forces the programmer to report a diagnostic before being able to use a `DiagnosticRef`.
pub struct DiagnosticRef(Rc<Diagnostic>);

/// A collection of diagnostics.
pub struct DiagnosticsContext {
    diagnostics: Vec<Rc<Diagnostic>>,
}

impl DiagnosticsContext {
    /// Creates a new diagnostic context.
    pub fn new() -> Self {
        DiagnosticsContext {
            diagnostics: Vec::new(),
        }
    }

    /// Reports a diagnostic in our diagnostic context.
    pub fn report(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        let diagnostic = Rc::new(diagnostic);
        self.diagnostics.push(Rc::clone(&diagnostic));
        DiagnosticRef(diagnostic)
    }
}
