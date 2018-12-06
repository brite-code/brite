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
//! - Use correct English grammar. It can be hard to make a program which produces correct English
//!   grammar. If you must, consult a spellchecker.
//!
//! - Write messages in first-person plural. That is, use “we”. For example “we found an error”.
//!   This personifies our type checker as a team of people looking for bugs in the programmer’s
//!   code. By personifying our type checker error messages feel like a dialogue. Elm’s error
//!   messages are famous for using first-person tense. I (Caleb) always found messages like “I
//!   found an error” to be a bit annoying since the type checker is certainly not a person nor is
//!   it built by a single person. Hopefully “we” will be a nice compromise.
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

use super::markup::Markup;
use crate::source::{Glyph, Token};

/// The diagnostic message. Includes the severity of the message. Each diagnostic may have some
/// related information.
#[derive(Debug, PartialEq)]
pub enum DiagnosticMessage {
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
pub enum ErrorDiagnosticMessage {
    /// The parser ran into a token it did not recognize.
    UnexpectedToken {
        unexpected: Token,
        expected: ParserExpected,
    },
}

/// What did the parser expect when it encountered an unexpected token?
#[derive(Clone, Debug, PartialEq)]
pub enum ParserExpected {
    Glyph(Glyph),
    Identifier,
    Number,
    Item,
    Expression,
    Pattern,
}

#[derive(Debug, PartialEq)]
pub enum WarningDiagnosticMessage {}

#[derive(Debug, PartialEq)]
pub enum InfoDiagnosticMessage {}

impl DiagnosticMessage {
    /// Creates a diagnostic message to be displayed to the user. Diagnostic messages are built
    /// using `Markup` objects for some light formatting.
    pub fn message(&self) -> Markup {
        match self {
            DiagnosticMessage::Error(message) => Self::error_message(message),
            DiagnosticMessage::Warning(_) => unreachable!(),
            DiagnosticMessage::Info(_) => unreachable!(),
        }
    }

    /// Creates an error message to be displayed to the user
    fn error_message(message: &ErrorDiagnosticMessage) -> Markup {
        use self::ErrorDiagnosticMessage::*;
        match message {
            // Thought and care that went into this error message:
            //
            // - When designing this message we started with “Unexpected character `%`. Expected
            //   expression.” and ended with the message “We found `%` when we wanted an
            //   expression.” The latter uses smaller words. It isn’t abrupt. It personifies the
            //   type checker with “we”.
            // - Instead of “We found a `%` character” we print the message as “We found `%`”. The
            //   latter is shorter. It is also very hard to choose correctly between “a” and “an”
            //   for arbitrary user input. For example this is wrong “a `=` character” since `=` is
            //   pronounced “equals” which starts with a vowel sound. It should be “an `=`
            //   character”. We are unaware of a way to correctly guess the pronunciation people use
            //   for glyphs in general.
            // - For unexpected endings we say “We found the file’s end”. We reference “the file”
            //   and use a curly quote (`’`) for the possessive.
            // - For unexpected tokens when we expected a pattern we say “We found `=` when we
            //   wanted a variable name.” because the word “pattern” is compiler speak. Even though
            //   patterns can be more than a variable name, 80% of the time the programmer will
            //   write a variable name.
            UnexpectedToken {
                unexpected,
                expected,
            } => {
                let mut message = Markup::new();
                message.push("We found ");
                match unexpected {
                    Token::Glyph(token) => message.push_code(token.glyph().as_str()),
                    Token::Identifier(_) => message.push("a variable name"),
                    Token::Number(_) => message.push("a number"),
                    Token::End(_) => message.push("the file’s end"),
                    Token::Error(token) => {
                        use crate::source::ErrorTokenDescription;
                        match &token.description {
                            ErrorTokenDescription::UnexpectedChar { unexpected } => {
                                message.push_code(unexpected.to_string())
                            }
                            ErrorTokenDescription::InvalidNumber { .. } => message.push("a number"),
                        }
                    }
                }
                message.push(" when we wanted ");
                match expected {
                    ParserExpected::Glyph(glyph) => message.push_code(glyph.as_str()),
                    ParserExpected::Identifier => message.push("a variable name"),
                    ParserExpected::Number => message.push("a number"),
                    // NOTE: The words “statement”, “declaration”, “expression”, and “pattern” are
                    // compiler speak. However, at least “statement” and “expression” are relatively
                    // common in C-like languages, so we use those words. But we use “variable name”
                    // for “pattern”. Even though patterns can be more than a variable name.
                    ParserExpected::Item => message.push("a statement"),
                    ParserExpected::Expression => message.push("an expression"),
                    ParserExpected::Pattern => message.push("a variable name"),
                }
                message.push(".");
                message
            }
        }
    }
}
