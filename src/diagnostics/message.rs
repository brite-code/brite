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
    UnexpectedToken { unexpected: Token },
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
            UnexpectedToken { unexpected } => {
                let mut message = Markup::new();
                message.push("Unexpected ");
                match unexpected {
                    Token::Identifier(_) => message.push("variable name"),
                    Token::Number(_) => message.push("number"),
                    Token::End(_) => message.push("ending"),
                    Token::Glyph(token) => {
                        let glyph = token.glyph();
                        match glyph {
                            Glyph::Keyword(keyword) => {
                                message.push("keyword ");
                                message.push_code(keyword.as_str());
                            }
                            // NOTE: This list should only match single character glyphs! Remember
                            // that when adding a new glyph that not all glyphs have to be
                            // single character.
                            Glyph::BraceLeft
                            | Glyph::BraceRight
                            | Glyph::Comma
                            | Glyph::Dot
                            | Glyph::Equals
                            | Glyph::ParenLeft
                            | Glyph::ParenRight
                            | Glyph::Semicolon
                            | Glyph::Slash => {
                                message.push("character ");
                                message.push_code(glyph.as_str());
                            }
                        }
                    }
                    Token::Error(token) => {
                        use crate::source::ErrorTokenDescription;
                        match &token.description {
                            ErrorTokenDescription::UnexpectedChar { unexpected } => {
                                message.push("character ");
                                message.push_code(unexpected.to_string());
                            }
                            ErrorTokenDescription::InvalidNumber { .. } => message.push("number"),
                        }
                    }
                }
                message.push(".");
                message
            }
        }
    }
}
