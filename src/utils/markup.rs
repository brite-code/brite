//! A simple markup string builder to be used when rendering diagnostic messages. This is not
//! intended to be a full markdown parsing and printing framework. Just enough for
//! diagnostic messages.

use std::fmt;

/// A structured markup document that implements `fmt::Write`, so write away!
pub struct Markup {
    text: String,
}

impl Markup {
    /// Creates a new, empty, markup document.
    pub fn new() -> Self {
        Markup {
            text: String::new(),
        }
    }

    /// Create a writer for inline code markup. When `MarkupCode` drops we finish the inline
    /// code segment.
    pub fn code(&mut self) -> MarkupCode {
        MarkupCode::new(self)
    }

    /// Converts markup to a simple string format. This format is not intended to be parsed by any
    /// markup processor. We only escape backticks (`\``) since they are used for inline code.
    ///
    /// Don’t use this function to create markdown! Markdown requires more escapes. Use this
    /// function when you intend to display the markup to the user without any formatting.
    pub fn to_simple_string(self) -> String {
        self.text
    }
}

impl fmt::Write for Markup {
    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        // Reserve enough length for the string we’re about to push.
        self.text.reserve(s.len());

        // Add all the characters to our markup text.
        for c in s.chars() {
            match c {
                // We use backslashes to escape special characters, like backticks.
                '\\' => self.text.push_str("\\\\"),

                // We use backticks to denote inline code segments.
                '`' => self.text.push_str("\\`"),

                _ => self.text.push(c),
            }
        }

        Ok(())
    }
}

/// A writer for inline code in a `Markup` object. When dropped we finish the inline code segment.
pub struct MarkupCode<'a> {
    markup: &'a mut Markup,
}

impl<'a> MarkupCode<'a> {
    fn new(markup: &mut Markup) -> MarkupCode {
        markup.text.push('`');
        MarkupCode { markup }
    }
}

impl<'a> Drop for MarkupCode<'a> {
    fn drop(&mut self) {
        self.markup.text.push('`');
    }
}

impl<'a> fmt::Write for MarkupCode<'a> {
    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        self.markup.write_str(s)
    }
}
