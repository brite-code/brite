//! A simple markup string builder to be used when rendering messages. This is not intended to be
//! a full markdown parsing and printing framework. Just enough for diagnostic messages.

pub struct Markup {
    segments: Vec<Segment>,
}

/// An item in our markup string.
enum Segment {
    /// Plain text.
    Plain(String),
    /// Inline code.
    Code(String),
}

impl Markup {
    /// Creates a new markup object.
    pub fn new() -> Self {
        Markup {
            segments: Vec::new(),
        }
    }

    /// Pushes a plain text string to our markup.
    ///
    /// As an optimization we might append the string to an internal buffer which is why we take
    /// `&str` instead of `String`.
    pub fn push(&mut self, text: &str) {
        // As an optimization if our last item is a plain text item then push the string to there.
        match self.segments.last_mut() {
            Some(Segment::Plain(current_text)) => current_text.push_str(text),
            _ => self.segments.push(Segment::Plain(text.into())),
        }
    }

    /// Pushes an inline code string to our markup. This will always create a new inline code
    /// segment. Adjacent code segments will not be merged! Unlike plain text segments where there
    /// is no difference between adjacent segments.
    pub fn push_code(&mut self, code: impl Into<String>) {
        self.segments.push(Segment::Code(code.into()))
    }
}
