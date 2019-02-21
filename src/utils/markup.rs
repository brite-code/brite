//! A simple markup string builder to be used when rendering diagnostic messages. This is not
//! intended to be a full markdown parsing and printing framework. Just enough for
//! diagnostic messages.

/// A structured markup document.
pub struct Markup {
    segments: Vec<Segment>,
}

/// An item in our markup document.
enum Segment {
    /// Plain text.
    Plain(String),
    /// Inline code.
    Code(String),
}

impl Markup {
    /// Creates a new, empty, markup document.
    pub fn new() -> Self {
        Markup {
            segments: Vec::new(),
        }
    }

    /// Creates a new markdown document that’s just the provided inline code.
    pub fn code(code: String) -> Self {
        Markup {
            segments: vec![Segment::Code(code)],
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

    /// Converts markup to a simple string format. This format is not intended to be parsed by any
    /// markup processor. We only escape backticks (`\``) since they are used for inline code.
    ///
    /// Don’t use this function to create markdown! Markdown requires more escapes. Use this
    /// function when you intend to display the markup to the user without any formatting.
    pub fn to_simple_string(&self) -> String {
        // Compute a rough estimate for our string’s capacity based on the segments. We will only
        // exceed that capacity if there are backticks which need to be escaped.
        let mut s = String::with_capacity(
            self.segments
                .iter()
                .map(|segment| match segment {
                    Segment::Plain(s) => s.len(),
                    Segment::Code(s) => s.len() + 2,
                }).sum(),
        );
        // Push every character in every segment to our final string. Surround code segments in
        // backticks (`\``) and escape all other backticks.
        for segment in &self.segments {
            match segment {
                Segment::Plain(cs) => {
                    for c in cs.chars() {
                        match c {
                            '`' => s.push_str("\\`"),
                            _ => s.push(c),
                        }
                    }
                }
                Segment::Code(cs) => {
                    s.push('`');
                    for c in cs.chars() {
                        match c {
                            '`' => s.push_str("\\`"),
                            _ => s.push(c),
                        }
                    }
                    s.push('`');
                }
            }
        }
        s
    }
}
