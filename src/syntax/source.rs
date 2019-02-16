use super::{Document, DocumentChars, Position, Range};
use num::BigInt;
use std::iter;
use unicode_xid::UnicodeXID;

/// A token is a more semantic unit for describing Brite source code documents than a character.
/// Through the tokenization of a document we add meaning by parsing low-level code elements like
/// identifiers, numbers, strings, comments, and glyphs.
///
/// - The leading trivia of a token is all of the trivia which comes before the token which has not
///   already been parsed. Comments, spaces, and newlines.
/// - The trailing trivia of a token is all of the trivia after a token _up until the first new
///   line_. If a token is trailed by a line comment then that is part of the trailing trivia along
///   with the newline which immediately follows but nothing else!
///
/// This heuristic was inspired by the [Swift syntax library][1].
///
/// [1]: https://github.com/apple/swift/tree/e07a8cf2a68ad3c2c97a144369d06d427ba240a7/lib/Syntax#trivia
pub struct Token<'src> {
    range: Range,
    leading_trivia: Vec<Trivia<'src>>,
    trailing_trivia: Vec<Trivia<'src>>,
    kind: TokenKind<'src>,
}

/// The kind of a token.
enum TokenKind<'src> {
    /// A glyph represents some constant sequence of characters that is used in Brite syntax.
    Glyph(Glyph),
    /// A name in the program.
    Identifier(Identifier),
    /// Some number written in the program.
    Number(Number<'src>),
    /// An unexpected character in the program.
    UnexpectedChar(char),
}

impl<'src> Token<'src> {
    /// Add the source code we parsed this token from back to a string.
    fn add_source(&self, source: &mut String) {
        for trivia in &self.leading_trivia {
            trivia.add_source(source);
        }
        match &self.kind {
            TokenKind::Glyph(glyph) => source.push_str(glyph.source()),
            TokenKind::Identifier(identifier) => source.push_str(&identifier.0),
            TokenKind::Number(number) => source.push_str(&number.raw),
            TokenKind::UnexpectedChar(c) => source.push(*c),
        }
        for trivia in &self.trailing_trivia {
            trivia.add_source(source);
        }
    }
}

/// The last token in a document. An end token has the position at which the document ended and all
/// the trivia between the last token and the ending.
struct EndToken<'src> {
    position: Position,
    leading_trivia: Vec<Trivia<'src>>,
}

impl<'src> EndToken<'src> {
    /// Add the source code we parsed this token from back to a string.
    fn add_source(&self, source: &mut String) {
        for trivia in &self.leading_trivia {
            trivia.add_source(source);
        }
    }
}

/// A glyph represents some constant sequence of characters that is used in Brite syntax.
#[derive(Clone, Copy)]
enum Glyph {
    /// A reserved identifier.
    Keyword(Keyword),
    /// `&`
    Ampersand,
    /// `&&`
    AmpersandDouble,
    /// `->`
    Arrow,
    /// `*`
    Asterisk,
    /// `!`
    Bang,
    /// `|`
    Bar,
    /// `||`
    BarDouble,
    /// `{`
    BraceLeft,
    /// `}`
    BraceRight,
    /// `[`
    BracketLeft,
    /// `]`
    BracketRight,
    /// `^`
    Caret,
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `=`
    Equals,
    /// `==`
    EqualsDouble,
    /// `!=`
    EqualsNot,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanOrEqual,
    /// `<`
    LessThan,
    /// `<=`
    LessThanOrEqual,
    /// `-`
    Minus,
    /// `(`
    ParenLeft,
    /// `)`
    ParenRight,
    /// `%`
    Percent,
    /// `+`
    Plus,
    /// `;`
    Semicolon,
    /// `/`
    Slash,
}

impl Glyph {
    /// Get the source string this glyph was parsed from. Always a static string.
    fn source(&self) -> &'static str {
        use self::Glyph::*;
        match self {
            Keyword(keyword) => keyword.source(),
            Ampersand => "&",
            AmpersandDouble => "&&",
            Arrow => "->",
            Asterisk => "*",
            Bang => "!",
            Bar => "|",
            BarDouble => "||",
            BraceLeft => "{",
            BraceRight => "}",
            BracketLeft => "[",
            BracketRight => "]",
            Caret => "^",
            Colon => ":",
            Comma => ",",
            Dot => ".",
            Equals => "=",
            EqualsDouble => "==",
            EqualsNot => "!=",
            GreaterThan => ">",
            GreaterThanOrEqual => ">=",
            LessThan => "<",
            LessThanOrEqual => "<=",
            Minus => "-",
            ParenLeft => "(",
            ParenRight => ")",
            Percent => "%",
            Plus => "+",
            Semicolon => ";",
            Slash => "/",
        }
    }
}

/// A reserved identifier.
#[derive(Clone, Copy)]
enum Keyword {
    /// `_`
    Hole,
    /// `true`
    True,
    /// `false`
    False,
}

impl Keyword {
    /// Converts a string into a keyword if the string is a keyword.
    fn from_str(str: &str) -> Option<Keyword> {
        use self::Keyword::*;
        match str {
            "_" => Some(Hole),
            "true" => Some(True),
            "false" => Some(False),
            _ => None,
        }
    }

    /// Get the source string this keyword was parsed from. Always a static string.
    fn source(&self) -> &'static str {
        use self::Keyword::*;
        match self {
            Hole => "_",
            True => "true",
            False => "false",
        }
    }
}

/// A name written in a Brite program. Brite identifiers follow the [Unicode Identifier
/// Specification][1] including the optional underscore (`_`) character.
///
/// Some strings which are valid identifier syntax are reserved as keywords to disambiguate parsing.
/// We try to reserve the minimum number of keywords possible.
///
/// We could only have keywords in certain positions. For instance, only reserve the keyword `fun`
/// when in an expression context. However, this introduces a potentially confusing rule. It also
/// means, in this example, code transformations could not easily make expression identifiers out of
/// pattern identifiers.
///
/// [1]: http://www.unicode.org/reports/tr31
struct Identifier(
    // TODO: Intern identifier strings.
    String,
);

impl Identifier {
    /// Does this start an identifier?
    fn is_start(c: char) -> bool {
        match c {
            // Optimization: Quickly detect ASCII Latin characters.
            'a'...'z' => true,
            'A'...'Z' => true,
            // Include the optional underscore character.
            '_' => true,
            // Delegate to `UnicodeXID::is_xid_start` for everything else.
            c => UnicodeXID::is_xid_start(c),
        }
    }

    /// Is this a continuation of an identifier?
    fn is_continue(c: char) -> bool {
        match c {
            // Optimization: Quickly detect ASCII Latin characters and numbers.
            'a'...'z' => true,
            'A'...'Z' => true,
            '0'...'9' => true,
            // Include the optional underscore character.
            '_' => true,
            // Delegate to `UnicodeXID::is_xid_continue` for everything else.
            c => UnicodeXID::is_xid_continue(c),
        }
    }
}

/// A number in Brite source code could be written in a few different ways:
///
/// - Integer: `42`
/// - Binary integer: `0b1101`
/// - Hexadecimal integer: `0xFFF`
/// - Floating point: `3.1415`, `1e2`
///
/// For floating point numbers we use the same syntax as the [JSON specification][1] with the one
/// modification that we permit leading zeroes.
///
/// We never parse a negative sign as part of our number syntax. Instead we use a negative operator
/// in our language syntax.
///
/// [1]: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
struct Number<'src> {
    /// The raw string this number was parsed from.
    raw: &'src str,
    /// The kind of number we parsed
    kind: NumberKind,
}

/// The kind of a number.
enum NumberKind {
    /// `42`
    ///
    /// A base 10 integer. We have the raw text representation of the integer in addition to
    /// its value.
    DecimalInteger(BigInt),
    /// `0b1101`
    ///
    /// An integer written in binary form. The boolean is true if the “b” after `0` was lowercase.
    /// Then we have the integer’s raw text form and value.
    BinaryInteger(BigInt),
    /// `0xFFF`
    ///
    /// An integer written in hexadecimal form. The boolean is true if the “x” after `0` was
    /// lowercase. Then we have the integer’s raw text form and value.
    HexadecimalInteger(BigInt),
    /// `3.1415`, `1e2`
    ///
    /// A 64-bit floating point number. We aim for our floating point syntax to be compatible with
    /// the [JSON specification][1].
    ///
    /// [1]: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
    Float(f64),
    /// An invalid number token. We started trying to parse a number, but we encountered an
    /// unexpected character.
    Invalid,
}

/// Pieces of Brite syntax which (usually) don’t affect program behavior. Like comments or spaces.
enum Trivia<'src> {
    /// Contiguous space characters (` `).
    Spaces(usize),
    /// Contiguous tab characters (`\t`). The Brite formatter prefers spaces to tabs, but since tabs
    /// are common enough we’ll add a special case for them in `Trivia`.
    Tabs(usize),
    /// Contiguous newlines. The Brite formatter prefers line feeds (`\n`) to other forms
    /// of newlines.
    Newlines(Newline, usize),
    /// Some comment about the source code.
    Comment(Comment<'src>),
    /// Other whitespace characters which we optimize such as obscure Unicode whitespace
    /// characters like U+00A0.
    OtherWhitespace(char),
}

/// Supported newline sequences.
enum Newline {
    /// `\n`
    LF,
    /// `\r`
    CR,
    /// `\r\n`
    CRLF,
}

enum Comment<'src> {
    /// `// ...` does not include the newline that ends the comment. Does not include the
    /// `//` characters.
    Line(&'src str),
    /// `/* ... */` does include the `/*` and `*/` characters. If the boolean is false then we
    /// reached the end of the file before finding `*/`.
    Block(&'src str, bool),
}

impl<'src> Trivia<'src> {
    /// Add the source code we parsed this trivia from back to a string.
    fn add_source(&self, source: &mut String) {
        match self {
            Trivia::Spaces(n) => source.extend(iter::repeat(' ').take(*n)),
            Trivia::Tabs(n) => source.extend(iter::repeat('\t').take(*n)),
            Trivia::Newlines(Newline::LF, n) => source.extend(iter::repeat('\n').take(*n)),
            Trivia::Newlines(Newline::CR, n) => source.extend(iter::repeat('\r').take(*n)),
            Trivia::Newlines(Newline::CRLF, n) => source.extend(iter::repeat("\r\n").take(*n)),
            Trivia::OtherWhitespace(c) => source.push(*c),

            Trivia::Comment(Comment::Line(comment_source)) => {
                source.push_str("//");
                source.push_str(comment_source)
            }

            Trivia::Comment(Comment::Block(comment_source, ended)) => {
                source.push_str("/*");
                source.push_str(comment_source);
                if *ended {
                    source.push_str("*/");
                }
            }
        }
    }
}

/// A lexer generates `Token`s based on a `Document` input. Call `Lexer::next()` to advance the
/// lexer and `Lexer::end()` to get the end token. All of the `Token`s and `EndToken` may be used
/// to print back out a string which is equivalent to the `Document`’s source.
struct Lexer<'src> {
    chars: DocumentChars<'src>,
    end: Option<EndToken<'src>>,
}

impl<'src> Lexer<'src> {
    /// Creates a new lexer from a source code `Document`.
    fn new(document: &'src Document) -> Lexer<'src> {
        Lexer {
            chars: document.chars(),
            end: None,
        }
    }

    /// Once we are done generating tokens with our lexer we can call `end()` which consumes the
    /// lexer and optionally returns an `EndToken`.
    fn end(self) -> Option<EndToken<'src>> {
        self.end
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;

    /// Advances the lexer and returns the next token. We implement this with the `Iterator`
    /// interface to get some handy methods like `Iterator::collect()`.
    fn next(&mut self) -> Option<Token<'src>> {
        // If we have ended then keep returning `None`.
        if self.end.is_some() {
            return None;
        }

        let leading_trivia = self.next_trivia(true);
        let start = self.chars.position();

        let kind = match self.chars.next() {
            // Single character glyphs
            Some('*') => TokenKind::Glyph(Glyph::Asterisk),
            Some('{') => TokenKind::Glyph(Glyph::BraceLeft),
            Some('}') => TokenKind::Glyph(Glyph::BraceRight),
            Some('[') => TokenKind::Glyph(Glyph::BracketLeft),
            Some(']') => TokenKind::Glyph(Glyph::BracketRight),
            Some('^') => TokenKind::Glyph(Glyph::Caret),
            Some(':') => TokenKind::Glyph(Glyph::Colon),
            Some(',') => TokenKind::Glyph(Glyph::Comma),
            Some('(') => TokenKind::Glyph(Glyph::ParenLeft),
            Some(')') => TokenKind::Glyph(Glyph::ParenRight),
            Some('%') => TokenKind::Glyph(Glyph::Percent),
            Some('+') => TokenKind::Glyph(Glyph::Plus),
            Some(';') => TokenKind::Glyph(Glyph::Semicolon),
            Some('/') => TokenKind::Glyph(Glyph::Slash),

            // Multiple character glyphs
            Some('&') => match self.chars.peek() {
                Some('&') => {
                    self.chars.next();
                    TokenKind::Glyph(Glyph::AmpersandDouble)
                }
                _ => TokenKind::Glyph(Glyph::Ampersand),
            },
            Some('|') => match self.chars.peek() {
                Some('|') => {
                    self.chars.next();
                    TokenKind::Glyph(Glyph::BarDouble)
                }
                _ => TokenKind::Glyph(Glyph::Bar),
            },
            Some('.') => TokenKind::Glyph(Glyph::Dot), // TODO: Numbers
            Some('=') => match self.chars.peek() {
                Some('=') => {
                    self.chars.next();
                    TokenKind::Glyph(Glyph::EqualsDouble)
                }
                _ => TokenKind::Glyph(Glyph::Equals),
            },
            Some('!') => match self.chars.peek() {
                Some('=') => {
                    self.chars.next();
                    TokenKind::Glyph(Glyph::EqualsNot)
                }
                _ => TokenKind::Glyph(Glyph::Bang),
            },
            Some('>') => match self.chars.peek() {
                Some('=') => {
                    self.chars.next();
                    TokenKind::Glyph(Glyph::GreaterThanOrEqual)
                }
                _ => TokenKind::Glyph(Glyph::GreaterThan),
            },
            Some('<') => match self.chars.peek() {
                Some('=') => {
                    self.chars.next();
                    TokenKind::Glyph(Glyph::LessThanOrEqual)
                }
                _ => TokenKind::Glyph(Glyph::LessThan),
            },
            Some('-') => match self.chars.peek() {
                Some('>') => {
                    self.chars.next();
                    TokenKind::Glyph(Glyph::Arrow)
                }
                _ => TokenKind::Glyph(Glyph::Minus),
            },

            // Identifier
            Some(c) if Identifier::is_start(c) => {
                let mut identifier = String::new();
                identifier.push(c);
                self.chars.next();
                loop {
                    match self.chars.peek() {
                        Some(c) if Identifier::is_continue(c) => {
                            identifier.push(c);
                            self.chars.next();
                        }
                        _ => break,
                    }
                }
                if let Some(keyword) = Keyword::from_str(&identifier) {
                    TokenKind::Glyph(Glyph::Keyword(keyword))
                } else {
                    identifier.shrink_to_fit();
                    TokenKind::Identifier(Identifier(identifier))
                }
            }

            _ => unimplemented!(),
        };

        let end = self.chars.position();
        let trailing_trivia = self.next_trivia(false);

        let range = Range::new(start, end);

        // Return the token we just parsed.
        Some(Token {
            range,
            leading_trivia,
            trailing_trivia,
            kind,
        })
    }
}

impl<'src> Lexer<'src> {
    /// Parses some token trivia. If `leading` is true then we are parsing leading trivia. Otherwise
    /// we are parsing trailing trivia.
    fn next_trivia(&mut self, leading: bool) -> Vec<Trivia<'src>> {
        let mut trivia = Vec::new();

        loop {
            match self.chars.peek() {
                // Spaces
                Some(' ') => {
                    self.chars.next();
                    let mut n = 1;
                    while let Some(' ') = self.chars.peek() {
                        self.chars.next();
                        n += 1;
                    }
                    trivia.push(Trivia::Spaces(n));
                }

                // Tabs
                Some('\t') => {
                    self.chars.next();
                    let mut n = 1;
                    while let Some('\t') = self.chars.peek() {
                        self.chars.next();
                        n += 1;
                    }
                    trivia.push(Trivia::Tabs(n));
                }

                // Newlines (LF)
                Some('\n') => {
                    self.chars.next();
                    let mut n = 1;

                    // After we see one newline in trailing trivia, stop parsing trivia!
                    if !leading {
                        trivia.push(Trivia::Newlines(Newline::LF, n));
                        break;
                    }

                    while let Some('\n') = self.chars.peek() {
                        self.chars.next();
                        n += 1;
                    }
                    trivia.push(Trivia::Newlines(Newline::LF, n));
                }

                // Newlines (CR and CRLF)
                Some('\r') => {
                    self.chars.next();
                    let mut n = 1;
                    if let Some('\n') = self.chars.peek() {
                        self.chars.next();

                        // After we see one newline in trailing trivia, stop parsing trivia!
                        if !leading {
                            trivia.push(Trivia::Newlines(Newline::CRLF, n));
                            break;
                        }

                        while self.chars.peek() == Some('\r') && self.chars.peek2() == Some('\n') {
                            self.chars.next();
                            self.chars.next();
                            n += 1;
                        }
                        trivia.push(Trivia::Newlines(Newline::CRLF, n));
                    } else {
                        // After we see one newline in trailing trivia, stop parsing trivia!
                        if !leading {
                            trivia.push(Trivia::Newlines(Newline::CR, n));
                            break;
                        }

                        while let Some('\r') = self.chars.peek() {
                            self.chars.next();
                            n += 1;
                        }
                        trivia.push(Trivia::Newlines(Newline::CR, n));
                    }
                }

                // Comments
                Some('/') => match self.chars.peek2() {
                    // Line comments
                    Some('/') => {
                        self.chars.next();
                        self.chars.next();
                        let comment = self.chars.span(|c| c != '\n' && c != '\r');
                        trivia.push(Trivia::Comment(Comment::Line(comment)));
                    }

                    // Block comments
                    Some('*') => {
                        self.chars.next();
                        self.chars.next();
                        let mut asterisk = false;
                        let mut newline = false;
                        let comment = self.chars.span(|c| {
                            if c == '*' {
                                asterisk = true;
                                true
                            } else if c == '/' && asterisk {
                                false
                            } else {
                                if asterisk {
                                    asterisk = false;
                                }
                                if c == '\n' || c == '\r' {
                                    newline = true;
                                }
                                true
                            }
                        });
                        if let Some('/') = self.chars.peek() {
                            self.chars.next();
                            let comment = &comment[0..(comment.len() - 1)];
                            trivia.push(Trivia::Comment(Comment::Block(comment, true)));
                        } else {
                            trivia.push(Trivia::Comment(Comment::Block(comment, false)));
                        }
                        // If we are parsing trailing trivia and there was a newline in the block
                        // comment then stop parsing trivia.
                        if !leading && newline {
                            break;
                        }
                    }

                    _ => break,
                },

                // Other whitespace
                Some(c) if c.is_whitespace() => {
                    self.chars.next();
                    trivia.push(Trivia::OtherWhitespace(c));
                }

                // If there is no more trivia then we may stop the loop.
                _ => break,
            }
        }

        trivia
    }
}
