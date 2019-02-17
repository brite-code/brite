use super::document::{Document, DocumentChars, Position, Range};
use crate::ui::{Diagnostic, DiagnosticRef, DiagnosticsCollection, ExpectedSyntax};
use num::BigInt;
use std::cmp;
use std::f64;
use std::iter;
use std::str::FromStr;
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
    kind: TokenKind,
}

/// The kind of a token.
enum TokenKind {
    /// A glyph represents some constant sequence of characters that is used in Brite syntax.
    Glyph(Glyph),
    /// A name in the program.
    Identifier(Identifier),
    /// Some number written in the program.
    Number(Number),
    /// An unexpected character in the program.
    UnexpectedChar(char),
}

impl<'src> Token<'src> {
    /// Re-construct the source code that a list of tokens was parsed from. Every token contains
    /// all the necessary information to print back out the source code it was parsed from. We can
    /// use this function to verify that this behavior.
    pub fn source(tokens: &Vec<Token<'src>>, end_token: &EndToken<'src>) -> String {
        let mut source = String::new();
        for token in tokens {
            token.add_source(&mut source);
        }
        end_token.add_source(&mut source);
        source
    }

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
pub struct EndToken<'src> {
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
pub struct Identifier(
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
struct Number {
    /// The raw string this number was parsed from.
    raw: String,
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
    Invalid(DiagnosticRef),
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
pub struct Lexer<'src> {
    diagnostics: DiagnosticsCollection,
    chars: DocumentChars<'src>,
    end: Option<EndToken<'src>>,
}

impl<'src> Lexer<'src> {
    /// Creates a new lexer from a source code `Document`.
    pub fn new(diagnostics: DiagnosticsCollection, document: &'src Document) -> Lexer<'src> {
        Lexer {
            diagnostics,
            chars: document.chars(),
            end: None,
        }
    }

    /// Once we are done generating tokens with our lexer we can call `end()` which consumes the
    /// lexer and returns the `DiagnosticsContext` and an `EndToken`.
    pub fn end(self) -> (DiagnosticsCollection, Option<EndToken<'src>>) {
        (self.diagnostics, self.end)
    }

    /// Report a diagnostic using the `DiagnosticCollection` owned by this lexer.
    pub fn report_diagnostic(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        self.diagnostics.report(diagnostic)
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

            // Number
            Some(c) if c.is_digit(10) => {
                let mut raw = String::new();
                raw.push(c);

                let (expected, kind) = match (c, self.chars.peek()) {
                    // Binary integer
                    ('0', Some('b')) | ('0', Some('B')) => {
                        raw.push(self.chars.next().unwrap());
                        let mut value = BigInt::from(0);
                        loop {
                            match self.chars.peek() {
                                Some('0') => value = value * 2 + 0,
                                Some('1') => value = value * 2 + 1,
                                _ => break,
                            }
                            raw.push(self.chars.next().unwrap());
                        }
                        let kind = if raw.len() == 2 {
                            // If we did not get any digits then report an error.
                            let diagnostic = self.unexpected_peek(ExpectedSyntax::BinaryDigit);
                            NumberKind::Invalid(diagnostic)
                        } else {
                            NumberKind::BinaryInteger(value)
                        };
                        (ExpectedSyntax::BinaryDigit, kind)
                    }

                    // Hexadecimal integer
                    ('0', Some('x')) | ('0', Some('X')) => {
                        raw.push(self.chars.next().unwrap());
                        let mut value = BigInt::from(0);
                        loop {
                            match self.chars.peek() {
                                Some('0') => value = value * 16 + 0,
                                Some('1') => value = value * 16 + 1,
                                Some('2') => value = value * 16 + 2,
                                Some('3') => value = value * 16 + 3,
                                Some('4') => value = value * 16 + 4,
                                Some('5') => value = value * 16 + 5,
                                Some('6') => value = value * 16 + 6,
                                Some('7') => value = value * 16 + 7,
                                Some('8') => value = value * 16 + 8,
                                Some('9') => value = value * 16 + 9,
                                Some('a') | Some('A') => value = value * 16 + 10,
                                Some('b') | Some('B') => value = value * 16 + 11,
                                Some('c') | Some('C') => value = value * 16 + 12,
                                Some('d') | Some('D') => value = value * 16 + 13,
                                Some('e') | Some('E') => value = value * 16 + 14,
                                Some('f') | Some('F') => value = value * 16 + 15,
                                _ => break,
                            }
                            raw.push(self.chars.next().unwrap());
                        }
                        let kind = if raw.len() == 2 {
                            // If we did not get any digits then report an error.
                            let diagnostic = self.unexpected_peek(ExpectedSyntax::HexadecimalDigit);
                            NumberKind::Invalid(diagnostic)
                        } else {
                            NumberKind::HexadecimalInteger(value)
                        };
                        (ExpectedSyntax::HexadecimalDigit, kind)
                    }

                    // Parse either a decimal integer or a floating point number.
                    _ => {
                        let mut state = NumberState::Whole;
                        loop {
                            match (state, self.chars.peek()) {
                                // Always add digits to the state. Some states will need to
                                // be changed.
                                (NumberState::ExponentStart, Some(c))
                                | (NumberState::ExponentSign, Some(c))
                                    if c.is_digit(10) =>
                                {
                                    state = NumberState::Exponent;
                                }
                                (_, Some(c)) if c.is_digit(10) => {}

                                // Change state based on different characters we see.
                                (NumberState::Whole, Some('.')) => state = NumberState::Fraction,
                                (NumberState::Fraction, Some('e'))
                                | (NumberState::Fraction, Some('E'))
                                | (NumberState::Whole, Some('e'))
                                | (NumberState::Whole, Some('E')) => {
                                    state = NumberState::ExponentStart
                                }
                                (NumberState::ExponentStart, Some('+'))
                                | (NumberState::ExponentStart, Some('-')) => {
                                    state = NumberState::ExponentSign
                                }

                                _ => break,
                            }
                            raw.push(self.chars.next().unwrap());
                        }
                        let kind = match state {
                            // A whole number parses as an integer of arbitrary precision.
                            NumberState::Whole => {
                                NumberKind::DecimalInteger(BigInt::from_str(&raw).unwrap())
                            }

                            // If there was no digit after the start of an exponent or after the
                            // exponent’s sign then we need to error. We must have a decimal digit
                            // after the exponent start.
                            NumberState::ExponentStart | NumberState::ExponentSign => {
                                let diagnostic = self.unexpected_peek(ExpectedSyntax::DecimalDigit);
                                NumberKind::Invalid(diagnostic)
                            }

                            // If we parsed a fraction part or an exponent part then we have a
                            // decimal float.
                            NumberState::Fraction | NumberState::Exponent => {
                                NumberKind::Float(f64::from_str(&raw).unwrap())
                            }
                        };
                        (ExpectedSyntax::DecimalDigit, kind)
                    }
                };
                match self.chars.peek() {
                    // A number may not be followed by an identifier! If our number is followed by
                    // an identifier than report a diagnostic and return an invalid number token.
                    Some(c) if Identifier::is_continue(c) => {
                        let diagnostic = match kind {
                            NumberKind::Invalid(diagnostic) => diagnostic,
                            _ => self.diagnostics.report(Diagnostic::unexpected_char(
                                self.chars.position(),
                                c,
                                expected,
                            )),
                        };
                        raw.push(c);
                        self.chars.next();
                        loop {
                            match self.chars.peek() {
                                Some(c) if Identifier::is_continue(c) => {
                                    raw.push(c);
                                    self.chars.next();
                                }
                                _ => break,
                            }
                        }
                        // Return a number token with an invalid number kind.
                        raw.shrink_to_fit();
                        TokenKind::Number(Number {
                            raw,
                            kind: NumberKind::Invalid(diagnostic),
                        })
                    }

                    // Return the number token we created. If the next character is not part of
                    // an identifier.
                    _ => {
                        raw.shrink_to_fit();
                        TokenKind::Number(Number { raw, kind })
                    }
                }
            }

            // If we encountered an unexpected character then add an unexpected character token.
            Some(c) => TokenKind::UnexpectedChar(c),

            // If we’ve reached the end then create our `EndToken` and return `None`.
            None => {
                self.end = Some(EndToken {
                    position: self.chars.position(),
                    leading_trivia,
                });
                return None;
            }
        };

        let end = self.chars.position();
        let trailing_trivia = self.next_trivia(false);

        let range = Range::new(start, end.utf8_index() - start.utf8_index());

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
                        let mut newline = false;
                        let mut state = BlockCommentState::Normal;
                        let mut depth = 1;
                        let comment = self.chars.span(|c| match (state, c) {
                            (BlockCommentState::FoundAsterisk, '/') => {
                                state = BlockCommentState::Normal;
                                depth -= 1;
                                depth != 0
                            }
                            (BlockCommentState::FoundSlash, '*') => {
                                state = BlockCommentState::Normal;
                                depth += 1;
                                true
                            }
                            (_, '*') => {
                                state = BlockCommentState::FoundAsterisk;
                                true
                            }
                            (_, '/') => {
                                state = BlockCommentState::FoundSlash;
                                true
                            }
                            _ => {
                                state = BlockCommentState::Normal;
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
                            self.chars.next();
                            self.diagnostics.report(Diagnostic::unexpected_ending(
                                self.chars.position(),
                                ExpectedSyntax::BlockCommentEnd,
                            ));
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

    /// Peeks at the next character (or ending) and creates an unexpected diagnostic.
    fn unexpected_peek(&mut self, expected: ExpectedSyntax) -> DiagnosticRef {
        self.diagnostics.report(match self.chars.peek() {
            Some(c) => Diagnostic::unexpected_char(self.chars.position(), c, expected),
            None => Diagnostic::unexpected_ending(self.chars.position(), expected),
        })
    }
}

#[derive(Clone, Copy)]
enum NumberState {
    Whole,
    Fraction,
    ExponentStart,
    ExponentSign,
    Exponent,
}

#[derive(Clone, Copy)]
enum BlockCommentState {
    Normal,
    FoundAsterisk,
    FoundSlash,
}

impl<'src> Token<'src> {
    /// Prints a list of tokens and an end token to a markdown table for debugging.
    pub fn markdown_table(
        document: &'src Document,
        tokens: &Vec<Token<'src>>,
        end_token: &EndToken<'src>,
    ) -> String {
        let mut output = String::new();
        // Add the markdown table header.
        output.push_str(
            "| Range          | Kind                           | Data                       |\n",
        );
        output.push_str(
            "|----------------|--------------------------------|----------------------------|\n",
        );
        // Print each token.
        for token in tokens {
            Self::add_token_to_markdown_table(document, &mut output, token);
        }
        // Print each leading trivia of the end token.
        for trivia in &end_token.leading_trivia {
            Self::add_trivia_to_markdown_table(&mut output, true, trivia);
        }

        // Print the end token.
        let end_position = end_token.position.format(document);
        output.push_str("| ");
        output.push_str(&end_position);
        output.extend(iter::repeat(' ').take(16 - cmp::min(16, end_position.len() + 1)));
        output.push_str("| End                            |                            |\n");

        output
    }

    fn add_token_to_markdown_table(
        document: &'src Document,
        output: &mut String,
        token: &Token<'src>,
    ) {
        for trivia in &token.leading_trivia {
            Self::add_trivia_to_markdown_table(output, true, trivia);
        }

        let range = token.range.format(document);

        let (kind, data) = match &token.kind {
            TokenKind::Glyph(glyph) => ("Glyph", format!("`{}`", glyph.source().to_string())),
            TokenKind::Identifier(identifier) => ("Identifier", format!("`{}`", identifier.0)),
            TokenKind::Number(number) => match &number.kind {
                NumberKind::DecimalInteger(value) => {
                    ("Number::DecimalInteger", value.to_str_radix(10))
                }
                NumberKind::BinaryInteger(value) => {
                    ("Number::BinaryInteger", value.to_str_radix(2))
                }
                NumberKind::HexadecimalInteger(value) => (
                    "Number::HexadecimalInteger",
                    value.to_str_radix(16).to_uppercase(),
                ),
                NumberKind::Float(value) => (
                    "Number::Float",
                    if *value >= 10_000_000_000. {
                        format!("{:e}", value)
                    } else {
                        format!("{}", value)
                    },
                ),
                NumberKind::Invalid(_) => ("Number::Invalid", number.raw.clone()),
            },
            TokenKind::UnexpectedChar(c) => ("UnexpectedChar", format!("`{}`", c)),
        };

        output.push_str("| ");
        output.push_str(&range);
        output.extend(iter::repeat(' ').take(16 - cmp::min(16, range.len() + 1)));
        output.push_str("| ");
        output.push_str(kind);
        output.extend(iter::repeat(' ').take(32 - cmp::min(32, kind.len() + 1)));
        output.push_str("| ");
        output.push_str(&data);
        output.extend(iter::repeat(' ').take(28 - cmp::min(28, data.len() + 1)));
        output.push_str("|\n");

        for trivia in &token.trailing_trivia {
            Self::add_trivia_to_markdown_table(output, false, trivia);
        }
    }

    fn add_trivia_to_markdown_table(output: &mut String, leading: bool, trivia: &Trivia<'src>) {
        let (kind, data) = match trivia {
            Trivia::Spaces(n) => ("Trivia::Spaces", format!("{}", n)),
            Trivia::Tabs(n) => ("Trivia::Tabs", format!("{}", n)),
            Trivia::Newlines(Newline::LF, n) => ("Trivia::Newlines::LF", format!("{}", n)),
            Trivia::Newlines(Newline::CR, n) => ("Trivia::Newlines::CR", format!("{}", n)),
            Trivia::Newlines(Newline::CRLF, n) => ("Trivia::Newlines::CRLF", format!("{}", n)),
            Trivia::Comment(Comment::Line(_)) => ("Trivia::Comment::Line", format!("")),
            Trivia::Comment(Comment::Block(_, _)) => ("Trivia::Comment::Block", format!("")),
            Trivia::OtherWhitespace(_) => ("Trivia::OtherWhitespace", format!("")),
        };

        if leading {
            output.push_str("| leading        | ");
        } else {
            output.push_str("| trailing       | ");
        }
        output.push_str(kind);
        output.extend(iter::repeat(' ').take(32 - cmp::min(32, kind.len() + 1)));
        output.push_str("| ");
        output.push_str(&data);
        output.extend(iter::repeat(' ').take(28 - cmp::min(28, data.len() + 1)));
        output.push_str("|\n");
    }
}
