use super::document::{Document, DocumentChars, Position, Range};
use crate::diagnostics::*;
use num::BigInt;
use std::cmp;
use std::f64;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter;
use std::rc::Rc;
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
    pub range: Range,
    pub leading_trivia: Vec<Trivia<'src>>,
    pub trailing_trivia: Vec<Trivia<'src>>,
    pub kind: TokenKind,
}

/// The kind of a token.
pub enum TokenKind {
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

    /// Creates an [`UnexpectedSyntax`] description for this token.
    pub fn unexpected(&self) -> UnexpectedSyntax {
        match &self.kind {
            TokenKind::Glyph(glyph) => UnexpectedSyntax::Glyph(*glyph),
            TokenKind::Identifier(_) => UnexpectedSyntax::Identifier,
            TokenKind::Number(_) => UnexpectedSyntax::Number,
            TokenKind::UnexpectedChar(c) => UnexpectedSyntax::Char(*c),
        }
    }

    /// Is this the first token on a newline?
    pub fn first_on_newline(&self) -> bool {
        for trivia in &self.leading_trivia {
            if let Trivia::Newlines(_, _) = trivia {
                return true;
            }
        }
        false
    }

    /// Add the source code we parsed this token from back to a string.
    fn add_source(&self, source: &mut String) {
        for trivia in &self.leading_trivia {
            trivia.add_source(source);
        }
        match &self.kind {
            TokenKind::Glyph(glyph) => source.push_str(glyph.as_str()),
            TokenKind::Identifier(identifier) => source.push_str(&identifier.as_str()),
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
    /// The ending position of our document.
    pub position: Position,
    /// All the trivia which comes before the end of our document.
    pub leading_trivia: Vec<Trivia<'src>>,
}

impl<'src> EndToken<'src> {
    /// Get the position of our end token.
    pub fn position(&self) -> Position {
        self.position
    }

    /// Add the source code we parsed this token from back to a string.
    fn add_source(&self, source: &mut String) {
        for trivia in &self.leading_trivia {
            trivia.add_source(source);
        }
    }
}

/// A glyph represents some constant sequence of characters that is used in Brite syntax.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Glyph {
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
    pub fn as_str(&self) -> &'static str {
        use self::Glyph::*;
        match self {
            Keyword(keyword) => keyword.as_str(),
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
///
/// There are also some [`IdentifierKeyword`]s which are not reserved in our [`Identifier`] syntax.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Keyword {
    /// `_`
    Hole,
    /// `true`
    True,
    /// `false`
    False,
    /// `void`
    Void,
    /// `fun`
    Fun,
    /// `let`
    Let,
    /// `if`
    If,
    /// `else`
    Else,
    /// `return`
    Return,
    /// `do`
    Do,
    /// `this`
    This,
}

impl Keyword {
    /// Converts a string into a keyword if the string is a keyword.
    fn from_str(str: &str) -> Option<Keyword> {
        use self::Keyword::*;
        match str {
            "_" => Some(Hole),
            "true" => Some(True),
            "false" => Some(False),
            "void" => Some(Void),
            "fun" => Some(Fun),
            "let" => Some(Let),
            "if" => Some(If),
            "else" => Some(Else),
            "return" => Some(Return),
            "do" => Some(Do),
            "this" => Some(This),
            _ => None,
        }
    }

    /// Get the source string this keyword was parsed from. Always a static string.
    fn as_str(&self) -> &'static str {
        use self::Keyword::*;
        match self {
            Hole => "_",
            True => "true",
            False => "false",
            Void => "void",
            Fun => "fun",
            Let => "let",
            If => "if",
            Else => "else",
            Return => "return",
            Do => "do",
            This => "this",
        }
    }
}

/// An identifier that we give special meaning to. These keywords are not reserved in the identifier
/// syntax like [`Keyword`] but nevertheless have syntactic meaning.
///
/// We choose a [`Keyword`] when using an [`Identifier`] would be ambiguous. For example, `if` is a
/// [`Keyword`] because if we see `if` it could be ambiguous to whether we want a conditional
/// expression or a variable reference expression.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IdentifierKeyword {
    /// `class`
    Class,
    /// `base`
    Base,
    /// `extends`
    Extends,
}

impl IdentifierKeyword {
    /// Get the source string for this identifier keyword was parsed from. Always a static string.
    pub fn as_str(&self) -> &'static str {
        use self::IdentifierKeyword::*;
        match self {
            Class => "class",
            Base => "base",
            Extends => "extends",
        }
    }

    /// Tests if this identifier keyword matches the provided identifier.
    pub fn test(&self, identifier: &Identifier) -> bool {
        self.as_str() == identifier.as_str()
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
/// ## Representation
///
/// Identifiers are represented using a backing, shared, string and a pointer/length into the
/// string. You can think of an identifier as a slice of a reference-counted [`String`].
///
/// We use `u32`s for the start and length since we’ll have a lot of identifiers in our AST. Also
/// `u32`s match the representation for [`Position`].
///
/// [1]: http://www.unicode.org/reports/tr31
pub struct Identifier {
    source: Rc<String>,
    start: u32,
    length: u32,
}

impl Identifier {
    /// Creates a new identifier. Returns `None` if the string is not a valid identifier. If the
    /// identifier is a keyword then it is not a valid identifier so we return `None`.
    ///
    /// If we have a valid identifier then this allocates a new reference-counted string based on
    /// the provided identifier string.
    pub fn new(identifier: &str) -> Option<Identifier> {
        let mut chars = identifier.chars();
        match chars.next() {
            None => return None,
            Some(c) if !Self::is_start(c) => return None,
            Some(_) => {}
        }
        while let Some(c) = chars.next() {
            if !Self::is_continue(c) {
                return None;
            }
        }
        if Keyword::from_str(identifier).is_some() {
            return None;
        }
        Some(Identifier {
            source: Rc::new(identifier.to_string()),
            start: 0,
            length: identifier.len() as u32,
        })
    }

    /// Gets the source string for this identifier.
    #[inline(always)]
    pub fn as_str(&self) -> &str {
        &self.source[(self.start as usize)..((self.start + self.length) as usize)]
    }

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

impl Clone for Identifier {
    #[inline(always)]
    fn clone(&self) -> Self {
        Identifier {
            source: Rc::clone(&self.source),
            start: self.start,
            length: self.length,
        }
    }
}

impl PartialEq<Identifier> for Identifier {
    #[inline(always)]
    fn eq(&self, other: &Identifier) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for Identifier {}

impl Hash for Identifier {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
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
pub struct Number {
    /// The raw string this number was parsed from.
    raw: String,
    /// The kind of number we parsed
    pub kind: NumberKind,
}

/// The kind of a number.
pub enum NumberKind {
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
pub enum Trivia<'src> {
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
pub enum Newline {
    /// `\n`
    LF,
    /// `\r`
    CR,
    /// `\r\n`
    CRLF,
}

pub enum Comment<'src> {
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

/// A lexer generates [`Token`]s based on a source string input. Call [`Lexer::next`] to advance the
/// lexer and [`Lexer::end`] to get the end token. All of the [`Token`]s and [`EndToken`] may be
/// used to print back out a string which is equivalent to the source code we parsed from.
pub struct Lexer<'errs, 'src> {
    /// The diagnostics collection we report lexer errors in. Use [`Lexer::report_diagnostic`]
    /// instead of using this reference directly so that we may switch out the implementation of
    /// [`Lexer::report_diagnostic`] at any time.
    _diagnostics: &'errs mut DiagnosticsCollection,
    /// The document itself we are lexing.
    document: &'src Document,
    /// In iterator of document characters which also keeps track of the current position.
    chars: DocumentChars<'src>,
    /// The last token in a document. If we have an end token then the lexer is done iterating.
    end: Option<EndToken<'src>>,
    /// The programmer may lookahead at the next token, if they have done so we’ll have a
    /// token here.
    lookahead: Option<Option<Token<'src>>>,
}

impl<'errs, 'src> Lexer<'errs, 'src> {
    /// Creates a new lexer from source code.
    pub fn new(
        diagnostics: &'errs mut DiagnosticsCollection,
        document: &'src Document,
    ) -> Lexer<'errs, 'src> {
        Lexer {
            _diagnostics: diagnostics,
            document,
            chars: document.chars(),
            end: None,
            lookahead: None,
        }
    }

    /// Look at the next token without advancing the iterator. Next time [`Lexer::advance`] is
    /// called the same token will be returned and the iterator will advance.
    ///
    /// Peeking _will_ advance the lexer’s end token state which is observed through
    /// [`Lexer::lookahead_end`]. If [`Lexer::advance`] returns `Some()` and [`Lexer::lookahead`]
    /// returns `None`, then [`Lexer::lookahead_end`] will return an [`EndToken`] even though
    /// [`Lexer::advance`] has not advanced the lexer to the end.
    pub fn lookahead(&mut self) -> Option<&Token<'src>> {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.advance());
        }
        match &self.lookahead {
            Some(next) => next.as_ref(),
            None => unreachable!(),
        }
    }

    /// When one is done with their lexer they may call this function to get the final [`EndToken`].
    /// If [`Lexer::advance`] returns `None` it is guaranteed that [`Lexer::end`] will return
    /// an [`EndToken`].
    ///
    /// If [`Lexer::lookahead`] returns `None` then [`Lexer::end`] will also return an [`EndToken`].
    /// This means peeking will advance the iterator’s end token state.
    pub fn end(self) -> Option<EndToken<'src>> {
        self.end
    }

    /// Allows one to peek at the lexer’s [`EndToken`] without consuming the lexer. If
    /// [`Lexer::next`] returns `None` it is guaranteed that [`Lexer::peek_end`] will return
    /// an [`EndToken`].
    ///
    /// If [`Lexer::lookahead`] returns `None` then [`Lexer::lookahead_end`] will also return an
    /// [`EndToken`]. This means peeking will advance the iterator’s end token state.
    pub fn lookahead_end(&self) -> Option<&EndToken<'src>> {
        self.end.as_ref()
    }

    /// Report a diagnostic.
    ///
    /// The implementation may change at any time. Public in the parent-module so that our parser
    /// can call this function since our lexer owns a unique mutable reference to the
    /// diagnostics collection.
    pub(super) fn report_diagnostic(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        self._diagnostics.report(diagnostic)
    }

    /// Advances the lexer and returns the next token.
    ///
    /// We don’t implement [`Iterator`] since our lexer provides many stateful services which would
    /// be hidden or broken by iterator composition.
    pub fn advance(&mut self) -> Option<Token<'src>> {
        // If we’ve peeked, then let’s return the peeked iterator step.
        if let Some(next) = self.lookahead.take() {
            return next;
        }

        // If we have ended then keep returning `None`.
        if self.end.is_some() {
            return None;
        }

        let leading_trivia = self.next_trivia(true);
        let start = self.chars.position();

        let kind = match self.chars.advance() {
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
            Some('&') => match self.chars.lookahead() {
                Some('&') => {
                    self.chars.advance();
                    TokenKind::Glyph(Glyph::AmpersandDouble)
                }
                _ => TokenKind::Glyph(Glyph::Ampersand),
            },
            Some('|') => match self.chars.lookahead() {
                Some('|') => {
                    self.chars.advance();
                    TokenKind::Glyph(Glyph::BarDouble)
                }
                _ => TokenKind::Glyph(Glyph::Bar),
            },
            Some('=') => match self.chars.lookahead() {
                Some('=') => {
                    self.chars.advance();
                    TokenKind::Glyph(Glyph::EqualsDouble)
                }
                _ => TokenKind::Glyph(Glyph::Equals),
            },
            Some('!') => match self.chars.lookahead() {
                Some('=') => {
                    self.chars.advance();
                    TokenKind::Glyph(Glyph::EqualsNot)
                }
                _ => TokenKind::Glyph(Glyph::Bang),
            },
            Some('>') => match self.chars.lookahead() {
                Some('=') => {
                    self.chars.advance();
                    TokenKind::Glyph(Glyph::GreaterThanOrEqual)
                }
                _ => TokenKind::Glyph(Glyph::GreaterThan),
            },
            Some('<') => match self.chars.lookahead() {
                Some('=') => {
                    self.chars.advance();
                    TokenKind::Glyph(Glyph::LessThanOrEqual)
                }
                _ => TokenKind::Glyph(Glyph::LessThan),
            },
            Some('-') => match self.chars.lookahead() {
                Some('>') => {
                    self.chars.advance();
                    TokenKind::Glyph(Glyph::Arrow)
                }
                _ => TokenKind::Glyph(Glyph::Minus),
            },

            // Identifier
            Some(c) if Identifier::is_start(c) => {
                loop {
                    match self.chars.lookahead() {
                        Some(c) if Identifier::is_continue(c) => {
                            self.chars.advance();
                        }
                        _ => break,
                    }
                }
                let start = start.index_utf8();
                let end = self.chars.position().index_utf8();
                let identifier = &self.document.source()[(start as usize)..(end as usize)];
                if let Some(keyword) = Keyword::from_str(identifier) {
                    TokenKind::Glyph(Glyph::Keyword(keyword))
                } else {
                    TokenKind::Identifier(Identifier {
                        source: self.document.source_rc(),
                        start,
                        length: end - start,
                    })
                }
            }

            // Number
            Some(c) if c.is_digit(10) || c == '.' => {
                if c == '.'
                    && self
                        .chars
                        .lookahead()
                        .map(|c| !c.is_digit(10))
                        .unwrap_or(true)
                {
                    TokenKind::Glyph(Glyph::Dot)
                } else {
                    let mut raw = String::new();
                    raw.push(c);

                    let (expected, kind) = match (c, self.chars.lookahead()) {
                        // Binary integer
                        ('0', Some('b')) | ('0', Some('B')) => {
                            raw.push(self.chars.advance().unwrap());
                            let mut value = BigInt::from(0);
                            loop {
                                match self.chars.lookahead() {
                                    Some('0') => value = value * 2 + 0,
                                    Some('1') => value = value * 2 + 1,
                                    _ => break,
                                }
                                raw.push(self.chars.advance().unwrap());
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
                            raw.push(self.chars.advance().unwrap());
                            let mut value = BigInt::from(0);
                            loop {
                                match self.chars.lookahead() {
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
                                raw.push(self.chars.advance().unwrap());
                            }
                            let kind = if raw.len() == 2 {
                                // If we did not get any digits then report an error.
                                let diagnostic =
                                    self.unexpected_peek(ExpectedSyntax::HexadecimalDigit);
                                NumberKind::Invalid(diagnostic)
                            } else {
                                NumberKind::HexadecimalInteger(value)
                            };
                            (ExpectedSyntax::HexadecimalDigit, kind)
                        }

                        // Parse either a decimal integer or a floating point number.
                        _ => {
                            let mut state = if c == '.' {
                                NumberState::Fraction
                            } else {
                                NumberState::Whole
                            };
                            loop {
                                match (state, self.chars.lookahead()) {
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
                                    (NumberState::Whole, Some('.')) => {
                                        state = NumberState::Fraction
                                    }
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
                                raw.push(self.chars.advance().unwrap());
                            }
                            let kind = match state {
                                // A whole number parses as an integer of arbitrary precision.
                                NumberState::Whole => {
                                    NumberKind::DecimalInteger(BigInt::from_str(&raw).unwrap())
                                }

                                // If there was no digit after the start of an exponent or after the
                                // exponent’s sign then we need to error. We must have a decimal
                                // digit after the exponent start.
                                NumberState::ExponentStart | NumberState::ExponentSign => {
                                    let diagnostic =
                                        self.unexpected_peek(ExpectedSyntax::DecimalDigit);
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
                    match self.chars.lookahead() {
                        // A number may not be followed by an identifier! If our number is followed
                        // by an identifier than report a diagnostic and return an invalid
                        // number token.
                        Some(c) if Identifier::is_continue(c) => {
                            let diagnostic = match kind {
                                NumberKind::Invalid(diagnostic) => diagnostic,
                                _ => {
                                    let position = self.chars.position();
                                    let diagnostic =
                                        Diagnostic::unexpected_char(position, c, expected);
                                    self.report_diagnostic(diagnostic)
                                }
                            };
                            raw.push(c);
                            self.chars.advance();
                            loop {
                                match self.chars.lookahead() {
                                    Some(c) if Identifier::is_continue(c) => {
                                        raw.push(c);
                                        self.chars.advance();
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

        let range = Range::new(start, end.index_utf8() - start.index_utf8());

        // Return the token we just parsed.
        Some(Token {
            range,
            leading_trivia,
            trailing_trivia,
            kind,
        })
    }

    /// Parses some token trivia. If `leading` is true then we are parsing leading trivia. Otherwise
    /// we are parsing trailing trivia.
    fn next_trivia(&mut self, leading: bool) -> Vec<Trivia<'src>> {
        let mut trivia = Vec::new();

        loop {
            match self.chars.lookahead() {
                // Spaces
                Some(' ') => {
                    self.chars.advance();
                    let mut n = 1;
                    while let Some(' ') = self.chars.lookahead() {
                        self.chars.advance();
                        n += 1;
                    }
                    trivia.push(Trivia::Spaces(n));
                }

                // Tabs
                Some('\t') => {
                    self.chars.advance();
                    let mut n = 1;
                    while let Some('\t') = self.chars.lookahead() {
                        self.chars.advance();
                        n += 1;
                    }
                    trivia.push(Trivia::Tabs(n));
                }

                // Newlines (LF)
                Some('\n') => {
                    // If we are parsing trailing trivia then don’t parse newlines!
                    if !leading {
                        break;
                    }

                    self.chars.advance();
                    let mut n = 1;

                    while let Some('\n') = self.chars.lookahead() {
                        self.chars.advance();
                        n += 1;
                    }
                    trivia.push(Trivia::Newlines(Newline::LF, n));
                }

                // Newlines (CR and CRLF)
                Some('\r') => {
                    // If we are parsing trailing trivia then don’t parse newlines!
                    if !leading {
                        break;
                    }

                    self.chars.advance();
                    let mut n = 1;

                    if let Some('\n') = self.chars.lookahead() {
                        self.chars.advance();

                        while self.chars.lookahead() == Some('\r')
                            && self.chars.lookahead2() == Some('\n')
                        {
                            self.chars.advance();
                            self.chars.advance();
                            n += 1;
                        }
                        trivia.push(Trivia::Newlines(Newline::CRLF, n));
                    } else {
                        while let Some('\r') = self.chars.lookahead() {
                            self.chars.advance();
                            n += 1;
                        }
                        trivia.push(Trivia::Newlines(Newline::CR, n));
                    }
                }

                // Comments
                Some('/') => match self.chars.lookahead2() {
                    // Line comments
                    Some('/') => {
                        self.chars.advance();
                        self.chars.advance();

                        // Get the starting index.
                        let start = self.chars.position().index_utf8() as usize;

                        // Loop through all of the characters in our comment. The comment either
                        // ends when we’ve reached the end of our source or when we see a newline.
                        //
                        // This loop will return the length of the remaining string slice. We will
                        // use that length to get a sub-slice for our comment.
                        loop {
                            match self.chars.lookahead() {
                                None => break,
                                Some('\n') | Some('\r') => break,
                                Some(_) => {
                                    self.chars.advance();
                                }
                            }
                        }

                        // This is safe because we know that `Position` falls on UTF-8 boundaries.
                        let end = self.chars.position().index_utf8() as usize;
                        let comment = &self.document.source()[start..end];

                        trivia.push(Trivia::Comment(Comment::Line(comment)));
                    }

                    // Block comments
                    Some('*') => {
                        self.chars.advance();
                        self.chars.advance();

                        // Get the starting index.
                        let start = self.chars.position().index_utf8() as usize;
                        // Remember if we see newlines in our comment. If there’s a newline inside
                        // our comment and we’re parsing trailing trivia we’ll want to stop
                        // parsing trivia.
                        let mut newline = false;
                        // We allow block comment nesting so keep track of the block comment depth.
                        let mut depth = 1;

                        // Loop through all the characters in a block comment. There are a couple
                        // things to consider:
                        //
                        // - We need to maintain the correct position as we iterate. Including new
                        //   lines. Including CRLF.
                        // - We need to respect block comment depth.
                        // - We need to break out of the loop when we reach the end of our
                        //   source code.
                        // - We need to report the length of our remaining source so that we can get
                        //   a sub-slice of the source for our comment.
                        let ends_ok = loop {
                            match self.chars.advance() {
                                // Subtract from the block comment depth and possible break out of
                                // the loop.
                                Some('*') => match self.chars.lookahead() {
                                    Some('/') => {
                                        self.chars.advance();
                                        depth -= 1;
                                        if depth == 0 {
                                            break true;
                                        }
                                    }
                                    _ => {}
                                },
                                // Add to the block comment depth.
                                Some('/') => match self.chars.lookahead() {
                                    Some('*') => {
                                        self.chars.advance();
                                        depth += 1;
                                    }
                                    _ => {}
                                },
                                // LF
                                Some('\n') => newline = true,
                                // CR and CRLF
                                Some('\r') => {
                                    newline = true;
                                    if let Some('\n') = self.chars.lookahead() {
                                        self.chars.advance();
                                    }
                                }
                                // Other characters
                                Some(_) => {}
                                // Exit if there are no more characters.
                                None => break false,
                            }
                        };

                        // This is safe because we know that `Position` falls on UTF-8 boundaries.
                        let end = self.chars.position().index_utf8() as usize;
                        let end = if ends_ok { end - 2 } else { end };
                        let comment = &self.document.source()[start..end];

                        // If the block comment did not end ok then we need to report an
                        // error diagnostic!
                        if ends_ok {
                            trivia.push(Trivia::Comment(Comment::Block(comment, true)));
                        } else {
                            self.report_diagnostic(Diagnostic::unexpected_ending(
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
                    trivia.push(Trivia::OtherWhitespace(c));
                    self.chars.advance();
                }

                // If there is no more trivia then we may stop the loop.
                _ => break,
            }
        }

        trivia
    }

    /// Peeks at the next character (or ending) and creates an unexpected diagnostic.
    fn unexpected_peek(&mut self, expected: ExpectedSyntax) -> DiagnosticRef {
        let position = self.chars.position();
        let diagnostic = match self.chars.lookahead() {
            Some(c) => Diagnostic::unexpected_char(position, c, expected),
            None => Diagnostic::unexpected_ending(position, expected),
        };
        self.report_diagnostic(diagnostic)
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

impl<'src> Token<'src> {
    /// Prints a list of tokens and an end token to a markdown table for debugging.
    pub fn markdown_table(
        document: &Document,
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
        let end_position = end_token.position.display(document);
        output.push_str("| ");
        output.push_str(&end_position);
        output.extend(iter::repeat(' ').take(16 - cmp::min(16, end_position.len() + 1)));
        output.push_str("| End                            |                            |\n");

        output
    }

    fn add_token_to_markdown_table(document: &Document, output: &mut String, token: &Token<'src>) {
        for trivia in &token.leading_trivia {
            Self::add_trivia_to_markdown_table(output, true, trivia);
        }

        let range = token.range.display(document);

        let (kind, data) = match &token.kind {
            TokenKind::Glyph(glyph) => ("Glyph", format!("`{}`", glyph.as_str())),
            TokenKind::Identifier(identifier) => ("Identifier", format!("`{}`", identifier)),
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
