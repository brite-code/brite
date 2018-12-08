//! Welcome to the parser module! A delightful module.
//!
//! Brite as a language is relatively simple. Brite doesn’t have many complex cover grammars or
//! syntactic ambiguities. Brite also doesn’t have a large surface area. These decisions were made
//! because we want the entire Brite language to easily fit in a programmer’s head. Also to make
//! Brite familiar to programmers coming from a C-inspired syntax. These qualities also happen to
//! make it easier to write a parser.
//!
//! But did we choose to write a simple parser? No! Because we want our parser to have robust error
//! recovery capabilities. That means Brite should be able to take an arbitrary string and produce
//! an AST from that string. Even if the AST is complete garbage. However, if the input string is
//! close to a valid Brite language string then our AST should be really close to a valid Brite AST.
//!
//! For example, in Brite this is the syntax of a binding statement:
//!
//! ```ite
//! let x = 42;
//! ```
//!
//! Let’s say I’m having a bad day and I forget to type `=`.
//!
//! ```ite
//! let x 42;
//! ```
//!
//! Instead of Brite throwing its hands up and saying “Well, I can’t do anything until you fix this
//! syntax error” we instead recognize that this code is pretty close to `let x = 42;`. Our
//! resulting AST should be a binding statement while also letting us know that the `=` was missing.
//!
//! Similarly, let’s say I accidentally add an extra character or maybe I accidentally typed a
//! character instead of `=`.
//!
//! ```ite
//! let value 🐔 = 42; // Oops, silly me, I accidentally typed 🐔.
//!
//! let value 🐶 42; // Oops, silly me, I typed 🐶 instead of `=`.
//! ```
//!
//! We want our parser to recover from errors like this instead of giving up.
//!
//! To accomplish this, we _declaratively_ define our Brite parser. See the invocation of the
//! `parser!` macro in this module. By using the `parser!` macro we can generate code for the normal
//! parsing case and the error recovery parsing case.
//!
//! Instead of using an off-the-shelf parser generator we’ve written our own to control performance,
//! tokenization, error messages, error recovery, and other special syntactic features. We have no
//! intention of generalizing our parser generator at this time.
//!
//! Explore the source code of this module for more information about how everything works!

use super::ast::*;
use super::lexer::Lexer;
use super::token::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef, ParserExpected};

/// Parses a complete Brite module AST out of a stream of lexical tokens. Reports all diagnostics
/// discovered while parsing to the `DiagnosticSet` returned by this function.
///
/// Implements the ability to recover from parse errors.
///
/// Basically calls the statement parser defined in our `parser!` invocation until we reach the end
/// of our document.
pub fn parse(lexer: Lexer) -> Module {
    // Create our parsing context.
    let mut context = ParserContext::new(lexer);
    // Until we reach the end token, parse items. If an item is in recovery mode it will stop trying
    // to recover once it reaches the end token.
    let mut items = Vec::new();
    while !context.lexer.lookahead_end() {
        items.push(p::Statement::parse(&mut context));
    }
    // Optimization: We are done mutating our vector. Shrink it to the smallest size.
    items.shrink_to_fit();
    // We expect that advancing the lexer will give us our `EndToken`. Since we should only exit our
    // above while-loop when we have reached the end.
    let end = context.lexer.advance_end().unwrap();
    // Construct the module and return it.
    Module::new(items, end)
}

/// Our parser generator entry-point. We intend to only invoke this macro _once_. When we do invoke
/// this macro we will provide the syntax for our entire language.
///
/// The parser macro uses a [BNF][1] inspired syntax that better fits Rust’s macro model for
/// defining a [Context Free Grammar][2].
///
/// If you are not familiar with Rust macros then this code will probably be really confusing for
/// you. Some good resources on Rust macros includes the section on macros in [the Rust Book][3] and
/// an in-depth macro reference in [the Rust Reference][4].
///
/// [1]: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
/// [2]: https://en.wikipedia.org/wiki/Context-free_grammar
/// [3]: https://doc.rust-lang.org/1.30.0/book/second-edition/appendix-04-macros.html
/// [4]: https://doc.rust-lang.org/reference/macros-by-example.html
macro_rules! parser {
    (
        $(
            $name:ident ::= $( {$( $symbol:tt )+} )|+
        )*
    ) => {
        mod p {
            //! Our `parser!` macro defines a new module to scope our declaration names since they
            //! might collide with other declaration names. (Particularly from the `ast` module.)
            //! Import everything we need.

            use super::super::ast::{self, Recover};
            use super::super::identifier::Keyword;
            use super::super::token::*;
            use super::{ParserContext, ParserFrom};
            use crate::diagnostics::ParserExpected;

            $(
                parser_rule!($name ::= $( {$( $symbol )*} )|*);
            )*
        }
    };
}

/// Defines a parser based on a single parser rule. Remember that the syntax for parser rules is
/// [BNF][1] inspired.
///
/// [1]: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
macro_rules! parser_rule {
    // Special case for `BooleanConstant`. We need this because otherwise it would be difficult to
    // correctly initialize the true/false value otherwise.
    (BooleanConstant ::= {"true"} | {"false"}) => {
        pub(super) struct BooleanConstant;

        impl BooleanConstant {
            fn test(token: &Token) -> bool {
                token.is_glyph(Glyph::Keyword(Keyword::True)) ||
                    token.is_glyph(Glyph::Keyword(Keyword::False))
            }

            fn try_parse(context: &mut ParserContext) -> Option<ast::BooleanConstant> {
                if let Some(token) = context.lexer.advance_glyph(Glyph::Keyword(Keyword::True)) {
                    Some(ast::BooleanConstant::new(token, true))
                } else if let Some(token) = context.lexer.advance_glyph(Glyph::Keyword(Keyword::False)) {
                    Some(ast::BooleanConstant::new(token, false))
                } else {
                    None
                }
            }
        }
    };
    (
        $name:ident ::= $( {$symbol_1:tt $($symbol_n:tt)*} )|+
    ) => {
        /// A non-terminal in our grammar. The name of this non-terminal should be the same as the
        /// name of a node in our `source::ast` module.
        pub(super) struct $name;

        impl $name {
            /// Tests if this rule may apply used based on looking at the first token. Just because
            /// this returns true we don’t know if the entire parser will succeed. After all, this
            /// is only a hint based on the first token.
            #[allow(dead_code)]
            fn test(token: &Token) -> bool {
                $(parser_symbol_test!(token, $symbol_1))||*
            }

            /// Try to parse the rule. We return `Some()` unless the _first_ token is not valid for
            /// this rule. In that case we return `None`.
            ///
            /// _Returning `None` means we did not advance our lexer!_
            fn try_parse(context: &mut ParserContext) -> Option<ast::$name> {
                $(
                    // Push all the parser test functions in reverse order to our error recovery
                    // stack. If we encounter an unrecognized token we will test each recovery
                    // function to see if any future parsers will be able to handle the token. If no
                    // future parser can handle the token we will skip the token.
                    reverse_statements!($(context.recover_push(parser_symbol_test_fn!($symbol_n))),*);
                    // Try to parse the first symbol. Returns `None` if we could not parse _and_ we
                    // did not advance the lexer.
                    if let Some(x1) = parser_symbol_try_parse!(context, $symbol_1) {
                        // Parse all the other symbols in our rule. Make sure to pop its test
                        // function from our error recovery stack before trying to parse.
                        let data = (x1, $({
                            context.recover_pop();
                            parser_symbol_parse!(context, $symbol_n)
                        }),*);
                        // After we’ve parsed all the data create our AST node from the data we
                        // parsed using our `ParserFrom` trait. Which is kinda like `From` but
                        // specific to our parser.
                        return Some(ParserFrom::from(data));
                    } else {
                        // If we could not parse our first symbol then make sure to cleanup our
                        // error recovery stack.
                        //
                        // Rust won’t let us loop without referencing a looped variable so we have
                        // an `ignore!` macro just for referencing `$symbol_n`.
                        $(ignore!($symbol_n);
                        context.recover_pop();)*
                    }
                )*
                None
            }

            // We want different behaviors depending on whether we have only one expression or
            // multiple expressions. So defer to a function which splits on these two branches.
            parser_rule_parse_fn!($name ::= $({$symbol_1 $($symbol_n)*})|*);
        }
    };
}

macro_rules! parser_rule_parse_fn {
    // If our rule only has one expression...
    (
        $name:ident ::= {$symbol_1:tt $($symbol_n:tt)*}
    ) => {
        // Unimplemented...
    };

    // If our rule has two or more expressions...
    (
        $name:ident ::= {$symbol_1_1:tt $($symbol_1_n:tt)*}
                      | $( {$symbol_n_1:tt $($symbol_n_n:tt)*} )|+
    ) => {
        /// To parse this grammar rule we keep retrying the `try_parse` function until it succeeds
        /// or until we recover.
        pub(super) fn parse(context: &mut ParserContext) -> Recover<ast::$name> {
            context.retry(ParserExpected::$name, $name::try_parse)
        }
    };
}

/// Convert a string literal token into a glyph. The nice thing about using a macro for our language
/// grammar definition is that we can use the actual characters as they appear in source code for
/// each glyph. Instead of referencing glyphs by name.
macro_rules! parser_symbol_glyph {
    ("_") => {
        Glyph::Keyword(Keyword::Hole)
    };
    ("let") => {
        Glyph::Keyword(Keyword::Let)
    };
    ("=") => {
        Glyph::Equals
    };
    ("(") => {
        Glyph::ParenLeft
    };
    (")") => {
        Glyph::ParenRight
    };
    (";") => {
        Glyph::Semicolon
    };
}

/// Test if a token matches the first token of a symbol.
macro_rules! parser_symbol_test {
    ($token:expr, [opt: $symbol:tt]) => {
        parser_symbol_test!($symbol)
    };
    ($token:expr, identifier) => {
        $token.is_identifier()
    };
    ($token:expr, number) => {
        $token.is_number()
    };
    ($token:expr, $name:ident) => {
        $name::test($token)
    };
    ($token:expr, $glyph:tt) => {
        $token.is_glyph(parser_symbol_glyph!($glyph))
    };
}

/// Get a function pointer that takes a token as input and tests if that token is the first token of
/// the provided symbol.
///
/// We need a function pointer for error recovery because at runtime we’ll call a _dynamic_ stack of
/// these functions in `ParserContext::recover` to determine if we can recover from an error.
macro_rules! parser_symbol_test_fn {
    ([opt: $symbol:tt]) => {
        parser_symbol_test_fn!($symbol)
    };
    (identifier) => {
        Token::is_identifier
    };
    (number) => {
        Token::is_number
    };
    ($name:ident) => {
        $name::test
    };
    ($glyph:tt) => {
        |token| token.is_glyph(parser_symbol_glyph!($glyph))
    };
}

/// Try to parse a symbol. If the symbol fails to parse and we _do not_ advance the lexer then we
/// return `None`. Otherwise we return `Some()`. Even if there was an error.
macro_rules! parser_symbol_try_parse {
    ($context:expr, [opt: $symbol:tt]) => {
        parser_symbol_try_parse!($context, $symbol)
    };
    ($context:expr, identifier) => {
        $context.lexer.advance_identifier()
    };
    ($context:expr, number) => {
        $context.lexer.advance_number()
    };
    ($context:expr, $name:ident) => {
        $name::try_parse($context)
    };
    ($context:expr, $glyph:tt) => {
        $context.lexer.advance_glyph(parser_symbol_glyph!($glyph))
    };
}

/// Parses a symbol. Usually by calling `ParserContext::retry` with the `try_parse` version of
/// the parser.
macro_rules! parser_symbol_parse {
    ($context:expr, [opt: $symbol:tt]) => {
        parser_symbol_try_parse!($context, $symbol)
    };
    ($context:expr, identifier) => {
        $context.retry(ParserExpected::Identifier, |context| {
            context.lexer.advance_identifier()
        })
    };
    ($context:expr, number) => {
        $context.retry(ParserExpected::Number, |context| {
            context.lexer.advance_number()
        })
    };
    ($context:expr, $name:ident) => {
        $name::parse($context)
    };
    ($context:expr, $glyph:tt) => {
        $context.retry(
            ParserExpected::Glyph(parser_symbol_glyph!($glyph)),
            |context| context.lexer.advance_glyph(parser_symbol_glyph!($glyph)),
        )
    };
}

/// Executes a list of statements in reverse order. Needed to setup our error recovery stack in
/// macros above.
macro_rules! reverse_statements {
    () => {};
    ($s1:stmt) => {
        $s1;
    };
    ($s1:stmt, $($sn:stmt),*) => {
        reverse_statements!($($sn),*);
        $s1;
    };
}

/// A simple macro that ignores its argument. Needed for iterating over a macro loop when we don’t
/// use any of the variables in that loop.
macro_rules! ignore {
    ($x:tt) => {};
}

/// Yay! Finally, our `parser!` macro invocation. This is where we define Brite the language.
/// Remember that the syntax of `parser!` is [BNF][1] inspired.
///
/// [1]: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
parser! {
    Statement ::= {ExpressionStatement}
                | {BindingStatement}
                | {EmptyStatement}

    ExpressionStatement ::= {Expression [opt: ";"]}

    BindingStatement ::= {"let" Pattern "=" Expression [opt: ";"]}

    EmptyStatement ::= {";"}

    BooleanConstant ::= {"true"} | {"false"}

    NumberConstant ::= {number}

    Expression ::= {VariableExpression}
                 | {BooleanConstant}
                 | {NumberConstant}
                 | {WrappedExpression}

    VariableExpression ::= {identifier}

    WrappedExpression ::= {"(" Expression ")"}

    Pattern ::= {VariablePattern}
              | {HolePattern}

    VariablePattern ::= {identifier}

    HolePattern ::= {"_"}
}

/// Our context while parsing. Holds the lexer and a an error recovery stack.
struct ParserContext<'a> {
    /// The lexer we use for producing tokens as we parse.
    lexer: Lexer<'a>,
    /// The error recovery stack is a stack of functions that take a `Token` as an input and return
    /// a boolean. When we encounter a token we don’t recognize we call all the functions in
    /// this stack. If any of the functions return `true` then we know that one of our callers wants
    /// us to recover from this function.
    recover: Vec<fn(&Token) -> bool>,
}

impl<'a> ParserContext<'a> {
    /// Creates a new parser context.
    fn new(lexer: Lexer<'a>) -> Self {
        ParserContext {
            lexer,
            recover: Vec::new(),
        }
    }

    /// Adds a function to our error recovery stack.
    fn recover_push(&mut self, f: fn(&Token) -> bool) {
        self.recover.push(f);
    }

    /// Removes the last function to be added from our error recovery stack.
    fn recover_pop(&mut self) {
        self.recover.pop();
    }

    /// Retries a parsing function while skipping unrecognized tokens until either:
    ///
    /// 1. The parsing function successfully returns.
    /// 2. Someone in our error recovery stack tells us that we should recover from an
    ///    unrecognized token.
    ///
    /// The parsing function we accept should return `None` if parsing failed _and_ we did not
    /// advance the lexer.
    ///
    /// Also takes a `ParserExpected` value for diagnostics.
    fn retry<T>(
        &mut self,
        expected: ParserExpected,
        try_parse: impl Fn(&mut ParserContext) -> Option<T>,
    ) -> Recover<T> {
        // Try to parse our node. If successful return the node. If unsuccesful then we are in
        // error recovery mode!
        if let Some(x) = try_parse(self) {
            Ok(x)
        } else {
            self.recover(expected, try_parse)
        }
    }

    /// Implements the error recovery part of `ParserContext::retry`. We implement this in a
    /// separate function so that Rust may inline the functions separately.
    fn recover<T>(
        &mut self,
        expected: ParserExpected,
        try_parse: impl Fn(&mut ParserContext) -> Option<T>,
    ) -> Recover<T> {
        // Setup information we will need for creating errors.
        let mut skipped = Vec::new();
        let mut first_diagnostic = None;
        // At the beginning of this loop we know that the current token (returned by
        // `Lexer::lookahead`) can _not_ be parsed by `try_parse`.
        //
        // We will first check to see if one of the parsing functions in our stack can recover
        // from this token. If no parsing function can recover from this token then we will
        // advance the lexer and try to parse again.
        loop {
            let token = self.lexer.lookahead();

            // Now that we know this token fails to parse, let’s check if we can recover from
            // this token.
            //
            // If the token is an end token we _must_ recover. If we don’t recover we will be in
            // this loop forever since the lexer will keep giving us the end token.
            let recover = if let Token::End(_) = token {
                true
            } else {
                // If any of the recovery functions in our stack match the current token then
                // we recover. If _all_ recovery functions return false then we know no one can
                // handle this token so we should skip it.
                //
                // We reverse the iterator because we expect local errors to be more common. So
                // the last recovery functions on the stack should be more likely to match
                // the token.
                self.recover.iter().rev().any(|recover| recover(token))
            };

            // If we recover then return `Err()` instead of advancing the lexer.
            if recover {
                // If we can recover from this token then presumably it is part of a valid
                // AST node to be parsed later. So we only want to report an error if we have
                // not found any other unexpected tokens. Meaning this token is certainly out
                // of place.
                let diagnostic = match first_diagnostic {
                    Some(diagnostic) => diagnostic,
                    None => {
                        let token = token.clone();
                        self.unexpected_token(token, expected)
                    }
                };
                let error = RecoverError::new(skipped, diagnostic, None);
                return Err(Box::new(error));
            }

            // Advance the lexer and push the token to our `skipped` list.
            let token = self.lexer.advance();
            skipped.push(token.clone());

            // Report an unexpected token diagnostic for every token that we skip. Keep the
            // first skipped token diagnostic around. We’ll use this diagnostic in the AST to
            // cause runtime crashes.
            let diagnostic = self.unexpected_token(token, expected.clone());
            if first_diagnostic.is_none() {
                first_diagnostic = Some(diagnostic);
            }

            // Retry our parsing function.
            match try_parse(self) {
                // If we were able to parse our node then, hooray! However, we still encountered
                // an error which must not be ignored. So return `Recover::Error` instead of
                // `Recover::Ok` with the skipped tokens and the first diagnostic we reported
                // in our error recovery process.
                Some(x) => {
                    let error = RecoverError::new(skipped, first_diagnostic.unwrap(), Some(x));
                    return Err(Box::new(error));
                }
                // If our parsing function failed, again, then do nothing and go back to the top
                // of our loop.
                None => {}
            }
        }
    }

    /// Report an unexpected token error. The diagnostics object is owned by our lexer.
    fn unexpected_token(&mut self, unexpected: Token, expected: ParserExpected) -> DiagnosticRef {
        let range = unexpected.full_range().range();
        let diagnostic = Diagnostic::unexpected_token(range, unexpected, expected);
        self.lexer.diagnostics_mut().report(diagnostic)
    }
}

/// Conversion from a data type produced by a parser generated from `parser!` to an AST node.
trait ParserFrom<T> {
    fn from(data: T) -> Self;
}

/// Implicit convenience conversion for types that implement `Into<T>`.
impl<T, U: Into<T>> ParserFrom<(U,)> for T {
    fn from((data,): (U,)) -> T {
        data.into()
    }
}

// `ParserFrom` implementations. Pretty boring, but required, stuff...

type BindingStatementData = (
    GlyphToken,
    Recover<Pattern>,
    Recover<GlyphToken>,
    Recover<Expression>,
    Option<GlyphToken>,
);

impl ParserFrom<BindingStatementData> for BindingStatement {
    fn from((let_, pattern, equals, value, semicolon): BindingStatementData) -> Self {
        BindingStatement::new(let_, pattern, equals, value, semicolon)
    }
}

impl ParserFrom<(Expression, Option<GlyphToken>)> for ExpressionStatement {
    fn from((expression, semicolon): (Expression, Option<GlyphToken>)) -> Self {
        ExpressionStatement::new(expression, semicolon)
    }
}

impl ParserFrom<(GlyphToken,)> for EmptyStatement {
    fn from((semicolon,): (GlyphToken,)) -> Self {
        EmptyStatement::new(semicolon)
    }
}

impl ParserFrom<(NumberToken,)> for NumberConstant {
    fn from((number,): (NumberToken,)) -> Self {
        NumberConstant::new(number)
    }
}

impl ParserFrom<(IdentifierToken,)> for VariableExpression {
    fn from((identifier,): (IdentifierToken,)) -> Self {
        VariableExpression::new(identifier)
    }
}

type WrappedExpressionData = (GlyphToken, Recover<Expression>, Recover<GlyphToken>);

impl ParserFrom<WrappedExpressionData> for WrappedExpression {
    fn from((paren_left, expression, paren_right): WrappedExpressionData) -> Self {
        WrappedExpression::new(paren_left, expression, paren_right)
    }
}

impl ParserFrom<(IdentifierToken,)> for VariablePattern {
    fn from((identifier,): (IdentifierToken,)) -> Self {
        VariablePattern::new(identifier)
    }
}

impl ParserFrom<(GlyphToken,)> for HolePattern {
    fn from((hole,): (GlyphToken,)) -> Self {
        HolePattern::new(hole)
    }
}
