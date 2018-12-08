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
use super::identifier::Keyword;
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
    // While we are parsing, we want to recover at the next statement.
    context.recover_push(p::Statement::recover);
    // Until we reach the end token, parse items. If an item is in recovery mode it will stop trying
    // to recover once it reaches the end token.
    let mut items = Vec::new();
    while !context.lexer.lookahead_end() {
        items.push(p::Statement::parse(&mut context));
    }
    // Pop our recovery function we added to context and assert that our error recovery stack should
    // be empty.
    context.recover_pop();
    debug_assert!(context.recover.is_empty());
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
    (
        $name:ident ::= $( {$symbol_1:tt $($symbol_n:tt)*} )|+
    ) => {
        /// A non-terminal in our grammar. The name of this non-terminal should be the same as the
        /// name of a node in our `source::ast` module.
        pub(super) struct $name;

        impl $name {
            /// Try to parse the rule. We return `Some()` unless the _first_ token is not valid for
            /// this rule. In that case we return `None`.
            ///
            /// We need this function for speculative parsing. That is if we have a rule `Bar` that
            /// looks like:
            ///
            /// ```txt
            /// Bar ::= {D E F} | {G H I}
            /// ```
            ///
            /// We will call `D::try_parse` and `G::try_parse` to choose which branch to take.
            ///
            /// _Returning `None` means we did not advance our lexer!_
            #[allow(dead_code)]
            fn try_parse(context: &mut ParserContext) -> Option<ast::$name> {
                $(
                    // Push all the parser recover functions in reverse order to our error recovery
                    // stack. If we encounter an unrecognized token we will test each recovery
                    // function to see if any future parsers will be able to handle the token. If no
                    // future parser can handle the token we will skip the token.
                    reverse_statements!($(context.recover_push(parser_symbol_recover_fn!($symbol_n))),*);
                    // Try to parse the first symbol. Returns `None` if we could not parse _and_ we
                    // did not advance the lexer.
                    if let Some(x1) = parser_symbol_try_parse!(context, $symbol_1) {
                        // Parse all the other symbols in our rule. Make sure to pop its recover
                        // function from our error recovery stack before trying to parse.
                        let data = (x1, $({
                            context.recover_pop();
                            parser_symbol_parse!(context, $symbol_n)
                        }),*);
                        // After we’ve parsed all the data, create our AST node using our
                        // `ParserFrom` trait. Which is like `From` but specific to our parser.
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
            // multiple expressions. So defer to a macro which splits on these two branches.
            parser_rule_extra!($name ::= $({$symbol_1 $($symbol_n)*})|*);
        }
    };
}

/// Extra definitions for the the `parser_rule!` `impl`. We have a separate macro so that we can
/// split on one expression vs. multiple expressions.
macro_rules! parser_rule_extra {
    // If our rule only has one expression...
    (
        $name:ident ::= {$symbol_1:tt $($symbol_n:tt)*}
    ) => {
        /// Always parses an AST node. Unlike `try_parse` which bails and returns `None` if the
        /// first token does not match.
        ///
        /// The parsing behavior of a rule with a single expression is different from the parsing
        /// behavior of a rule with multiple expressions. For example, in the following `Foo`
        /// (which has one expression) and `Bar` (which has two expressions) have different
        /// parsing behaviors!
        ///
        /// ```txt
        /// Foo ::= {A B C}
        ///
        /// Bar ::= {D E F} | {G H I}
        /// ```
        ///
        /// For `Foo` we will parse each of its components `A`, `B`, and `C` with error recovery. If
        /// we can’t parse an `A` but we can parse a `B` then we will create an AST node with an
        /// error for `A` and a valid `B`.
        ///
        /// However, we can’t do the same for `Bar`. Our parser is an [LR(1)][1] parser so when we
        /// need to make a choice we can only look ahead one token. That means to parse `Bar` we
        /// _must_ see a `D` or a `G`. We can’t skip `D` and parse `E` since that means we’d have
        /// to commit to the first expression when parsing. So our parser for `Bar` will keep
        /// retrying until it sees a `D` or a `G`.
        ///
        /// [1]: https://en.wikipedia.org/wiki/LR_parser
        #[allow(dead_code)]
        pub(super) fn parse(context: &mut ParserContext) -> ast::$name {
            // Push all the parser recover functions in reverse order to our error recovery
            // stack. If we encounter an unrecognized token we will test each recovery
            // function to see if any future parsers will be able to handle the token. If no
            // future parser can handle the token we will skip the token.
            reverse_statements!($(context.recover_push(parser_symbol_recover_fn!($symbol_n))),*);
            // Parse all our symbols, including the first one. Make sure that before we parse a
            // symbol we pop its error recovery function from the stack.
            let data = (parser_symbol_parse!(context, $symbol_1), $({
                context.recover_pop();
                parser_symbol_parse!(context, $symbol_n)
            }),*);
            // After we’ve parsed all the data, create our AST node using our `ParserFrom` trait.
            // Which is like `From` but specific to our parser.
            ParserFrom::from(data)
        }

        /// We can recover from any of the symbols in our rule. Since when we parse we will skip
        /// over missing nodes.
        ///
        /// This is not the same for a rule with multiple expressions!
        #[allow(dead_code)]
        pub(super) fn recover(token: &Token) -> bool {
            parser_symbol_recover!(token, $symbol_1)
                $(|| parser_symbol_recover!(token, $symbol_n))*
        }
    };

    // If our rule has two or more expressions...
    (
        $name:ident ::= {$symbol_1_1:tt $($symbol_1_n:tt)*}
                      | $( {$symbol_n_1:tt $($symbol_n_n:tt)*} )|+
    ) => {
        /// Always parses an AST node. Unlike `try_parse` which bails and returns `None` if the
        /// first token does not match.
        ///
        /// To parse this grammar rule we keep retrying the `try_parse` function for the first
        /// symbol of each expression until one of the expressions match.
        #[allow(dead_code)]
        pub(super) fn parse(context: &mut ParserContext) -> Recover<ast::$name> {
            context.retry(ParserExpected::$name, $name::try_parse)
        }

        /// We can only recover if the _first_ symbol in one of our expressions can recover.
        ///
        /// This is not the same as the behavior for a rule with a single expression! A rule with a
        /// single expression may recover from any symbol in the expression. Not just the first
        /// symbol.
        ///
        /// Our multiple expression parser will not be able to skip the first token so we can’t
        /// recover at any token besides the first token.
        #[allow(dead_code)]
        pub(super) fn recover(token: &Token) -> bool {
            parser_symbol_recover!(token, $symbol_1_1)
                || $(parser_symbol_recover!(token, $symbol_n_1))||*
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
    ("true") => {
        Glyph::Keyword(Keyword::True)
    };
    ("false") => {
        Glyph::Keyword(Keyword::False)
    };
    ("let") => {
        Glyph::Keyword(Keyword::Let)
    };
    ("if") => {
        Glyph::Keyword(Keyword::If)
    };
    ("else") => {
        Glyph::Keyword(Keyword::Else)
    };
    ("do") => {
        Glyph::Keyword(Keyword::Do)
    };
    ("{") => {
        Glyph::BraceLeft
    };
    ("}") => {
        Glyph::BraceRight
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
macro_rules! parser_symbol_recover {
    ($token:expr, [opt: $symbol:tt]) => {
        parser_symbol_recover!($token, $symbol)
    };
    ($token:expr, [many: $item_symbol:tt, until: $until_symbol:tt]) => {
        parser_symbol_recover!($token, $item_symbol)
            || parser_symbol_recover!($token, $until_symbol)
    };
    ($token:expr, identifier) => {
        $token.is_identifier()
    };
    ($token:expr, number) => {
        $token.is_number()
    };
    ($token:expr, $name:ident) => {
        $name::recover($token)
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
macro_rules! parser_symbol_recover_fn {
    ([opt: $symbol:tt]) => {
        parser_symbol_recover_fn!($symbol)
    };
    ([many: $item_symbol:tt, until: $until_symbol:tt]) => {
        |token| parser_symbol_recover!(token, [many: $item_symbol, until: $until_symbol])
    };
    (identifier) => {
        Token::is_identifier
    };
    (number) => {
        Token::is_number
    };
    ($name:ident) => {
        $name::recover
    };
    ($glyph:tt) => {
        |token| token.is_glyph(parser_symbol_glyph!($glyph))
    };
}

/// Try to parse a symbol. If the symbol fails to parse and we _do not_ advance the lexer then we
/// return `None`. Otherwise we return `Some()`. Even if there was an error.
macro_rules! parser_symbol_try_parse {
    // Try to parse the optional symbol. Do nothing special.
    ($context:expr, [opt: $symbol:tt]) => {
        parser_symbol_try_parse!($context, $symbol)
    };

    // Lookahead and see if we could parse the many tokens symbol. If yes then invoke
    // `parser_symbol_parse!` for parsing.
    ($context:expr, [many: $item_symbol:tt, until: $until_symbol:tt]) => {
        if parser_symbol_test!(
            $context.lexer.lookahead(),
            [many: $item_symbol, until: $until_symbol]
        ) {
            Some(parser_symbol_parse!(
                $context,
                [many: $item_symbol, until: $until_symbol]
            ))
        } else {
            None
        }
    };

    // Only advance the lexer if the next token is an identifier.
    ($context:expr, identifier) => {
        $context.lexer.advance_identifier()
    };

    // Only advance the lexer if the next token is a number.
    ($context:expr, number) => {
        $context.lexer.advance_number()
    };

    // Use the non-terminal’s `try_parse` function.
    ($context:expr, $name:ident) => {
        $name::try_parse($context)
    };

    // Only advance the lexer if the next token is the specified glyph.
    ($context:expr, $glyph:tt) => {
        $context.lexer.advance_glyph(parser_symbol_glyph!($glyph))
    };
}

/// Parses a symbol. Usually by calling `ParserContext::retry` with the `try_parse` version of
/// the parser.
macro_rules! parser_symbol_parse {
    // To parse an optional symbol we instead use the `try_parse` version. We do not retry if it
    // returns `None`.
    ($context:expr, [opt: $symbol:tt]) => {
        parser_symbol_try_parse!($context, $symbol)
    };

    // Parses as many of a symbol as we can until we reach a specified ending symbol. The algorithm
    // (in words) goes something like this:
    //
    // 1. If we see the end symbol then stop looping.
    // 2. Parse an item and add it to our list.
    // 3. If the item we parsed erred fatally (the parser did not recover an item) then try to parse
    //    our end token and stop looping.
    // 4. Go back to 1.
    //
    // We expect both `$item_symbol` and `$until_symbol` to return `Recover<T>` when parsing.
    ($context:expr, [many: $item_symbol:tt, until: $until_symbol:tt]) => {{
        let mut items = Vec::new();
        // Parse all the items we can in a loop...
        let until: Recover<_> = loop {
            // If we see the “until” symbol then break.
            if let Some(until) = parser_symbol_try_parse!($context, $until_symbol) {
                break Ok(until);
            }
            // Parse an item. Add some recovery functions to the stack before doing so.
            $context.recover_push(parser_symbol_recover_fn!($item_symbol));
            $context.recover_push(parser_symbol_recover_fn!($until_symbol));
            let item: Recover<_> = parser_symbol_parse!($context, $item_symbol);
            $context.recover_pop();
            $context.recover_pop();
            match &item {
                // If the item erred fatally then parse the “until” symbol and break out
                // of the loop.
                Err(error) if error.recovered().is_none() => {
                    items.push(item);
                    break parser_symbol_parse!($context, $until_symbol);
                }
                // Push the item and carry on with the loop...
                _ => items.push(item),
            }
        };
        // Return the items list and the “until” value we parsed.
        (items, until)
    }};

    // Parse an identifier. Retry until we either find one or give up.
    ($context:expr, identifier) => {
        $context.retry(ParserExpected::Identifier, |context| {
            context.lexer.advance_identifier()
        })
    };

    // Parse a number. Retry until we either find one or give up.
    ($context:expr, number) => {
        $context.retry(ParserExpected::Number, |context| {
            context.lexer.advance_number()
        })
    };

    // Use the non-terminal’s parsing function to parse.
    ($context:expr, $name:ident) => {
        $name::parse($context)
    };

    // Parse a glyph. Retry until we either find one or give up.
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
    Statement ::= {Expression [opt: ";"]}
                | {"let" Pattern "=" Expression [opt: ";"]}
                | {";"}

    Block ::= {"{" [many: Statement, until: "}"]}

    Constant ::= {"true"}
               | {"false"}
               | {number}

    Expression ::= {identifier}
                 | {Constant}
                 | {"if" Expression Block [opt: ConditionalExpressionAlternate]}
                 | {"do" Block}
                 | {"(" Expression ")"}

    ConditionalExpressionAlternate ::= {"else" Block}

    Pattern ::= {identifier}
              | {"_"}
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

impl ParserFrom<BindingStatementData> for Statement {
    fn from((let_, pattern, equals, value, semicolon): BindingStatementData) -> Self {
        BindingStatement::new(let_, pattern, equals, value, semicolon).into()
    }
}

impl ParserFrom<(Expression, Option<GlyphToken>)> for Statement {
    fn from((expression, semicolon): (Expression, Option<GlyphToken>)) -> Self {
        ExpressionStatement::new(expression, semicolon).into()
    }
}

impl ParserFrom<(GlyphToken,)> for Statement {
    fn from((semicolon,): (GlyphToken,)) -> Self {
        EmptyStatement::new(semicolon).into()
    }
}

impl ParserFrom<(GlyphToken,)> for Constant {
    fn from((token,): (GlyphToken,)) -> Self {
        let value = token.glyph() == &Glyph::Keyword(Keyword::True);
        BooleanConstant::new(token, value).into()
    }
}

impl ParserFrom<(NumberToken,)> for Constant {
    fn from((number,): (NumberToken,)) -> Self {
        NumberConstant::new(number).into()
    }
}

type BlockData = (
    Recover<GlyphToken>,
    (Vec<Recover<Statement>>, Recover<GlyphToken>),
);

impl ParserFrom<BlockData> for Block {
    fn from((brace_left, (statements, brace_right)): BlockData) -> Self {
        Block::new(brace_left, statements, brace_right)
    }
}

type TryBlockData = (GlyphToken, (Vec<Recover<Statement>>, Recover<GlyphToken>));

impl ParserFrom<TryBlockData> for Block {
    fn from((brace_left, (statements, brace_right)): TryBlockData) -> Self {
        Block::new(Ok(brace_left), statements, brace_right)
    }
}

impl ParserFrom<(IdentifierToken,)> for Expression {
    fn from((identifier,): (IdentifierToken,)) -> Self {
        VariableExpression::new(identifier).into()
    }
}

impl ParserFrom<(GlyphToken, Block)> for Expression {
    fn from((do_, block): (GlyphToken, Block)) -> Self {
        BlockExpression::new(do_, block).into()
    }
}

type ConditionalExpressionData = (
    GlyphToken,
    Recover<Expression>,
    Block,
    Option<ConditionalExpressionAlternate>,
);

impl ParserFrom<ConditionalExpressionData> for Expression {
    fn from((if_, test, consequent, alternate): ConditionalExpressionData) -> Self {
        ConditionalExpression::new(if_, test, consequent, alternate).into()
    }
}

type WrappedExpressionData = (GlyphToken, Recover<Expression>, Recover<GlyphToken>);

impl ParserFrom<WrappedExpressionData> for Expression {
    fn from((paren_left, expression, paren_right): WrappedExpressionData) -> Self {
        WrappedExpression::new(paren_left, expression, paren_right).into()
    }
}

impl ParserFrom<(GlyphToken, Block)> for ConditionalExpressionAlternate {
    fn from((else_, block): (GlyphToken, Block)) -> Self {
        ConditionalExpressionAlternate::new(else_, block)
    }
}

impl ParserFrom<(Recover<GlyphToken>, Block)> for ConditionalExpressionAlternate {
    fn from(_: (Recover<GlyphToken>, Block)) -> Self {
        unimplemented!(
            "We don’t expect `ConditionalExpressionAlternate::parse()` to ever be called."
        )
    }
}

impl ParserFrom<(IdentifierToken,)> for Pattern {
    fn from((identifier,): (IdentifierToken,)) -> Self {
        VariablePattern::new(identifier).into()
    }
}

impl ParserFrom<(GlyphToken,)> for Pattern {
    fn from((hole,): (GlyphToken,)) -> Self {
        HolePattern::new(hole).into()
    }
}
