use super::ast::*;
use super::lexer::Lexer;
use super::token::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef, ParserExpected};

/// Parses a complete Brite module AST out of a stream of lexical tokens. Reports all diagnostics
/// discovered while parsing to the `DiagnosticSet` returned by this function.
///
/// Implements the ability to recover from parse errors.
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

macro_rules! parser {
    (
        $(
            $name:ident ::= $( {$( $symbol:tt )+} )|+
        )*
    ) => {
        mod p {
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

macro_rules! parser_rule {
    // Optimization for `BooleanConstant`. We need this because otherwise it would be difficult to
    // correctly initialize the true/false value.
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
        pub(super) struct $name;

        impl $name {
            #[allow(dead_code)]
            fn test(token: &Token) -> bool {
                $(parser_symbol_test!(token, $symbol_1))||*
            }

            fn try_parse(context: &mut ParserContext) -> Option<ast::$name> {
                $(
                    reverse_statements!($(context.recover_push(parser_symbol_test_fn!($symbol_n))),*);
                    if let Some(x1) = parser_symbol_try_parse!(context, $symbol_1) {
                        let data = (x1, $({
                            context.recover_pop();
                            parser_symbol_parse!(context, $symbol_n)
                        }),*);
                        return Some(ParserFrom::from(data));
                    } else {
                        $(ignore!($symbol_n); context.recover_pop();)*
                    }
                )*
                None
            }

            parser_rule_parse_fn_def!($name ::= $({$symbol_1 $($symbol_n)*})|*);
        }
    };
}

macro_rules! parser_rule_parse_fn_def {
    (
        $name:ident ::= {$symbol_1:tt $($symbol_n:tt)*}
    ) => {
        // Unimplemented...
    };
    (
        $name:ident ::= {$symbol_1_1:tt $($symbol_1_n:tt)*}
                      | $( {$symbol_n_1:tt $($symbol_n_n:tt)*} )|+
    ) => {
        pub(super) fn parse(context: &mut ParserContext) -> Recover<ast::$name> {
            context.retry(ParserExpected::$name, $name::try_parse)
        }
    };
}

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

macro_rules! ignore {
    ($x:tt) => {};
}

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

struct ParserContext<'a> {
    lexer: Lexer<'a>,
    recover: Vec<fn(&Token) -> bool>,
}

impl<'a> ParserContext<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        ParserContext {
            lexer,
            recover: Vec::new(),
        }
    }

    fn recover_push(&mut self, f: fn(&Token) -> bool) {
        self.recover.push(f);
    }

    fn recover_pop(&mut self) {
        self.recover.pop();
    }

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

    /// Provides error recovery logic for a parsing operation which might fail. Takes a
    /// `Parser::try_parse` function which consumes no input on failure and returns `None`. If a
    /// call to `try_parse` fails we will advance to the next token and try again. _Unless_ the
    /// next token can be handled by any parser in our stack. Then we will return a fatal error
    /// node and let the parent parser handle the token.
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

trait ParserFrom<T> {
    fn from(data: T) -> Self;
}

impl<T, U: Into<T>> ParserFrom<(U,)> for T {
    fn from((data,): (U,)) -> T {
        data.into()
    }
}

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
