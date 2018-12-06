use self::p::{Parser, ParserContext, Provide};
use super::ast::*;
use super::lexer::Lexer;
use super::token::*;
use crate::diagnostics::{DiagnosticSet, ParserExpected};

/// Parses a complete Brite module AST out of a stream of lexical tokens. Reports all diagnostics
/// discovered while parsing to the `DiagnosticSet` returned by this function.
///
/// Implements the ability to recover from parse errors.
pub fn parse(lexer: Lexer) -> (DiagnosticSet, Module) {
    // Create our parsing context.
    let mut context = ParserContext::new(lexer);
    // Until we reach the end token, parse items. If an item is in recovery mode it will stop trying
    // to recover once it reaches the end token.
    let mut items = Vec::new();
    while !context.lexer.lookahead_end() {
        items.push(StatementParser::parse(&mut context));
    }
    // Optimization: We are done mutating our vector. Shrink it to the smallest size.
    items.shrink_to_fit();
    // We expect that advancing the lexer will give us our `EndToken`. Since we should only exit our
    // above while-loop when we have reached the end.
    let end = context.lexer.advance_end().unwrap();
    // Construct the module and return it.
    let module = Module::new(items, end);
    let diagnostics = context.diagnostics;
    (diagnostics, module)
}

type StatementParser =
    p::Choice2<ExpectedStatement, ExpressionStatementParser, BindingStatementParser>;

struct ExpectedStatement;

impl Provide<ParserExpected> for ExpectedStatement {
    fn get() -> ParserExpected {
        ParserExpected::Statement
    }
}

/// ```ite
/// E;
/// ```
struct ExpressionStatementParser;

impl p::Transform for ExpressionStatementParser {
    type Parser = p::Group2<ExpressionParser, p::Optional<p::Semicolon>>;
    type Data = Statement;

    fn transform((expression, semicolon): (Expression, Recover<Option<GlyphToken>>)) -> Statement {
        ExpressionStatement::new(expression, semicolon.unwrap()).into()
    }
}

/// ```ite
/// let x = E;
/// ```
struct BindingStatementParser;

impl p::Transform for BindingStatementParser {
    type Parser =
        p::Group5<p::Let, PatternParser, p::Equals, ExpressionParser, p::Optional<p::Semicolon>>;
    type Data = Statement;

    fn transform(
        (_let, pattern, equals, expression, semicolon): (
            GlyphToken,
            Recover<Pattern>,
            Recover<GlyphToken>,
            Recover<Expression>,
            Recover<Option<GlyphToken>>,
        ),
    ) -> Statement {
        BindingStatement::new(_let, pattern, equals, expression, semicolon.unwrap()).into()
    }
}

/// ```ite
/// true
/// ```
struct TrueConstantParser;

impl p::Transform for TrueConstantParser {
    type Parser = p::True;
    type Data = Constant;

    fn transform(boolean: GlyphToken) -> Constant {
        BooleanConstant::new(boolean, true).into()
    }
}

/// ```ite
/// false
/// ```
struct FalseConstantParser;

impl p::Transform for FalseConstantParser {
    type Parser = p::False;
    type Data = Constant;

    fn transform(boolean: GlyphToken) -> Constant {
        BooleanConstant::new(boolean, false).into()
    }
}

/// ```ite
/// 0
/// 1
/// 42
/// 3.1415
/// ```
struct NumberConstantParser;

impl p::Transform for NumberConstantParser {
    type Parser = p::Number;
    type Data = Constant;

    fn transform(number: NumberToken) -> Constant {
        NumberConstant::new(number).into()
    }
}

type ExpressionParser = p::Choice5<
    ExpectedExpression,
    VariableExpressionParser,
    p::Into<NumberConstantParser, Expression>,
    p::Into<TrueConstantParser, Expression>,
    p::Into<FalseConstantParser, Expression>,
    WrappedExpressionParser,
>;

struct ExpectedExpression;

impl Provide<ParserExpected> for ExpectedExpression {
    fn get() -> ParserExpected {
        ParserExpected::Expression
    }
}

/// ```ite
/// x
/// ```
struct VariableExpressionParser;

impl p::Transform for VariableExpressionParser {
    type Parser = p::Identifier;
    type Data = Expression;

    fn transform(identifier: IdentifierToken) -> Expression {
        VariableExpression::new(identifier).into()
    }
}

/// ```ite
/// (E)
/// ```
struct WrappedExpressionParser;

impl p::Transform for WrappedExpressionParser {
    type Parser = p::Group3<p::ParenLeft, ExpressionParser, p::ParenRight>;
    type Data = Expression;

    fn transform(
        (paren_left, expression, paren_right): (
            GlyphToken,
            Recover<Expression>,
            Recover<GlyphToken>,
        ),
    ) -> Expression {
        WrappedExpression::new(paren_left, expression, paren_right).into()
    }
}

type PatternParser = p::Choice2<ExpectedPattern, HolePatternParser, VariablePatternParser>;

struct ExpectedPattern;

impl Provide<ParserExpected> for ExpectedPattern {
    fn get() -> ParserExpected {
        ParserExpected::Pattern
    }
}

/// ```ite
/// _
/// ```
struct HolePatternParser;

impl p::Transform for HolePatternParser {
    type Parser = p::Hole;
    type Data = Pattern;

    fn transform(hole: GlyphToken) -> Pattern {
        HolePattern::new(hole).into()
    }
}

/// ```ite
/// x
/// ```
struct VariablePatternParser;

impl p::Transform for VariablePatternParser {
    type Parser = p::Identifier;
    type Data = Pattern;

    fn transform(identifier: IdentifierToken) -> Pattern {
        VariablePattern::new(identifier).into()
    }
}

mod p {
    use super::super::ast::{Recover, RecoverError};
    use super::super::identifier::Keyword;
    use super::super::lexer::Lexer;
    use super::super::token::*;
    use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticSet, ParserExpected};
    use std::convert;
    use std::marker::PhantomData;

    pub trait Parser: Sized {
        type Data;

        fn expected() -> ParserExpected;

        fn test(token: &Token) -> bool;

        fn try_parse(context: &mut ParserContext) -> Option<Self::Data>;

        fn parse(context: &mut ParserContext) -> Recover<Self::Data> {
            // Try to parse our node. If successful return the node. If unsuccesful then we are in
            // error recovery mode!
            if let Some(x) = Self::try_parse(context) {
                Ok(x)
            } else {
                context.recover(Self::expected(), Self::try_parse)
            }
        }
    }

    pub struct ParserContext<'a> {
        pub lexer: Lexer<'a>,
        pub diagnostics: DiagnosticSet,
        recover: Vec<fn(&Token) -> bool>,
    }

    impl<'a> ParserContext<'a> {
        pub fn new(lexer: Lexer<'a>) -> Self {
            ParserContext {
                lexer,
                diagnostics: DiagnosticSet::new(),
                recover: Vec::new(),
            }
        }

        fn recover_push(&mut self, f: fn(&Token) -> bool) {
            self.recover.push(f);
        }

        fn recover_pop(&mut self) {
            self.recover.pop();
        }

        /// Provides error recovery logic for a parsing operation which might fail. Takes a
        /// `Parser::try_parse` function which consumes no input on failure and returns `None`. If a
        /// call to `try_parse` fails we will advance to the next token and try again. _Unless_ the
        /// next token can be handled by any parser in our stack. Then we will return a fatal error
        /// node and let the parent parser handle the token.
        fn recover<T>(
            &mut self,
            expected: ParserExpected,
            try_parse: fn(&mut ParserContext) -> Option<T>,
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
        fn unexpected_token(
            &mut self,
            unexpected: Token,
            expected: ParserExpected,
        ) -> DiagnosticRef {
            let range = unexpected.full_range().range();
            let diagnostic = Diagnostic::unexpected_token(range, unexpected, expected);
            self.diagnostics.report(diagnostic)
        }
    }

    pub trait Provide<T> {
        fn get() -> T;
    }

    /// NOTE: Ideally, `Optional<P>::parse` would return `Option<P::Data>` instead of
    /// `Recover<Option<P::Data>>`, but its good enough for now.
    pub struct Optional<P: Parser> {
        phantom: PhantomData<P>,
    }

    impl<P: Parser> Parser for Optional<P> {
        type Data = Option<P::Data>;

        fn expected() -> ParserExpected {
            P::expected()
        }

        fn test(token: &Token) -> bool {
            P::test(token)
        }

        fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
            P::try_parse(context).map(Some)
        }

        fn parse(context: &mut ParserContext) -> Recover<Self::Data> {
            Ok(P::try_parse(context))
        }
    }

    /// NOTE: Ideally, `Many<P>::parse` would return `Vec<P::Data>` instead of
    /// `Recover<Vec<P::Data>>`, but its good enough for now.
    pub struct Many<P: Parser, Q: Parser> {
        phantom: PhantomData<(P, Q)>,
    }

    impl<P: Parser, Q: Parser> Parser for Many<P, Q> {
        type Data = Vec<Recover<P::Data>>;

        fn expected() -> ParserExpected {
            P::expected()
        }

        fn test(token: &Token) -> bool {
            P::test(token)
        }

        fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
            let mut items = Vec::new();
            while !Q::test(context.lexer.lookahead()) {
                items.push(P::parse(context));
            }
            items.shrink_to_fit();
            if items.len() == 0 {
                None
            } else {
                Some(items)
            }
        }

        fn parse(context: &mut ParserContext) -> Recover<Self::Data> {
            let mut items = Vec::new();
            while !Q::test(context.lexer.lookahead()) {
                items.push(P::parse(context));
            }
            items.shrink_to_fit();
            Ok(items)
        }
    }

    pub trait Transform {
        type Parser: Parser;
        type Data;

        fn transform(data: <Self::Parser as Parser>::Data) -> Self::Data;
    }

    impl<T: Transform> Parser for T {
        type Data = T::Data;

        fn expected() -> ParserExpected {
            T::Parser::expected()
        }

        fn test(token: &Token) -> bool {
            T::Parser::test(token)
        }

        fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
            T::Parser::try_parse(context).map(T::transform)
        }
    }

    pub struct Into<P, T>
    where
        P: Parser,
        P::Data: convert::Into<T>,
    {
        phantom: PhantomData<(P, T)>,
    }

    impl<P, T> Transform for Into<P, T>
    where
        P: Parser,
        P::Data: convert::Into<T>,
    {
        type Parser = P;
        type Data = T;

        fn transform(data: P::Data) -> T {
            data.into()
        }
    }

    pub struct Identifier;

    impl Parser for Identifier {
        type Data = IdentifierToken;

        fn expected() -> ParserExpected {
            ParserExpected::Identifier
        }

        fn test(token: &Token) -> bool {
            token.is_identifier()
        }

        fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
            context.lexer.advance_identifier()
        }
    }

    pub struct Number;

    impl Parser for Number {
        type Data = NumberToken;

        fn expected() -> ParserExpected {
            ParserExpected::Number
        }

        fn test(token: &Token) -> bool {
            token.is_number()
        }

        fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
            context.lexer.advance_number()
        }
    }

    macro_rules! glyph {
        ($glyph:ident) => {
            pub struct $glyph;

            impl Parser for $glyph {
                type Data = GlyphToken;

                fn expected() -> ParserExpected {
                    ParserExpected::Glyph(Glyph::$glyph)
                }

                fn test(token: &Token) -> bool {
                    token.is_glyph(Glyph::$glyph)
                }

                fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                    context.lexer.advance_glyph(Glyph::$glyph)
                }
            }
        };
    }

    macro_rules! keyword {
        ($keyword:ident) => {
            pub struct $keyword;

            impl Parser for $keyword {
                type Data = GlyphToken;

                fn expected() -> ParserExpected {
                    ParserExpected::Glyph(Glyph::Keyword(Keyword::$keyword))
                }

                fn test(token: &Token) -> bool {
                    token.is_glyph(Glyph::Keyword(Keyword::$keyword))
                }

                fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                    context
                        .lexer
                        .advance_glyph(Glyph::Keyword(Keyword::$keyword))
                }
            }
        };
    }

    glyph!(Equals);
    glyph!(ParenLeft);
    glyph!(ParenRight);
    glyph!(Semicolon);

    keyword!(Hole);
    keyword!(True);
    keyword!(False);
    keyword!(Let);

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

    macro_rules! group {
        (pub struct $name:ident<P1, $($t:ident),+>) => {
            pub struct $name<P1: Parser, $($t: Parser),*>(PhantomData<(P1, $($t),*)>);

            impl<P1: Parser, $($t: Parser),*> Parser for $name<P1, $($t),*> {
                type Data = (P1::Data, $(Recover<$t::Data>),*);

                fn expected() -> ParserExpected {
                    P1::expected()
                }

                fn test(token: &Token) -> bool {
                    P1::test(token)
                }

                #[allow(non_snake_case)]
                fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                    reverse_statements!($(context.recover_push($t::test)),*);
                    let P1 = match P1::try_parse(context) {
                        Some(P1) => P1,
                        None => {
                            $(ignore!($t); context.recover_pop();)*
                            return None;
                        }
                    };
                    $(context.recover_pop();
                    let $t = $t::parse(context);)*
                    Some((P1, $($t),*))

                }
            }
        };
    }

    macro_rules! choice {
        (pub struct $name:ident<P1, $($t:ident),+>) => {
            pub struct $name<E, P1, $($t),*>
            where
                E: Provide<ParserExpected>,
                P1: Parser,
                $($t: Parser<Data = P1::Data>),*
            {
                phantom: PhantomData<(E, P1, $($t),*)>,
            }

            impl<E, P1, $($t),*> Parser for $name<E, P1, $($t),*>
            where
                E: Provide<ParserExpected>,
                P1: Parser,
                $($t: Parser<Data = P1::Data>),*
            {
                type Data = P1::Data;

                fn expected() -> ParserExpected {
                    E::get()
                }

                fn test(token: &Token) -> bool {
                    P1::test(token) || $($t::test(token))||*
                }

                fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                    if let x @ Some(_) = P1::try_parse(context) {
                        return x;
                    }
                    $(if let x @ Some(_) = $t::try_parse(context) {
                        return x;
                    })*
                    None
                }
            }
        };
    }

    group!(pub struct Group2<P1, P2>);
    group!(pub struct Group3<P1, P2, P3>);
    group!(pub struct Group5<P1, P2, P3, P4, P5>);

    choice!(pub struct Choice2<P1, P2>);
    choice!(pub struct Choice5<P1, P2, P3, P4, P5>);
}
