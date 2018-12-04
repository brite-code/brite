use super::ast::*;
use super::identifier::Keyword;
use super::lexer::Lexer;
use super::token::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef};
use std::marker::PhantomData;

pub fn parse_expression(lexer: Lexer) -> Recover<Expression> {
    let mut context = ParserContext::new(lexer);
    ExpressionParser::parse(&mut context)
}

type ExpressionParser = ChoiceParser5<
    VariableExpressionParser,
    NumberExpressionParser,
    TrueExpressionParser,
    FalseExpressionParser,
    WrappedExpressionParser,
>;

struct VariableExpressionParser;

impl TransformParser for VariableExpressionParser {
    type Parser = IdentifierParser;
    type Data = Expression;

    fn transform(identifier: Recover<IdentifierToken>) -> Expression {
        VariableExpression::new(identifier).into()
    }
}

struct NumberExpressionParser;

impl TransformParser for NumberExpressionParser {
    type Parser = NumberParser;
    type Data = Expression;

    fn transform(number: Recover<NumberToken>) -> Expression {
        NumberConstant::new(number).into()
    }
}

struct TrueExpressionParser;

impl TransformParser for TrueExpressionParser {
    type Parser = keyword_parser::True;
    type Data = Expression;

    fn transform(boolean: Recover<GlyphToken>) -> Expression {
        BooleanConstant::new(boolean, true).into()
    }
}

struct FalseExpressionParser;

impl TransformParser for FalseExpressionParser {
    type Parser = keyword_parser::False;
    type Data = Expression;

    fn transform(boolean: Recover<GlyphToken>) -> Expression {
        BooleanConstant::new(boolean, false).into()
    }
}

struct WrappedExpressionParser;

impl TransformParser for WrappedExpressionParser {
    type Parser = GroupParser3<glyph_parser::ParenLeft, ExpressionParser, glyph_parser::ParenRight>;
    type Data = Expression;

    fn transform(
        (paren_left, expression, paren_right): (
            Recover<GlyphToken>,
            Recover<Expression>,
            Recover<GlyphToken>,
        ),
    ) -> Expression {
        WrappedExpression::new(paren_left, expression, paren_right).into()
    }
}

/* ─── Framework ──────────────────────────────────────────────────────────────────────────────── */

trait Parser: Sized {
    type Data;

    fn test(token: &Token) -> bool;

    fn parse(context: &mut ParserContext) -> Self::Data;

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data>;
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

    /// Provides error recovery logic for a parsing operation which might fail. Takes a
    /// `try_parse` function which, like `Parser::try_parse`, consumes no input on failure and
    /// returns `None`. If a call to `try_parse` fails we will advance to the next token and
    /// try again. _Unless_ the next token can be handled by any parser in our stack. Then we
    /// will return a fatal error node and let the parent parser handle the token.
    fn retry<T>(&mut self, try_parse: impl Fn(&mut ParserContext) -> Option<T>) -> Recover<T> {
        // Try to parse our node. If successful return the node. If unsuccesful then we are in
        // error recovery mode!
        if let Some(x) = try_parse(self) {
            Recover::Ok(x)
        } else {
            self.recover(try_parse)
        }
    }

    /// Error recovery behavior for `ParserContext::retry`.
    fn recover<T>(&mut self, try_parse: impl Fn(&mut ParserContext) -> Option<T>) -> Recover<T> {
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

            // If we have reached the end token then we are _guaranteed_ to be unable to
            // parse our node. After all, there are no more tokens. Return a fatal error.
            if let Token::End(_) = token {
                // Always report an unexpected end token error.
                let token = token.clone();
                let diagnostic = self.unexpected_token(token);
                // But only attach the first diagnostic to the AST. This will be the
                // diagnostic thrown at runtime.
                let diagnostic = first_diagnostic.unwrap_or(diagnostic);
                let error = Error::new(skipped, diagnostic);
                // Return a fatal error.
                return Recover::FatalError(error);
            }

            // Call all the recovery functions in our stack. If one of the recovery functions
            // returns true then we know one of our parent parsing functions may continue from
            // this token! So we return a fatal error.
            //
            // If _all_ recovery functions return false then we know no one can handle this
            // token so we should skip it.
            for recover in self.recover.iter().rev() {
                if recover(token) {
                    // If we can recover from this token then presumably it is part of a valid
                    // AST node. So we only want to report an error if we have not found any
                    // other unexpected tokens. Meaning this token is certainly out of place.
                    let diagnostic = match first_diagnostic {
                        Some(diagnostic) => diagnostic,
                        None => {
                            let token = token.clone();
                            self.unexpected_token(token)
                        }
                    };
                    let error = Error::new(skipped, diagnostic);
                    return Recover::FatalError(error);
                }
            }

            // Advance the lexer and push the token to our `skipped` list.
            let token = self.lexer.advance();
            skipped.push(token.clone());

            // Report an unexpected token diagnostic for every token that we skip. Keep the
            // first skipped token diagnostic around. We’ll use this diagnostic in the AST to
            // cause runtime crashes.
            let diagnostic = self.unexpected_token(token);
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
                    let error = Error::new(skipped, first_diagnostic.unwrap());
                    return Recover::Error(error, x);
                }
                // If our parsing function failed, again, then do nothing and go back to the top
                // of our loop.
                None => {}
            }
        }
    }

    /// Report an unexpected token error. The diagnostics object is owned by our lexer.
    fn unexpected_token(&mut self, token: Token) -> DiagnosticRef {
        let range = token.full_range().range();
        let diagnostic = Diagnostic::unexpected_token(range, token);
        self.lexer.diagnostics.report(diagnostic)
    }
}

trait TransformParser {
    type Parser: Parser;
    type Data;

    fn transform(data: <Self::Parser as Parser>::Data) -> Self::Data;
}

impl<T: TransformParser> Parser for T {
    type Data = T::Data;

    fn test(token: &Token) -> bool {
        T::Parser::test(token)
    }

    fn parse(context: &mut ParserContext) -> Self::Data {
        T::transform(T::Parser::parse(context))
    }

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
        T::Parser::try_parse(context).map(T::transform)
    }
}

/* ─── Parsers ────────────────────────────────────────────────────────────────────────────────── */

struct IdentifierParser;

impl Parser for IdentifierParser {
    type Data = Recover<IdentifierToken>;

    fn test(token: &Token) -> bool {
        token.is_identifier()
    }

    fn parse(context: &mut ParserContext) -> Self::Data {
        context.retry(|context| context.lexer.advance_identifier())
    }

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
        context.lexer.advance_identifier().map(Recover::Ok)
    }
}

struct NumberParser;

impl Parser for NumberParser {
    type Data = Recover<NumberToken>;

    fn test(token: &Token) -> bool {
        token.is_number()
    }

    fn parse(context: &mut ParserContext) -> Self::Data {
        context.retry(|context| context.lexer.advance_number())
    }

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
        context.lexer.advance_number().map(Recover::Ok)
    }
}

mod glyph_parser {
    use super::*;

    macro_rules! glyph {
        ($glyph:ident) => {
            pub struct $glyph;

            impl Parser for $glyph {
                type Data = Recover<GlyphToken>;

                fn test(token: &Token) -> bool {
                    token.is_glyph(Glyph::$glyph)
                }

                fn parse(context: &mut ParserContext) -> Self::Data {
                    context.retry(|context| context.lexer.advance_glyph(Glyph::$glyph))
                }

                fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                    context.lexer.advance_glyph(Glyph::$glyph).map(Recover::Ok)
                }
            }
        };
    }

    glyph!(ParenLeft);
    glyph!(ParenRight);
}

mod keyword_parser {
    use super::*;

    macro_rules! keyword {
        ($keyword:ident) => {
            pub struct $keyword;

            impl Parser for $keyword {
                type Data = Recover<GlyphToken>;

                fn test(token: &Token) -> bool {
                    token.is_glyph(Glyph::Keyword(Keyword::$keyword))
                }

                fn parse(context: &mut ParserContext) -> Self::Data {
                    context.retry(|context| {
                        context
                            .lexer
                            .advance_glyph(Glyph::Keyword(Keyword::$keyword))
                    })
                }

                fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                    context
                        .lexer
                        .advance_glyph(Glyph::Keyword(Keyword::$keyword))
                        .map(Recover::Ok)
                }
            }
        };
    }

    keyword!(True);
    keyword!(False);
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

macro_rules! group {
    (struct $name:ident<P1, $($t:ident),+>) => {
        struct $name<P1: Parser, $($t: Parser),*>(PhantomData<(P1, $($t),*)>);

        impl<P1: Parser, $($t: Parser),*> Parser for $name<P1, $($t),*> {
            type Data = (P1::Data, $($t::Data),*);

            fn test(token: &Token) -> bool {
                P1::test(token)
            }

            #[allow(non_snake_case)]
            fn parse(context: &mut ParserContext) -> Self::Data {
                reverse_statements!($(context.recover_push($t::test)),*);
                let P1 = P1::parse(context);
                $(context.recover_pop();
                let $t = $t::parse(context);)*
                (P1, $($t),*)
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
    (struct $name:ident<P1, $($t:ident),+>) => {
        struct $name<P1: Parser, $($t: Parser<Data = P1::Data>),*>(PhantomData<(P1, $($t),*)>);

        impl<P1: Parser, $($t: Parser<Data = P1::Data>),*> Parser for $name<P1, $($t),*> {
            type Data = Recover<P1::Data>;

            fn test(token: &Token) -> bool {
                P1::test(token) || $($t::test(token))||*
            }

            fn parse(context: &mut ParserContext) -> Self::Data {
                context.retry(|context| {
                    if let x @ Some(_) = P1::try_parse(context) {
                        return x;
                    }
                    $(if let x @ Some(_) = $t::try_parse(context) {
                        return x;
                    })*
                    None
                })
            }

            fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                if let Some(x) = P1::try_parse(context) {
                    return Some(Recover::Ok(x));
                }
                $(if let Some(x) = $t::try_parse(context) {
                    return Some(Recover::Ok(x));
                })*
                None
            }
        }
    };
}

group!(struct GroupParser3<P1, P2, P3>);

choice!(struct ChoiceParser5<P1, P2, P3, P4, P5>);
