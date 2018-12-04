use super::ast::*;
use super::identifier::Keyword;
use super::lexer::Lexer;
use super::token::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef};
use std::marker::PhantomData;

/// Parses a complete Brite module AST out of a stream of lexical tokens.
///
/// Implements the ability to recover from parse errors.
pub fn parse(lexer: Lexer) -> Module {
    let mut context = ParserContext::new(lexer);
    let expression = ExpressionParser::parse(&mut context);
    let statement: Statement = ExpressionStatement::new(expression.unwrap(), None).into();
    let end = loop {
        match context.lexer.advance() {
            Token::End(end) => break end,
            _ => {}
        }
    };
    Module::new(vec![statement.into()], end)
}

type StatementParser = ChoiceParser2<ExpressionStatementParser, BindingStatementParser>;

/// ```ite
/// E;
/// ```
struct ExpressionStatementParser;

impl TransformParser for ExpressionStatementParser {
    type Parser = OptionalSemicolonParser<ExpressionParser>;
    type Data = Statement;

    fn transform((expression, semicolon): (Expression, Option<GlyphToken>)) -> Statement {
        ExpressionStatement::new(expression, semicolon).into()
    }
}

/// ```ite
/// let x = E;
/// ```
struct BindingStatementParser;

impl TransformParser for BindingStatementParser {
    type Parser = OptionalSemicolonParser<
        GroupParser4<keyword_parser::Let, PatternParser, glyph_parser::Equals, ExpressionParser>,
    >;
    type Data = Statement;

    fn transform(
        ((_let, pattern, equals, expression), semicolon): (
            (
                GlyphToken,
                Recover<Pattern>,
                Recover<GlyphToken>,
                Recover<Expression>,
            ),
            Option<GlyphToken>,
        ),
    ) -> Statement {
        BindingStatement::new(_let, pattern, equals, expression, semicolon).into()
    }
}

/// ```ite
/// true
/// ```
struct TrueConstantParser;

impl TransformParser for TrueConstantParser {
    type Parser = keyword_parser::True;
    type Data = Constant;

    fn transform(boolean: GlyphToken) -> Constant {
        BooleanConstant::new(boolean, true).into()
    }
}

/// ```ite
/// false
/// ```
struct FalseConstantParser;

impl TransformParser for FalseConstantParser {
    type Parser = keyword_parser::False;
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

impl TransformParser for NumberConstantParser {
    type Parser = NumberParser;
    type Data = Constant;

    fn transform(number: NumberToken) -> Constant {
        NumberConstant::new(number).into()
    }
}

type ExpressionParser = ChoiceParser5<
    VariableExpressionParser,
    IntoParser<NumberConstantParser, Expression>,
    IntoParser<TrueConstantParser, Expression>,
    IntoParser<FalseConstantParser, Expression>,
    WrappedExpressionParser,
>;

/// ```ite
/// x
/// ```
struct VariableExpressionParser;

impl TransformParser for VariableExpressionParser {
    type Parser = IdentifierParser;
    type Data = Expression;

    fn transform(identifier: IdentifierToken) -> Expression {
        VariableExpression::new(identifier).into()
    }
}

/// ```ite
/// (E)
/// ```
struct WrappedExpressionParser;

impl TransformParser for WrappedExpressionParser {
    type Parser = GroupParser3<glyph_parser::ParenLeft, ExpressionParser, glyph_parser::ParenRight>;
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

type PatternParser = ChoiceParser2<HolePatternParser, VariablePatternParser>;

/// ```ite
/// _
/// ```
struct HolePatternParser;

impl TransformParser for HolePatternParser {
    type Parser = glyph_parser::Underscore;
    type Data = Pattern;

    fn transform(hole: GlyphToken) -> Pattern {
        HolePattern::new(hole).into()
    }
}

/// ```ite
/// x
/// ```
struct VariablePatternParser;

impl TransformParser for VariablePatternParser {
    type Parser = IdentifierParser;
    type Data = Pattern;

    fn transform(identifier: IdentifierToken) -> Pattern {
        VariablePattern::new(identifier).into()
    }
}

/* ─── Parsing Framework ──────────────────────────────────────────────────────────────────────── */

trait Parser: Sized {
    type Data;

    fn test(token: &Token) -> bool;

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data>;

    fn parse(context: &mut ParserContext) -> Recover<Self::Data> {
        // Try to parse our node. If successful return the node. If unsuccesful then we are in
        // error recovery mode!
        if let Some(x) = Self::try_parse(context) {
            Ok(x)
        } else {
            context.recover(Self::try_parse)
        }
    }
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
    /// `Parser::try_parse` function which consumes no input on failure and returns `None`. If a
    /// call to `try_parse` fails we will advance to the next token and try again. _Unless_ the next
    /// token can be handled by any parser in our stack. Then we will return a fatal error node and
    /// let the parent parser handle the token.
    fn recover<T>(&mut self, try_parse: fn(&mut ParserContext) -> Option<T>) -> Recover<T> {
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
                let error = RecoverError::new(skipped, diagnostic, None);
                return Err(Box::new(error));
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
                    let error = RecoverError::new(skipped, diagnostic, None);
                    return Err(Box::new(error));
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
    fn unexpected_token(&mut self, token: Token) -> DiagnosticRef {
        let range = token.full_range().range();
        let diagnostic = Diagnostic::unexpected_token(range, token);
        self.lexer.diagnostics.report(diagnostic)
    }
}

/* ─── Primitive Parsers ──────────────────────────────────────────────────────────────────────── */

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

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
        T::Parser::try_parse(context).map(T::transform)
    }
}

struct IntoParser<P, T>
where
    P: Parser,
    P::Data: Into<T>,
{
    phantom: PhantomData<(P, T)>,
}

impl<P, T> TransformParser for IntoParser<P, T>
where
    P: Parser,
    P::Data: Into<T>,
{
    type Parser = P;
    type Data = T;

    fn transform(data: P::Data) -> T {
        data.into()
    }
}

struct IdentifierParser;

impl Parser for IdentifierParser {
    type Data = IdentifierToken;

    fn test(token: &Token) -> bool {
        token.is_identifier()
    }

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
        context.lexer.advance_identifier()
    }
}

struct NumberParser;

impl Parser for NumberParser {
    type Data = NumberToken;

    fn test(token: &Token) -> bool {
        token.is_number()
    }

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
        context.lexer.advance_number()
    }
}

mod glyph_parser {
    use super::*;

    macro_rules! glyph {
        ($glyph:ident) => {
            pub struct $glyph;

            impl Parser for $glyph {
                type Data = GlyphToken;

                fn test(token: &Token) -> bool {
                    token.is_glyph(Glyph::$glyph)
                }

                fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                    context.lexer.advance_glyph(Glyph::$glyph)
                }
            }
        };
    }

    glyph!(Equals);
    glyph!(ParenLeft);
    glyph!(ParenRight);
    glyph!(Semicolon);
    glyph!(Underscore);
}

mod keyword_parser {
    use super::*;

    macro_rules! keyword {
        ($keyword:ident) => {
            pub struct $keyword;

            impl Parser for $keyword {
                type Data = GlyphToken;

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

    keyword!(True);
    keyword!(False);
    keyword!(Let);
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
            type Data = (P1::Data, $(Recover<$t::Data>),*);

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
    (struct $name:ident<P1, $($t:ident),+>) => {
        struct $name<P1: Parser, $($t: Parser<Data = P1::Data>),*>(PhantomData<(P1, $($t),*)>);

        impl<P1: Parser, $($t: Parser<Data = P1::Data>),*> Parser for $name<P1, $($t),*> {
            type Data = P1::Data;

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

group!(struct GroupParser2<P1, P2>);
group!(struct GroupParser3<P1, P2, P3>);
group!(struct GroupParser4<P1, P2, P3, P4>);

choice!(struct ChoiceParser2<P1, P2>);
choice!(struct ChoiceParser5<P1, P2, P3, P4, P5>);

struct OptionalSemicolonParser<P: Parser>(PhantomData<P>);

impl<P: Parser> Parser for OptionalSemicolonParser<P> {
    type Data = (P::Data, Option<GlyphToken>);

    fn test(token: &Token) -> bool {
        P::test(token)
    }

    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
        let item = P::try_parse(context)?;
        let semicolon = context.lexer.advance_glyph(Glyph::Semicolon);
        Some((item, semicolon))
    }
}
