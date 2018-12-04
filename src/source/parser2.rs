use self::p::Parser;
use super::ast::*;
use super::identifier::Keyword;
use super::lexer::Lexer;
use super::token::*;

fn expression() -> impl Parser<Data = Recover<IdentifierToken>> {
    p::identifier()

    // p::choose5(
    //     // Parse `VariableExpression`.
    //     p::identifier().map(|token| Expression::Variable(VariableExpression::new(token))),
    //     // Parse `NumberExpression`.
    //     p::number().map(|token| NumberConstant::new(token).into()),
    //     // Parse true `BooleanExpression`.
    //     p::keyword(Keyword::True).map(|token| BooleanConstant::new(token, true).into()),
    //     // Parse false `BooleanExpression`.
    //     p::keyword(Keyword::False).map(|token| BooleanConstant::new(token, false).into()),
    //     // Parse `WrappedExpression`.
    //     p::group3(
    //         p::glyph(Glyph::ParenLeft),
    //         parse_expression,
    //         p::glyph(Glyph::ParenRight),
    //     ).map(|(paren_left, expression, paren_right)| {
    //         WrappedExpression::new(paren_left, expression, paren_right).into()
    //     }),
    // )
}

mod p {
    use super::super::ast::{Error, Recover};
    use super::super::lexer::Lexer;
    use super::super::token::*;
    use crate::diagnostics::{Diagnostic, DiagnosticRef};
    use std::marker::PhantomData;
    use std::mem;

    pub trait Parser: Sized {
        type Data;

        fn test(token: &Token) -> bool;

        fn parse(context: &mut ParserContext) -> Self::Data;

        fn try_parse(context: &mut ParserContext) -> Option<Self::Data>;

        fn map<F>(self, _f: F) -> MapParser<Self, F>
        where
            F: Function<Parameter = Self::Data>,
        {
            MapParser(PhantomData)
        }
    }

    pub struct ParserContext<'a> {
        lexer: Lexer<'a>,
        recover: Vec<fn(&Token) -> bool>,
    }

    impl<'a> ParserContext<'a> {
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
        fn recover<T>(
            &mut self,
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

    /// Type-level function.
    ///
    /// TODO: Document!
    pub trait Function {
        type Parameter;
        type Return;

        fn call(a: Self::Parameter) -> Self::Return;
    }

    pub struct MapParser<P, F>(PhantomData<(P, F)>);

    impl<P, F> Parser for MapParser<P, F>
    where
        P: Parser,
        F: Function<Parameter = P::Data>,
    {
        type Data = F::Return;

        fn test(token: &Token) -> bool {
            P::test(token)
        }

        fn parse(context: &mut ParserContext) -> Self::Data {
            F::call(P::parse(context))
        }

        fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
            P::try_parse(context).map(F::call)
        }
    }

    pub fn identifier() -> impl Parser<Data = Recover<IdentifierToken>> {
        IdentifierParser
    }

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

    pub fn number() -> impl Parser<Data = Recover<NumberToken>> {
        NumberParser
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
        (pub fn $name:ident(p1: P1, $($x:ident: $t:ident),+)) => {
            pub fn $name<P1: Parser, $($t: Parser),*>(p1: P1, $($x: $t),*) -> impl Parser<Data = (P1::Data, $($t::Data),*)> {
                #[allow(dead_code)]
                struct GroupParser<P1, $($t),*> {
                    p1: P1,
                    $($x: $t),*
                }

                impl<P1: Parser, $($t: Parser),*> Parser for GroupParser<P1, $($t),*> {
                    type Data = (P1::Data, $($t::Data),*);

                    fn test(token: &Token) -> bool {
                        P1::test(token)
                    }

                    fn parse(context: &mut ParserContext) -> Self::Data {
                        reverse_statements!($(context.recover_push($t::test)),*);
                        let p1 = P1::parse(context);
                        $(context.recover_pop();
                        let $x = $t::parse(context);)*
                        (p1, $($x),*)
                    }

                    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                        reverse_statements!($(context.recover_push($t::test)),*);
                        let p1 = match P1::try_parse(context) {
                            Some(p1) => p1,
                            None => {
                                $(ignore!($t); context.recover_pop();)*
                                return None;
                            }
                        };
                        $(context.recover_pop();
                        let $x = $t::parse(context);)*
                        Some((p1, $($x),*))

                    }
                }

                let parser = GroupParser { p1, $($x),* };
                debug_assert_eq!(
                    mem::size_of_val(&parser), 0,
                    "We expect all parsers to be zero sized. This makes them compile-time only."
                );
                parser
            }
        };
    }

    macro_rules! choose {
        (pub fn $name:ident($($x:ident: $t:ident),+)) => {
            pub fn $name<T, $($t: Parser<Data = T>),*>($($x: $t),*) -> impl Parser<Data = Recover<T>> {
                #[allow(dead_code)]
                pub struct ChooseParser<$($t),*>{
                    $($x: $t),*
                }

                impl<T, $($t: Parser<Data = T>),*> Parser for ChooseParser<$($t),*> {
                    type Data = Recover<T>;

                    fn test(token: &Token) -> bool {
                        $($t::test(token))||*
                    }

                    fn parse(context: &mut ParserContext) -> Self::Data {
                        context.retry(|context| {
                            $(if let $x @ Some(_) = $t::try_parse(context) {
                                return $x;
                            })*
                            None
                        })
                    }

                    fn try_parse(context: &mut ParserContext) -> Option<Self::Data> {
                        $(if let Some($x) = $t::try_parse(context) {
                            return Some(Recover::Ok($x));
                        })*
                        None
                    }
                }

                let parser = ChooseParser { $($x),* };
                debug_assert_eq!(
                    mem::size_of_val(&parser), 0,
                    "We expect all parsers to be zero sized. This makes them compile-time only."
                );
                parser
            }
        };
    }

    group!(pub fn group2(p1: P1, p2: P2));

    choose!(pub fn choose5(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5));
}
