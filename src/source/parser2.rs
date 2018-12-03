use super::ast::*;
use super::identifier::Keyword;
use super::lexer::Lexer;
use super::token::*;

use self::p::{ParseResult, Parser};

/// Intentionally `#[inline(always)]`. We want `parse_expression` to be inlined into `Parser`
/// specialized functions like `Parser::run::<parse_expression>`.
#[inline(always)]
fn parse_expression() -> impl Parser<Data = Expression> {
    p::choose5(
        // Parse `VariableExpression`.
        p::identifier().map(|token| Expression::Variable(VariableExpression::new(token))),
        // Parse `NumberExpression`.
        p::number().map(|token| NumberConstant::new(token).into()),
        // Parse true `BooleanExpression`.
        p::keyword(Keyword::True).map(|token| BooleanConstant::new(token, true).into()),
        // Parse false `BooleanExpression`.
        p::keyword(Keyword::False).map(|token| BooleanConstant::new(token, false).into()),
        // Parse `WrappedExpression`.
        p::group3(
            p::glyph(Glyph::ParenLeft),
            parse_expression,
            p::glyph(Glyph::ParenRight),
        ).map(|(paren_left, expression, paren_right)| {
            WrappedExpression::new(paren_left, expression, paren_right).into()
        }),
    )
}

mod p {
    use super::super::identifier::Keyword;
    use super::super::lexer::Lexer;
    use super::super::token::*;
    use std::marker::PhantomData;

    pub trait ParseResult<T>: Sized {
        fn parse<P: Parser<Data = T>>(lexer: &mut Lexer, p: P) -> Self;
    }

    pub struct TestParseResult<T> {
        ok: bool,
        phantom: PhantomData<T>,
    }

    impl<T> ParseResult<T> for TestParseResult<T> {
        #[inline(always)]
        fn parse<P: Parser<Data = T>>(lexer: &mut Lexer, p: P) -> Self {
            let ok = p.test(lexer);
            TestParseResult {
                ok,
                phantom: PhantomData,
            }
        }
    }

    pub struct RunParseResult<T> {
        result: Option<T>,
    }

    impl<T> ParseResult<T> for RunParseResult<T> {
        #[inline(always)]
        fn parse<P: Parser<Data = T>>(lexer: &mut Lexer, p: P) -> Self {
            let result = p.run(lexer);
            RunParseResult { result }
        }
    }

    pub trait Parser: Sized {
        type Data;

        /// Tests if this parser _might_ successfully parse without consuming any tokens from the
        /// lexer. We use this to resolve a choice to a certain parser.
        ///
        /// Implementations _must not_ advance the lexer!
        fn test(&self, lexer: &mut Lexer) -> bool;

        fn run(&self, lexer: &mut Lexer) -> Option<Self::Data>;

        #[inline(always)]
        fn map<T, F>(self, mapper: F) -> MapParser<Self, F>
        where
            F: Fn(Self::Data) -> T,
        {
            MapParser {
                parser: self,
                mapper,
            }
        }
    }

    pub struct MapParser<P, F> {
        parser: P,
        mapper: F,
    }

    impl<T, U, P, F> Parser for MapParser<P, F>
    where
        P: Parser<Data = T>,
        F: Fn(T) -> U,
    {
        type Data = U;

        #[inline(always)]
        fn test(&self, lexer: &mut Lexer) -> bool {
            self.parser.test(lexer)
        }

        #[inline(always)]
        fn run(&self, lexer: &mut Lexer) -> Option<Self::Data> {
            self.parser.run(lexer).map(&self.mapper)
        }
    }

    impl<P: Parser, F> Parser for F
    where
        F: Fn() -> P,
    {
        type Data = P::Data;

        /// Intentionally not `#[inline(always)]`. Say we have `fn parse_expression()` which returns
        /// a `Parser`. We want `parse_expression` to be inlined in
        /// `Parser::test::<fn parse_expression()>` and have that function be the one expressed in
        /// the build.
        fn test(&self, lexer: &mut Lexer) -> bool {
            self().test(lexer)
        }

        /// Intentionally not `#[inline(always)]`. Say we have `fn parse_expression()` which returns
        /// a `Parser`. We want `parse_expression` to be inlined in
        /// `Parser::run::<fn parse_expression()>` and have that function be the one expressed in
        /// the build.
        fn run(&self, lexer: &mut Lexer) -> Option<Self::Data> {
            self().run(lexer)
        }
    }

    struct IdentifierParser;

    #[inline(always)]
    pub fn identifier() -> impl Parser<Data = IdentifierToken> {
        IdentifierParser
    }

    impl Parser for IdentifierParser {
        type Data = IdentifierToken;

        #[inline(always)]
        fn test(&self, lexer: &mut Lexer) -> bool {
            match lexer.lookahead() {
                Token::Identifier(_) => true,
                _ => false,
            }
        }

        #[inline(always)]
        fn run(&self, lexer: &mut Lexer) -> Option<Self::Data> {
            match lexer.advance() {
                Token::Identifier(token) => Some(token),
                _ => None,
            }
        }
    }

    struct NumberParser;

    #[inline(always)]
    pub fn number() -> impl Parser<Data = NumberToken> {
        NumberParser
    }

    impl Parser for NumberParser {
        type Data = NumberToken;

        #[inline(always)]
        fn test(&self, lexer: &mut Lexer) -> bool {
            match lexer.lookahead() {
                Token::Number(_) => true,
                _ => false,
            }
        }

        #[inline(always)]
        fn run(&self, lexer: &mut Lexer) -> Option<Self::Data> {
            match lexer.advance() {
                Token::Number(token) => Some(token),
                _ => None,
            }
        }
    }

    struct GlyphParser {
        glyph: Glyph,
    }

    #[inline(always)]
    pub fn glyph(glyph: Glyph) -> impl Parser<Data = GlyphToken> {
        GlyphParser { glyph }
    }

    #[inline(always)]
    pub fn keyword(keyword: Keyword) -> impl Parser<Data = GlyphToken> {
        glyph(Glyph::Keyword(keyword))
    }

    impl Parser for GlyphParser {
        type Data = GlyphToken;

        #[inline(always)]
        fn test(&self, lexer: &mut Lexer) -> bool {
            match lexer.lookahead() {
                Token::Glyph(token) => token.glyph() == &self.glyph,
                _ => false,
            }
        }

        #[inline(always)]
        fn run(&self, lexer: &mut Lexer) -> Option<Self::Data> {
            match lexer.advance() {
                Token::Glyph(token) => if token.glyph() == &self.glyph {
                    Some(token)
                } else {
                    None
                },
                _ => None,
            }
        }
    }

    macro_rules! group_parser {
        (fn $f:ident($($x:ident: $t:ident),*)) => {
            #[inline(always)]
            pub fn $f<$($t: Parser),*>($($x: $t),*) -> impl Parser<Data = ($($t::Data),*)> {
                struct GroupParser<$($t),*> { $($x: $t),* }

                impl<$($t: Parser),*> Parser for GroupParser<$($t),*> {
                    type Data = ($($t::Data),*);

                    #[inline(always)]
                    fn test(&self, lexer: &mut Lexer) -> bool {
                        // NOTE: We assume that every macro invocation has at least `p1: P1` and
                        // that `p1` is the first parser.
                        self.p1.test(lexer)
                    }

                    #[inline(always)]
                    fn run(&self, lexer: &mut Lexer) -> Option<Self::Data> {
                        $(let $x = self.$x.run(lexer)?;)*
                        Some(($($x),*))
                    }
                }

                GroupParser { $($x),* }
            }
        };
    }

    macro_rules! choose_parser {
        (fn $f:ident($($x:ident: $t:ident),*)) => {
            #[inline(always)]
            pub fn $f<T, $($t: Parser<Data = T>),*>($($x: $t),*) -> impl Parser<Data = T> {
                struct ChooseParser<$($t),*> { $($x: $t),* }

                impl<T, $($t: Parser<Data = T>),*> Parser for ChooseParser<$($t),*> {
                    type Data = T;

                    #[inline(always)]
                    fn test(&self, lexer: &mut Lexer) -> bool {
                        $(self.$x.test(lexer))||*
                    }

                    #[inline(always)]
                    fn run(&self, lexer: &mut Lexer) -> Option<Self::Data> {
                        $(if self.$x.test(lexer) {
                            return self.$x.run(lexer);
                        })*
                        None
                    }
                }

                ChooseParser { $($x),* }
            }
        };
    }

    group_parser!(fn group3(p1: P1, p2: P2, p3: P3));
    choose_parser!(fn choose5(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5));
}

// struct Parser<T>;

// impl<T> Parser<T> {
//     fn glyph(glyph: Glyph) -> Parser<GlyphToken> {}

//     fn keyword(keyword: Keyword) -> Parser<GlyphToken> {
//         Self::glyph(Glyph::Keyword(keyword))
//     }

//     fn identifier() -> Parser<IdentifierToken> {}

//     fn number() -> Parser<IdentifierToken> {}

//     fn group3<A, B, C>(a: Parser<A>, b: Parser<B>, c: Parser<C>) -> Parser<(A, B, C)> {}

//     fn map<U>(self, mapper: impl Fn(T) -> U) -> Parser<U> {}
// }

// trait Parser {
//     type Return;

//     fn parse_glyph(lexer: &mut Lexer, glyph: Glyph) -> Self::Return;
// }

// struct LookaheadParser;

// impl Parser for LookaheadParser {
//     type Return = bool;

//     fn parse_glyph(lexer: &mut Lexer, glyph: Glyph) -> Self::Return {
//         match lexer.lookahead() {
//             Token::Glyph(token) => token.glyph() == &glyph,
//             _ => false,
//         }
//     }
// }

// fn parse_expression<P: Parser>(lexer: &mut Lexer) -> P::Return {
//     P::choose2(P::parse_glyph(lexer, Glyph::Keyword(Keyword::If)))
// }

// fn parse(lexer: &mut Lexer) {
//     parse_expression::<LookaheadParser>(lexer);
// }

// trait Parser {
//     type Data;
//     fn test(&self, lexer: &mut Lexer) -> bool;
//     fn parse(&self, lexer: &mut Lexer) -> Option<Self::Data>;
// }

// struct GlyphParser {
//     glyph: Glyph,
// }

// impl GlyphParser {
//     #[inline(always)]
//     pub fn new(glyph: Glyph) -> Self {
//         GlyphParser { glyph }
//     }
// }

// impl Parser for GlyphParser {
//     type Data = GlyphToken;

//     #[inline(always)]
//     fn test(&self, lexer: &mut Lexer) -> bool {
//         match lexer.lookahead() {
//             Token::Glyph(token) => token.glyph() == &self.glyph,
//             _ => false,
//         }
//     }

//     #[inline(always)]
//     fn parse(&self, lexer: &mut Lexer) -> Option<Self::Data> {
//         match lexer.lookahead() {
//             Token::Glyph(token) => if token.glyph() == &self.glyph {
//                 match lexer.advance() {
//                     Token::Glyph(token) => Some(token),
//                     _ => unreachable!(),
//                 }
//             } else {
//                 None
//             },
//             _ => None,
//         }
//     }
// }

// fn parse_expression() {}

// fn parse_binding_statement() {
//     join5(
//         parse_let,
//         parse_pattern,
//         parse_equals,
//         parse_expression,
//         optional(parse_semicolon),
//         |x1, x2, x3, x4, x5| {},
//     )
// }

// // type BindingStatement = Group5<Let, Pattern, Equals, Expression, Option<Semicolon>>;

// trait Phrase: Sized {
//     fn parse(lexer: &mut Lexer) -> Option<Self>;
// }

// trait Terminal: Sized {
//     fn parse(token: Token) -> Option<Self>;
// }

// impl<T: Terminal> Phrase for T {
//     fn parse(lexer: &mut Lexer) -> Option<Self> {
//         Terminal::parse(lexer.advance())
//     }
// }

// macro_rules! terminal_glyph {
//     ($glyph:ident) => {
//         struct $glyph(GlyphToken);

//         impl Terminal for $glyph {
//             fn parse(token: Token) -> Option<Self> {
//                 match token {
//                     Token::Glyph(token) => if token.glyph() == &Glyph::$glyph {
//                         Some($glyph(token))
//                     } else {
//                         None
//                     },
//                     _ => None,
//                 }
//             }
//         }
//     };
// }

// terminal_glyph!(BraceLeft);
// terminal_glyph!(BraceRight);
// terminal_glyph!(Comma);
// terminal_glyph!(Dot);
// terminal_glyph!(Equals);
// terminal_glyph!(ParenLeft);
// terminal_glyph!(ParenRight);
// terminal_glyph!(Semicolon);
// terminal_glyph!(Slash);
// terminal_glyph!(Underscore);

// macro_rules! terminal_keyword {
//     ($keyword:ident) => {
//         struct $keyword(GlyphToken);

//         impl Terminal for $keyword {
//             fn parse(token: Token) -> Option<Self> {
//                 match token {
//                     Token::Glyph(token) => if token.glyph() == &Glyph::Keyword(Keyword::$keyword) {
//                         Some($keyword(token))
//                     } else {
//                         None
//                     },
//                     _ => None,
//                 }
//             }
//         }
//     };
// }

// terminal_keyword!(Hole);
// terminal_keyword!(True);
// terminal_keyword!(False);
// terminal_keyword!(Let);
// terminal_keyword!(If);
// terminal_keyword!(Else);
// terminal_keyword!(Do);

// // enum State {
// //     Expression,
// // }

// // fn parse(lexer: &mut Lexer) {
// //     let mut stack: Vec<State> = Vec::new();
// //     while let Some(state) = stack.pop() {

// //     }

// //     // // let mut stack = Vec::new();
// //     // let state = State::Expression;

// //     // loop {
// //     //     let token = lexer.advance();
// //     //     match state {
// //     //         State::Expression => {
// //     //             let expression: Expression = match token {
// //     //                 // Parse `VariableExpression`.
// //     //                 Token::Identifier(identifier) => VariableExpression::new(identifier).into(),

// //     //                 Token::Glyph(token) => match token.glyph() {
// //     //                     // Parse `WrappedExpression`.
// //     //                     Glyph::ParenLeft => {
// //     //                         let paren_left = token;
// //     //                     }

// //     //                     _ => unimplemented!(),
// //     //                 },

// //     //                 _ => unimplemented!(),
// //     //             };
// //     //         }
// //     //     }
// //     // }
// // }

// // type BindingStatement = Group2<Group4<Let, Pattern, Equals, Expression>, Option<Semicolon>>;

// // mod grammar {
// //     struct Group;
// // }

// // mod cfg {
// //     use super::super::lexer::Lexer;
// //     use super::super::token::Token;

// //     trait Phrase {
// //         fn test(lexer: &mut Lexer) -> bool;

// //         fn parse(lexer: &mut Lexer) -> Self;
// //     }

// //     struct And<A, B>(A, B);

// //     impl<A, B> Phrase for And<A, B>
// //     where
// //         A: Phrase,
// //         B: Phrase,
// //     {
// //         fn test(lexer: &mut Lexer) -> bool {
// //             A::test(lexer)
// //         }

// //         fn parse(lexer: &mut Lexer) -> Self {
// //             And(A::parse(lexer), B::parse(lexer))
// //         }
// //     }

// //     type And3<A, B, C> = And<A, And<B, C>>;
// //     type And4<A, B, C, D> = And3<A, B, And<C, D>>;
// //     type And5<A, B, C, D, E> = And4<A, B, C, And<D, E>>;

// //     enum Or<A, B> {
// //         A(A),
// //         B(B),
// //         None,
// //     }

// //     impl<A, B> Phrase for Or<A, B>
// //     where
// //         A: Phrase,
// //         B: Phrase,
// //     {
// //         fn test(lexer: &mut Lexer) -> bool {
// //             A::test(lexer) || B::test(lexer)
// //         }

// //         fn parse(lexer: &mut Lexer) -> Self {
// //             if A::test(lexer) {
// //                 Or::A(A::parse(lexer))
// //             } else {
// //                 Or::B(B::parse(lexer))
// //             }
// //         }
// //     }

// //     type Or3<A, B, C> = Or<A, Or<B, C>>;
// //     type Or4<A, B, C, D> = Or3<A, B, Or<C, D>>;
// //     type Or5<A, B, C, D, E> = Or4<A, B, C, Or<D, E>>;
// // }
