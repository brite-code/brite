use super::ast::*;
use super::document::Range;
use super::lexer::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef, ExpectedSyntax};

#[cfg(rustdoc)]
use super::document::Document;

/// Manages the parsing of Brite syntactical elements from a [`Document`]. The `Parser` struct is
/// more like a parsing “context”. It does not hold much state itself. Most of the parsing state is
/// implemented in our [`Lexer`].
pub struct Parser<'errs, 'src> {
    /// The lexer our parser uses.
    lexer: Lexer<'errs, 'src>,
}

impl<'errs, 'src> Parser<'errs, 'src> {
    /// Creates a new parser.
    pub fn new(lexer: Lexer<'errs, 'src>) -> Self {
        Parser { lexer }
    }

    /// Parses a Brite module to the end of the document being parsed. Consumes the parser as we
    /// consume the provided lexer. Either succeeds and returns a module or fails and returns the
    /// error diagnostic.
    pub fn parse_module(mut self) -> Result<Module, DiagnosticRef> {
        let mut declarations = Vec::new();
        while self.lexer.peek().is_some() {
            declarations.push(self.parse_declaration()?);
        }
        Ok(Module { declarations })
    }

    fn parse_declaration(&mut self) -> Result<Declaration, DiagnosticRef> {
        // Function Declaration
        if self.try_parse_keyword(Keyword::Fun).is_some() {
            let name = self.parse_name()?;
            let function = self.parse_function()?;
            return Ok(Declaration::Function(FunctionDeclaration {
                name,
                function,
            }));
        }

        self.unexpected(ExpectedSyntax::Declaration)
    }

    /// Parses the common parts of every function. Starting at the parameters.
    fn parse_function(&mut self) -> Result<Function, DiagnosticRef> {
        self.parse_glyph(Glyph::ParenLeft)?;
        let parameters =
            self.parse_comma_list(Glyph::ParenRight, Self::parse_function_parameter)?;
        let return_type = if self.try_parse_glyph(Glyph::Arrow).is_some() {
            Some(self.parse_type()?)
        } else {
            None
        };
        let body = self.parse_block()?;
        Ok(Function {
            parameters,
            return_type,
            body,
        })
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, DiagnosticRef> {
        let pattern = self.parse_pattern()?;
        let annotation = self.try_parse_type_annotation()?;
        Ok(FunctionParameter {
            pattern,
            annotation,
        })
    }

    fn parse_block(&mut self) -> Result<Block, DiagnosticRef> {
        let mut statements = Vec::new();
        let start = self.parse_glyph(Glyph::BraceLeft)?;
        let end = loop {
            if let Some(end) = self.try_parse_glyph(Glyph::BraceRight) {
                break end;
            } else {
                let statement = self.parse_statement()?;
                statements.push(statement);
            }
        };
        let range = start.between(end);
        Ok(Block { range, statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, DiagnosticRef> {
        // Binding Statement
        if self.try_parse_keyword(Keyword::Let).is_some() {
            let pattern = self.parse_pattern()?;
            let annotation = self.try_parse_type_annotation()?;
            self.parse_glyph(Glyph::Equals)?;
            let value = self.parse_expression()?;
            self.try_parse_glyph(Glyph::Semicolon);
            return Ok(Statement::Binding(BindingStatement {
                pattern,
                annotation,
                value,
            }));
        }

        // Empty Statement
        if self.try_parse_glyph(Glyph::Semicolon).is_some() {
            return Ok(Statement::Empty);
        }

        // Expression Statement
        let expression = self.parse_expression()?;
        self.try_parse_glyph(Glyph::Semicolon);
        Ok(Statement::Expression(expression))
    }

    fn try_parse_constant(&mut self) -> Result<Option<(Range, Constant)>, DiagnosticRef> {
        // True Boolean Constant
        if let Some(range) = self.try_parse_keyword(Keyword::True) {
            return Ok(Some((range, Constant::Boolean(true))));
        }

        // False Boolean Constant
        if let Some(range) = self.try_parse_keyword(Keyword::False) {
            return Ok(Some((range, Constant::Boolean(false))));
        }

        // Number Constant
        if let Some((range, number)) = self.try_parse_number() {
            let constant = match number.kind {
                NumberKind::DecimalInteger(value) => Constant::Integer(IntegerBase::Decimal, value),
                NumberKind::HexadecimalInteger(value) => {
                    Constant::Integer(IntegerBase::Hexadecimal, value)
                }
                NumberKind::BinaryInteger(value) => Constant::Integer(IntegerBase::Binary, value),
                NumberKind::Float(value) => Constant::Float(value),
                NumberKind::Invalid(error) => return Err(error),
            };
            return Ok(Some((range, constant)));
        }

        Ok(None)
    }

    fn parse_expression(&mut self) -> Result<Expression, DiagnosticRef> {
        // Reference Expression
        if let Some((range, identifier)) = self.try_parse_identifier() {
            return Ok(Expression {
                range,
                kind: ExpressionKind::Reference(identifier),
            });
        }

        // This Expression
        if let Some(range) = self.try_parse_keyword(Keyword::This) {
            return Ok(Expression {
                range,
                kind: ExpressionKind::This,
            });
        }

        // Constant Expression
        if let Some((range, constant)) = self.try_parse_constant()? {
            return Ok(Expression {
                range,
                kind: ExpressionKind::Constant(constant),
            });
        }

        // Function Expression
        if let Some(start) = self.try_parse_keyword(Keyword::Fun) {
            let function = self.parse_function()?;
            let range = start.between(function.body.range);
            return Ok(Expression {
                range,
                kind: ExpressionKind::Function(function),
            });
        }

        // Wrapped Expression
        if let Some(start) = self.try_parse_glyph(Glyph::ParenLeft) {
            let expression = self.parse_expression()?;
            let annotation = self.try_parse_type_annotation()?;
            let end = self.parse_glyph(Glyph::ParenRight)?;
            let range = start.between(end);
            return Ok(Expression {
                range,
                kind: ExpressionKind::Wrapped(Box::new(WrappedExpression {
                    expression,
                    annotation,
                })),
            });
        }

        self.unexpected(ExpectedSyntax::Expression)
    }

    fn parse_pattern(&mut self) -> Result<Pattern, DiagnosticRef> {
        // Binding Pattern
        if let Some((range, identifier)) = self.try_parse_identifier() {
            return Ok(Pattern {
                range,
                kind: PatternKind::Binding(identifier),
            });
        }

        // Hole Pattern
        if let Some(range) = self.try_parse_keyword(Keyword::Hole) {
            return Ok(Pattern {
                range,
                kind: PatternKind::Hole,
            });
        }

        // This Pattern
        if let Some(range) = self.try_parse_keyword(Keyword::This) {
            return Ok(Pattern {
                range,
                kind: PatternKind::This,
            });
        }

        self.unexpected(ExpectedSyntax::Pattern)
    }

    fn parse_type(&mut self) -> Result<Type, DiagnosticRef> {
        // Reference type
        if let Some((range, identifier)) = self.try_parse_identifier() {
            return Ok(Type {
                range,
                kind: TypeKind::Reference(identifier),
            });
        }

        // This type
        if let Some(range) = self.try_parse_keyword(Keyword::This) {
            return Ok(Type {
                range,
                kind: TypeKind::This,
            });
        }

        // Function type
        if let Some(start) = self.try_parse_keyword(Keyword::Fun) {
            self.parse_glyph(Glyph::ParenLeft)?;
            let parameters = self.parse_comma_list(Glyph::ParenRight, Self::parse_type)?;
            self.parse_glyph(Glyph::Arrow)?;
            let return_ = self.parse_type()?;
            let range = start.between(return_.range);
            return Ok(Type {
                range,
                kind: TypeKind::Function(FunctionType {
                    parameters,
                    return_: Box::new(return_),
                }),
            });
        }

        self.unexpected(ExpectedSyntax::Type)
    }

    /// If there is a colon then we parse a type annotation. Otherwise parse nothing.
    fn try_parse_type_annotation(&mut self) -> Result<Option<Type>, DiagnosticRef> {
        if self.try_parse_glyph(Glyph::Colon).is_some() {
            Ok(Some(self.parse_type()?))
        } else {
            Ok(None)
        }
    }

    /// Parses a list of comma separated items with support for trailing commas. To support trailing
    /// commas we need to know what the glyph which comes after the comma list is. Always parses
    /// the last glyph.
    fn parse_comma_list<T>(
        &mut self,
        last_glyph: Glyph,
        parse_item: impl Fn(&mut Self) -> Result<T, DiagnosticRef>,
    ) -> Result<Vec<T>, DiagnosticRef> {
        let mut items = Vec::new();
        loop {
            if self.try_parse_glyph(last_glyph).is_some() {
                break;
            }
            let item = parse_item(self)?;
            items.push(item);
            if self.try_parse_glyph(Glyph::Comma).is_none() {
                self.parse_glyph(last_glyph)?;
                break;
            }
        }
        Ok(items)
    }

    /// Parses a glyph. Reports an error if the next token is not a glyph.
    fn parse_glyph(&mut self, expected: Glyph) -> Result<Range, DiagnosticRef> {
        if let Some(token) = self.lexer.peek() {
            if let TokenKind::Glyph(actual) = &token.kind {
                if expected == *actual {
                    let range = token.range;
                    self.lexer.next();
                    return Ok(range);
                }
            }
        }
        self.unexpected(ExpectedSyntax::Glyph(expected))
    }

    /// Tries to parse a glyph. If the next token is the expected token then we advance the lexer
    /// and return the glyph’s range. Otherwise we don’t advance the lexer and return nothing.
    fn try_parse_glyph(&mut self, expected: Glyph) -> Option<Range> {
        if let Some(token) = self.lexer.peek() {
            if let TokenKind::Glyph(actual) = &token.kind {
                if expected == *actual {
                    let range = token.range;
                    self.lexer.next();
                    return Some(range);
                }
            }
        }
        None
    }

    /// Tries to parse a keyword. If the next token is the expected token then we advance the lexer
    /// and return the keyword’s range. Otherwise we don’t advance the lexer and return nothing.
    fn try_parse_keyword(&mut self, expected: Keyword) -> Option<Range> {
        self.try_parse_glyph(Glyph::Keyword(expected))
    }

    /// Tries to parse an identifier. If the next token is an identifier then we advance the lexer
    /// and return that identifier. Otherwise we don’t advance the lexer and return nothing.
    fn try_parse_identifier(&mut self) -> Option<(Range, Identifier)> {
        if let Some(token) = self.lexer.peek() {
            if let TokenKind::Identifier(_) = &token.kind {
                let token = self.lexer.next().unwrap();
                let range = token.range;
                return match token.kind {
                    TokenKind::Identifier(identifier) => Some((range, identifier)),
                    _ => unreachable!(),
                };
            }
        }
        None
    }

    /// Parses a name. If no name can be parsed then an error diagnostic will be reported.
    fn parse_name(&mut self) -> Result<Name, DiagnosticRef> {
        if let Some(token) = self.lexer.peek() {
            if let TokenKind::Identifier(_) = &token.kind {
                let token = self.lexer.next().unwrap();
                let range = token.range;
                return match token.kind {
                    TokenKind::Identifier(identifier) => Ok(Name { range, identifier }),
                    _ => unreachable!(),
                };
            }
        }
        self.unexpected(ExpectedSyntax::Identifier)
    }

    /// Tries to parse a number. If the next token is a number then we advance the lexer
    /// and return that number. Otherwise we don’t advance the lexer and return nothing.
    fn try_parse_number(&mut self) -> Option<(Range, Number)> {
        if let Some(token) = self.lexer.peek() {
            if let TokenKind::Number(_) = &token.kind {
                let token = self.lexer.next().unwrap();
                let range = token.range;
                return match token.kind {
                    TokenKind::Number(number) => Some((range, number)),
                    _ => unreachable!(),
                };
            }
        }
        None
    }

    /// If the next token is unexpected then call this function and say what we did expect. This
    /// function will throw an unexpected syntax error.
    fn unexpected<T>(&mut self, expected: ExpectedSyntax) -> Result<T, DiagnosticRef> {
        match self.lexer.peek() {
            Some(token) => {
                let diagnostic = Diagnostic::unexpected_token(token, expected);
                Err(self.report_diagnostic(diagnostic))
            }
            None => {
                let end_position = self.lexer.peek_end().unwrap().position();
                let diagnostic = Diagnostic::unexpected_ending(end_position, expected);
                Err(self.report_diagnostic(diagnostic))
            }
        }
    }

    /// Report a diagnostic.
    ///
    /// The implementation may change at any time. Currently calls `Lexer::report_diagnostic` since
    /// the lexer owns a unique mutable reference to our diagnostics collection.
    fn report_diagnostic(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        self.lexer.report_diagnostic(diagnostic)
    }
}
