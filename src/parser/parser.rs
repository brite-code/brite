// TODO: Error recovery? Incremental parsing? These can be solved at the same time. We can recover
// from errors by parsing at a different increment. Having syntax with clear semicolons and/
// declarations makes it really easy for us to determine good, incremental, ranges.

// TODO: Special handling for `a < b < c`?
// TODO: Disallow `a < b > (c)` since we’ll use that for function call syntax.

use super::ast::*;
use super::lexer::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef, ExpectedSyntax};

/// Manages the parsing of Brite syntactical elements from source code. The `Parser` struct is
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

        // Class Declaration
        if let Some(_) = self.try_parse_identifier_keyword(IdentifierKeyword::Class) {
            return Ok(Declaration::Class(self.parse_class_declaration(false)?));
        }

        // Base Class Declaration
        if let Some(_) = self.try_parse_identifier_keyword(IdentifierKeyword::Base) {
            self.parse_identifier_keyword(IdentifierKeyword::Class)?;
            return Ok(Declaration::Class(self.parse_class_declaration(true)?));
        }

        self.unexpected(ExpectedSyntax::Declaration)
    }

    /// Parses a class declaration.
    fn parse_class_declaration(&mut self, base: bool) -> Result<ClassDeclaration, DiagnosticRef> {
        let name = self.parse_name()?;
        let extends = if self
            .try_parse_identifier_keyword(IdentifierKeyword::Extends)
            .is_some()
        {
            Some(self.parse_name()?)
        } else {
            None
        };
        let mut members = Vec::new();
        if self.try_parse_glyph(Glyph::BraceLeft).is_some() {
            while self.try_parse_glyph(Glyph::BraceRight).is_none() {
                let member = self.parse_class_member()?;
                members.push(member);
            }
        }
        Ok(ClassDeclaration {
            base,
            name,
            extends,
            members,
        })
    }

    /// Parses a class member.
    fn parse_class_member(&mut self) -> Result<ClassMember, DiagnosticRef> {
        // Class Field Member
        if let Some(name) = self.try_parse_name() {
            // Class Base Method Member
            if IdentifierKeyword::Base.test(&name.identifier) {
                if self.try_parse_glyph(Glyph::Colon).is_none() {
                    self.parse_keyword(Keyword::Fun)?;
                    let name = self.parse_name()?;
                    self.parse_glyph(Glyph::ParenLeft)?;
                    let (parameters, _) =
                        self.parse_comma_list(Glyph::ParenRight, Self::parse_function_parameter)?;
                    self.parse_glyph(Glyph::Arrow)?;
                    let return_type = self.parse_type()?;
                    return Ok(ClassMember::BaseMethod(BaseMethodClassMember {
                        name,
                        parameters,
                        return_type,
                    }));
                }
            } else {
                self.parse_glyph(Glyph::Colon)?;
            }
            let value = self.parse_type()?;
            self.try_parse_glyph(Glyph::Semicolon);
            return Ok(ClassMember::Field(FieldClassMember { name, value }));
        }

        // Class Method Member
        if self.try_parse_keyword(Keyword::Fun).is_some() {
            let name = self.parse_name()?;
            let function = self.parse_function()?;
            return Ok(ClassMember::Method(MethodClassMember { name, function }));
        }

        self.unexpected(ExpectedSyntax::ClassMember)
    }

    /// Parses the common parts of every function. Starting at the parameters.
    fn parse_function(&mut self) -> Result<Function, DiagnosticRef> {
        self.parse_glyph(Glyph::ParenLeft)?;
        let (parameters, _) =
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
            // Parse all semicolons which were left hanging around...
            while self.try_parse_glyph(Glyph::Semicolon).is_some() {}

            if let Some(end) = self.try_parse_glyph(Glyph::BraceRight) {
                break end;
            } else {
                let statement = self.parse_statement()?;
                statements.push(statement);
            }
        };
        let range = start.union(end);
        Ok(Block { range, statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, DiagnosticRef> {
        // Binding Statement
        if let Some(start) = self.try_parse_keyword(Keyword::Let) {
            let pattern = self.parse_pattern()?;
            let annotation = self.try_parse_type_annotation()?;
            self.parse_glyph(Glyph::Equals)?;
            let value = self.parse_expression()?;
            let maybe_end = self.try_parse_glyph(Glyph::Semicolon);
            return Ok(Statement {
                range: start.union(maybe_end.unwrap_or(value.range)),
                kind: StatementKind::Binding(BindingStatement {
                    pattern,
                    annotation,
                    value,
                }),
            });
        }

        // Return Statement
        if let Some(start) = self.try_parse_keyword(Keyword::Return) {
            // NOTE: If there is a newline between the return keyword and an expression then don’t
            // parse that expression as the return statement’s argument! This makes programming
            // without semicolons in Brite easier.
            let argument = match self.lexer.peek() {
                Some(token) if start.end().line() == token.range.start().line() => {
                    self.try_parse_expression()?
                }
                _ => None,
            };
            let maybe_end = self.try_parse_glyph(Glyph::Semicolon);
            let end =
                maybe_end.unwrap_or_else(|| argument.as_ref().map(|x| x.range).unwrap_or(start));
            return Ok(Statement {
                range: start.union(end),
                kind: StatementKind::Return(argument),
            });
        }

        // Expression Statement
        if let Some(expression) = self.try_parse_expression()? {
            let maybe_end = self.try_parse_glyph(Glyph::Semicolon);
            return Ok(Statement {
                range: if let Some(end) = maybe_end {
                    expression.range.union(end)
                } else {
                    expression.range
                },
                kind: StatementKind::Expression(expression),
            });
        }

        self.unexpected(ExpectedSyntax::Statement)
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

    #[inline]
    fn parse_expression(&mut self) -> Result<Expression, DiagnosticRef> {
        self.parse_expression_with_config(&ParseExpressionConfig::default())
    }

    #[inline]
    fn try_parse_expression(&mut self) -> Result<Option<Expression>, DiagnosticRef> {
        self.try_parse_infix_expression(&ParseExpressionConfig::default(), Precedence::LogicalOr)
    }

    /// Parses an expression with some custom configuration.
    #[inline]
    fn parse_expression_with_config(
        &mut self,
        config: &ParseExpressionConfig,
    ) -> Result<Expression, DiagnosticRef> {
        if let Some(expression) = self.try_parse_infix_expression(config, Precedence::LogicalOr)? {
            Ok(expression)
        } else {
            self.unexpected(ExpectedSyntax::Expression)
        }
    }

    /// Parses an infix expression. An infix expression is an operation on two expressions written
    /// in between the expressions. For example, addition or subtraction. Care must be taken to
    /// maintain the appropriate order of operations when parsing an infix expression.
    #[inline]
    fn try_parse_infix_expression(
        &mut self,
        config: &ParseExpressionConfig,
        precedence: Precedence,
    ) -> Result<Option<Expression>, DiagnosticRef> {
        if let Some(expression) = self.try_parse_prefix_expression(config)? {
            let expression = self.try_parse_infix_operator(config, precedence, expression)?;
            Ok(Some(expression))
        } else {
            Ok(None)
        }
    }

    #[inline]
    fn parse_infix_expression(
        &mut self,
        config: &ParseExpressionConfig,
        precedence: Precedence,
    ) -> Result<Expression, DiagnosticRef> {
        if let Some(expression) = self.try_parse_infix_expression(config, precedence)? {
            Ok(expression)
        } else {
            self.unexpected(ExpectedSyntax::Expression)
        }
    }

    fn try_parse_infix_operator(
        &mut self,
        config: &ParseExpressionConfig,
        precedence: Precedence,
        left: Expression,
    ) -> Result<Expression, DiagnosticRef> {
        // Or Logical Expression
        if precedence >= Precedence::LogicalOr {
            let next_precedence = Precedence::LogicalAnd;
            if self.try_parse_glyph(Glyph::BarDouble).is_some() {
                let op = LogicalOperator::Or;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::logical(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
        }

        // And Logical Expression
        if precedence >= Precedence::LogicalAnd {
            let next_precedence = Precedence::Equality;
            if self.try_parse_glyph(Glyph::AmpersandDouble).is_some() {
                let op = LogicalOperator::And;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::logical(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
        }

        // Equality Infix Expression
        if precedence >= Precedence::Equality {
            let next_precedence = Precedence::Relational;
            if self.try_parse_glyph(Glyph::EqualsDouble).is_some() {
                let op = InfixOperator::Equals;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
            if self.try_parse_glyph(Glyph::EqualsNot).is_some() {
                let op = InfixOperator::NotEquals;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
        }

        // Relational Infix Expression
        if precedence >= Precedence::Relational {
            let next_precedence = Precedence::Additive;
            if self.try_parse_glyph(Glyph::LessThan).is_some() {
                let op = InfixOperator::LessThan;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
            if self.try_parse_glyph(Glyph::LessThanOrEqual).is_some() {
                let op = InfixOperator::LessThanOrEqual;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
            if self.try_parse_glyph(Glyph::GreaterThan).is_some() {
                let op = InfixOperator::GreaterThan;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
            if self.try_parse_glyph(Glyph::GreaterThanOrEqual).is_some() {
                let op = InfixOperator::GreaterThanOrEqual;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
        }

        // Additive Infix Expression
        if precedence >= Precedence::Additive {
            let next_precedence = Precedence::Multiplicative;
            if self.try_parse_glyph(Glyph::Plus).is_some() {
                let op = InfixOperator::Add;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
            if self.try_parse_glyph(Glyph::Minus).is_some() {
                let op = InfixOperator::Subtract;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
        }

        // Multiplicative Infix Expression
        if precedence >= Precedence::Multiplicative {
            let next_precedence = Precedence::Exponentiation;
            if self.try_parse_glyph(Glyph::Asterisk).is_some() {
                let op = InfixOperator::Multiply;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
            if self.try_parse_glyph(Glyph::Slash).is_some() {
                let op = InfixOperator::Divide;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
            if self.try_parse_glyph(Glyph::Percent).is_some() {
                let op = InfixOperator::Remainder;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
        }

        // Exponentiation Infix Expression
        if precedence >= Precedence::Exponentiation {
            let next_precedence = Precedence::Bottom;
            if self.try_parse_glyph(Glyph::Caret).is_some() {
                let op = InfixOperator::Exponent;
                let right = self.parse_infix_expression(config, next_precedence)?;
                let range = left.range.union(right.range);
                let node = Expression::infix(range, op, left, right);
                return self.try_parse_infix_operator(config, precedence, node);
            }
        }

        Ok(left)
    }

    /// Parses a prefix expression. A prefix expression is a postfix expression extended with some
    /// operations before the expression. Like the boolean “not” operator or the number
    /// “negative” operator.
    fn try_parse_prefix_expression(
        &mut self,
        config: &ParseExpressionConfig,
    ) -> Result<Option<Expression>, DiagnosticRef> {
        // Try to parse a prefix expression operator. If no such operator exists then try to parse a
        // postfix expression.
        let (start, operator) = if let Some(range) = self.try_parse_glyph(Glyph::Bang) {
            (range, PrefixOperator::Not)
        } else if let Some(range) = self.try_parse_glyph(Glyph::Minus) {
            (range, PrefixOperator::Negative)
        } else if let Some(range) = self.try_parse_glyph(Glyph::Plus) {
            (range, PrefixOperator::Positive)
        } else {
            return self.try_parse_postfix_expression(config);
        };

        // If we parsed a prefix operator then we expect there to be an expression after
        // the operator.
        let operand = if let Some(expression) = self.try_parse_prefix_expression(config)? {
            expression
        } else {
            self.unexpected(ExpectedSyntax::Expression)?
        };

        let range = start.union(operand.range);
        Ok(Some(Expression {
            range,
            kind: ExpressionKind::Prefix(Box::new(PrefixExpression { operator, operand })),
        }))
    }

    /// Parses a postfix expression. A postfix expression is a primary expression extended with
    /// some operations after the expression. Like member access or call arguments.
    fn try_parse_postfix_expression(
        &mut self,
        config: &ParseExpressionConfig,
    ) -> Result<Option<Expression>, DiagnosticRef> {
        if let Some(mut expression) = self.try_parse_primary_expression()? {
            loop {
                // Member Expression
                if self.try_parse_glyph(Glyph::Dot).is_some() {
                    let property = self.parse_name()?;
                    let range = expression.range.union(property.range);
                    expression = Expression {
                        range,
                        kind: ExpressionKind::Member(Box::new(MemberExpression {
                            object: expression,
                            property,
                        })),
                    };
                    continue;
                }

                // Call Expression
                if let Some(token) = self.lexer.peek() {
                    if let TokenKind::Glyph(Glyph::ParenLeft) = &token.kind {
                        // NOTE: Call arguments must be on the same line as the callee! This makes
                        // programming without semicolons in Brite easier.
                        if expression.range.end().line() == token.range.start().line() {
                            self.lexer.next();
                            let (arguments, end) =
                                self.parse_comma_list(Glyph::ParenRight, Self::parse_expression)?;
                            let range = expression.range.union(end);
                            expression = Expression {
                                range,
                                kind: ExpressionKind::Call(CallExpression {
                                    callee: Box::new(expression),
                                    arguments,
                                }),
                            };
                            continue;
                        }
                    }
                }

                // Construct Expression
                if !config.before_block {
                    if let Some(token) = self.lexer.peek() {
                        if let TokenKind::Glyph(Glyph::BraceLeft) = &token.kind {
                            // NOTE: Constructor fields must be on the same line as the constructor!
                            // This makes programming without semicolons in Brite easier.
                            if expression.range.end().line() == token.range.start().line() {
                                match into_constructor(expression) {
                                    Err(x) => expression = x,

                                    // If the next token is a left brace (`{`) which is on the same
                                    // line as our expression and that expression is convertible
                                    // into a constructor then we have a `ConstructExpression`!
                                    Ok(constructor) => {
                                        self.lexer.next();
                                        let (fields, end) = self.parse_comma_list(
                                            Glyph::BraceRight,
                                            Self::parse_construct_expression_field,
                                        )?;
                                        let range = constructor.range.union(end);
                                        expression = Expression {
                                            range,
                                            kind: ExpressionKind::Construct(ConstructExpression {
                                                constructor,
                                                fields,
                                            }),
                                        };
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                }

                break;
            }
            Ok(Some(expression))
        } else {
            Ok(None)
        }
    }

    /// Parses a primary expression. A primary expression is balanced. It may stand on its own. It
    /// has a start and end which are not expressions themselves. It has the most basic precedence
    /// because of this. Other expressions which do depend on precedence build themselves out of
    /// primary expressions.
    fn try_parse_primary_expression(&mut self) -> Result<Option<Expression>, DiagnosticRef> {
        // Reference Expression
        if let Some((range, identifier)) = self.try_parse_identifier() {
            return Ok(Some(Expression {
                range,
                kind: ExpressionKind::Reference(identifier),
            }));
        }

        // This Expression
        if let Some(range) = self.try_parse_keyword(Keyword::This) {
            return Ok(Some(Expression {
                range,
                kind: ExpressionKind::This,
            }));
        }

        // Constant Expression
        if let Some((range, constant)) = self.try_parse_constant()? {
            return Ok(Some(Expression {
                range,
                kind: ExpressionKind::Constant(constant),
            }));
        }

        // Function Expression
        if let Some(start) = self.try_parse_keyword(Keyword::Fun) {
            let function = self.parse_function()?;
            let range = start.union(function.body.range);
            return Ok(Some(Expression {
                range,
                kind: ExpressionKind::Function(function),
            }));
        }

        // Conditional Expression
        if let Some(start) = self.try_parse_keyword(Keyword::If) {
            let conditional = self.parse_conditional_expression_if()?;
            let range = start.union(conditional.last_block().range);
            return Ok(Some(Expression {
                range,
                kind: ExpressionKind::Conditional(Box::new(conditional)),
            }));
        }

        // Wrapped Expression
        if let Some(start) = self.try_parse_glyph(Glyph::ParenLeft) {
            let expression = self.parse_expression()?;
            let annotation = self.try_parse_type_annotation()?;
            let end = self.parse_glyph(Glyph::ParenRight)?;
            let range = start.union(end);
            return Ok(Some(Expression {
                range,
                kind: ExpressionKind::Wrapped(Box::new(WrappedExpression {
                    expression,
                    annotation,
                })),
            }));
        }

        // Block Expression
        if let Some(start) = self.try_parse_keyword(Keyword::Do) {
            let block = self.parse_block()?;
            let range = start.union(block.range);
            return Ok(Some(Expression {
                range,
                kind: ExpressionKind::Block(block),
            }));
        }

        Ok(None)
    }

    fn parse_construct_expression_field(
        &mut self,
    ) -> Result<ConstructExpressionField, DiagnosticRef> {
        let name = self.parse_name()?;
        self.parse_glyph(Glyph::Colon)?;
        let value = self.parse_expression()?;
        Ok(ConstructExpressionField { name, value })
    }

    fn parse_conditional_expression_if(
        &mut self,
    ) -> Result<ConditionalExpressionIf, DiagnosticRef> {
        let mut test_config = ParseExpressionConfig::default();
        test_config.before_block = true;
        let test = self.parse_expression_with_config(&test_config)?;
        let consequent = self.parse_block()?;
        let alternate = if self.try_parse_keyword(Keyword::Else).is_some() {
            Some(if self.try_parse_keyword(Keyword::If).is_some() {
                ConditionalExpressionElse::ElseIf(Box::new(self.parse_conditional_expression_if()?))
            } else {
                ConditionalExpressionElse::Else(self.parse_block()?)
            })
        } else {
            None
        };
        Ok(ConditionalExpressionIf {
            test,
            consequent,
            alternate,
        })
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
            let (parameters, _) = self.parse_comma_list(Glyph::ParenRight, Self::parse_type)?;
            self.parse_glyph(Glyph::Arrow)?;
            let return_ = self.parse_type()?;
            let range = start.union(return_.range);
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
    /// the last glyph and returns the range of that last glyph.
    fn parse_comma_list<T>(
        &mut self,
        last_glyph: Glyph,
        parse_item: impl Fn(&mut Self) -> Result<T, DiagnosticRef>,
    ) -> Result<(Vec<T>, Range), DiagnosticRef> {
        let mut items = Vec::new();
        let last_glyph_range = loop {
            if let Some(range) = self.try_parse_glyph(last_glyph) {
                break range;
            }
            let item = parse_item(self)?;
            items.push(item);
            if self.try_parse_glyph(Glyph::Comma).is_none() {
                break self.parse_glyph(last_glyph)?;
            }
        };
        Ok((items, last_glyph_range))
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

    /// Parses a keyword. Reports an error if the next token is not a glyph.
    fn parse_keyword(&mut self, expected: Keyword) -> Result<Range, DiagnosticRef> {
        self.parse_glyph(Glyph::Keyword(expected))
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

    /// Tries to parse a specific identifier that we give special meaning to. Not all keywords need
    /// to be reserved. If the next token is the keyword then we advance the lexer and return the
    /// keyword’s range. Otherwise we don’t advance the lexer and return nothing.
    fn try_parse_identifier_keyword(&mut self, keyword: IdentifierKeyword) -> Option<Range> {
        if let Some(token) = self.lexer.peek() {
            if let TokenKind::Identifier(identifier) = &token.kind {
                if keyword.test(identifier) {
                    let range = token.range;
                    self.lexer.next();
                    return Some(range);
                }
            }
        }
        None
    }

    /// Parses a specific identifier that we give special meaning to. Not all keywords need to
    /// be reserved. If no name can be parsed then an error diagnostic will be reported.
    fn parse_identifier_keyword(
        &mut self,
        keyword: IdentifierKeyword,
    ) -> Result<Range, DiagnosticRef> {
        if let Some(token) = self.lexer.peek() {
            if let TokenKind::Identifier(identifier) = &token.kind {
                if keyword.test(identifier) {
                    let range = token.range;
                    self.lexer.next();
                    return Ok(range);
                }
            }
        }
        self.unexpected(ExpectedSyntax::IdentifierKeyword(keyword))
    }

    /// Tries to parse a name. If no name can be parsed then nothing will be returned.
    fn try_parse_name(&mut self) -> Option<Name> {
        if let Some(token) = self.lexer.peek() {
            if let TokenKind::Identifier(_) = &token.kind {
                let token = self.lexer.next().unwrap();
                let range = token.range;
                return match token.kind {
                    TokenKind::Identifier(identifier) => Some(Name { range, identifier }),
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

struct ParseExpressionConfig {
    before_block: bool,
}

impl Default for ParseExpressionConfig {
    fn default() -> Self {
        ParseExpressionConfig {
            before_block: false,
        }
    }
}

/// The precedence level at which we parse an infix expression.
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
enum Precedence {
    Bottom,
    Exponentiation,
    Multiplicative,
    Additive,
    Relational,
    Equality,
    LogicalAnd,
    LogicalOr,
}

/// Converts an expression into a class constructor. If the expression cannot be converted into a
/// class constructor we return `Err` with the expression.
fn into_constructor(expression: Expression) -> Result<Name, Expression> {
    match &expression.kind {
        ExpressionKind::Reference(_) => {
            let range = expression.range;
            match expression.kind {
                ExpressionKind::Reference(identifier) => Ok(Name { range, identifier }),
                _ => unreachable!(),
            }
        }
        _ => Err(expression),
    }
}
