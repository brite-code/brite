//! This module represents the JavaScript AST. It is based on the Babel AST specified here:
//!
//! https://github.com/babel/babel/blob/master/packages/babel-parser/ast/spec.md

use std::io;

/// A valid JavaScript identifier.
pub struct Identifier(String);

impl Identifier {
    /// Create a new identifier without checking to see if the string is a valid
    /// JavaScript identifier. We trust that the caller of this function performed that check!
    pub fn new_unchecked(string: String) -> Identifier {
        Identifier(string)
    }
}

pub struct Statement(StatementKind);

enum StatementKind {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
}

struct VariableDeclaration {
    kind: VariableDeclarationKind,
    id: Pattern,
    init: Expression,
}

pub enum VariableDeclarationKind {
    Var,
    Let,
    Const,
}

pub struct Expression(ExpressionKind);

enum ExpressionKind {
    Identifier(Identifier),
    BooleanLiteral(bool),
    NumericLiteral(f64),
    Logical(Box<LogicalExpression>),
}

struct LogicalExpression {
    operator: LogicalOperator,
    left: Expression,
    right: Expression,
}

pub enum LogicalOperator {
    Or,
    And,
}

pub struct Pattern(PatternKind);

enum PatternKind {
    Identifier(Identifier),
}

impl Statement {
    pub fn expression(expression: Expression) -> Self {
        Statement(StatementKind::Expression(expression))
    }

    pub fn variable_declaration(
        kind: VariableDeclarationKind,
        id: Pattern,
        init: Expression,
    ) -> Self {
        Statement(StatementKind::VariableDeclaration(VariableDeclaration {
            kind,
            id,
            init,
        }))
    }
}

impl Expression {
    pub fn identifier(identifier: Identifier) -> Self {
        Expression(ExpressionKind::Identifier(identifier))
    }

    pub fn boolean_literal(value: bool) -> Self {
        Expression(ExpressionKind::BooleanLiteral(value))
    }

    pub fn numeric_literal(value: f64) -> Self {
        Expression(ExpressionKind::NumericLiteral(value))
    }

    pub fn logical(operator: LogicalOperator, left: Expression, right: Expression) -> Self {
        Expression(ExpressionKind::Logical(Box::new(LogicalExpression {
            operator,
            left,
            right,
        })))
    }
}

impl Pattern {
    pub fn identifier(identifier: Identifier) -> Self {
        Pattern(PatternKind::Identifier(identifier))
    }
}

/// Precedence levels in the [JavaScript expression][1] syntax. We don’t need all the precedence
/// levels so the ones which are currently unused are commented out.
///
/// We also have `Top` which represents all expressions and `Bottom` which represents
/// no expressions.
///
/// [1]: https://tc39.github.io/ecma262/#prod-Expression
#[derive(Eq, Ord, PartialEq, PartialOrd)]
enum Precedence {
    Top,
    // Assignment,
    // Conditional,
    LogicalOr,
    LogicalAnd,
    // BitwiseOr,
    // BitwiseXor,
    // BitwiseAnd,
    // Equality,
    // Relational,
    // Shift,
    // Additive,
    // Multiplicative,
    // Exponential,
    // Unary,
    // Update,
    // Call,
    // Member,
    Primary,
    // Bottom,
}

impl Statement {
    pub fn write(&self, w: &mut io::Write, i: usize) -> io::Result<()> {
        match &self.0 {
            StatementKind::Expression(expression) => {
                expression.write(w, i, Precedence::Top)?;
                write!(w, ";")
            }
            StatementKind::VariableDeclaration(variable_declaration) => {
                match &variable_declaration.kind {
                    VariableDeclarationKind::Var => write!(w, "var")?,
                    VariableDeclarationKind::Let => write!(w, "let")?,
                    VariableDeclarationKind::Const => write!(w, "const")?,
                }
                write!(w, " ")?;
                variable_declaration.id.write(w)?;
                write!(w, " = ")?;
                variable_declaration.init.write(w, i, Precedence::Top)?;
                write!(w, ";")
            }
        }
    }
}

impl Expression {
    /// Print an expression at the provided level of indentation. All expressions with a smaller
    /// precedence than `p` will be wrapped in parentheses.
    fn write(&self, w: &mut io::Write, i: usize, p: Precedence) -> io::Result<()> {
        let precedence = match &self.0 {
            ExpressionKind::Identifier(_) => Precedence::Primary,
            ExpressionKind::BooleanLiteral(_) => Precedence::Primary,
            ExpressionKind::NumericLiteral(_) => Precedence::Primary,
            ExpressionKind::Logical(logical) => match &logical.operator {
                LogicalOperator::Or => Precedence::LogicalOr,
                LogicalOperator::And => Precedence::LogicalAnd,
            },
        };
        if p > precedence {
            write!(w, "(")?;
        }
        match &self.0 {
            ExpressionKind::Identifier(identifier) => write!(w, "{}", &identifier.0)?,
            ExpressionKind::BooleanLiteral(true) => write!(w, "true")?,
            ExpressionKind::BooleanLiteral(false) => write!(w, "false")?,
            ExpressionKind::NumericLiteral(value) => {
                if value.is_nan() {
                    write!(w, "NaN")?
                } else if value.is_infinite() {
                    if value.is_sign_positive() {
                        write!(w, "Infinity")?
                    } else {
                        write!(w, "-Infinity")?
                    }
                } else if *value >= 10_000_000_000. {
                    write!(w, "{:e}", value)?
                } else {
                    write!(w, "{}", value)?
                }
            }
            ExpressionKind::Logical(logical) => match &logical.operator {
                LogicalOperator::Or => {
                    logical.left.write(w, i, Precedence::LogicalOr)?;
                    write!(w, " || ")?;
                    logical.right.write(w, i, Precedence::LogicalOr)?;
                }
                LogicalOperator::And => {
                    logical.left.write(w, i, Precedence::LogicalAnd)?;
                    write!(w, " && ")?;
                    logical.right.write(w, i, Precedence::LogicalAnd)?;
                }
            },
        }
        if p > precedence {
            write!(w, ")")?;
        }
        Ok(())
    }
}

impl Pattern {
    fn write(&self, w: &mut io::Write) -> io::Result<()> {
        match &self.0 {
            PatternKind::Identifier(identifier) => write!(w, "{}", &identifier.0)?,
        }
        Ok(())
    }
}
