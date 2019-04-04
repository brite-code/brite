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

/// A complete JavaScript program source tree.
pub struct Program {
    body: Vec<Statement>,
    _private: (),
}

impl Program {
    pub fn new(body: Vec<Statement>) -> Self {
        Program { body, _private: () }
    }
}

pub struct Statement(StatementKind);

enum StatementKind {
    Expression(Expression),
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
}

pub struct BlockStatement {
    body: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(body: Vec<Statement>) -> Self {
        BlockStatement { body }
    }
}

struct FunctionDeclaration {
    id: Identifier,
    params: Vec<Pattern>,
    body: BlockStatement,
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

    pub fn function_declaration(
        id: Identifier,
        params: Vec<Pattern>,
        body: BlockStatement,
    ) -> Self {
        Statement(StatementKind::FunctionDeclaration(FunctionDeclaration {
            id,
            params,
            body,
        }))
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

impl Identifier {
    fn write<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        write!(w, "{}", &self.0)
    }
}

impl Program {
    pub fn write<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        for statement in &self.body {
            statement.write(w, 0)?;
        }
        Ok(())
    }
}

impl Statement {
    fn write<W: io::Write>(&self, w: &mut W, i: usize) -> io::Result<()> {
        write_indentation(w, i)?;

        match &self.0 {
            StatementKind::Expression(expression) => {
                expression.write(w, i, Precedence::Top)?;
                write!(w, ";\n")
            }
            StatementKind::FunctionDeclaration(function_declaration) => {
                write!(w, "function ")?;
                function_declaration.id.write(w)?;
                write!(w, "(")?;
                for i in 0..function_declaration.params.len() {
                    if i != 0 {
                        write!(w, ", ")?;
                    }
                    function_declaration.params[i].write(w)?;
                }
                write!(w, ") ")?;
                function_declaration.body.write(w, i)?;
                write!(w, "\n")
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
                write!(w, ";\n")
            }
        }
    }
}

impl BlockStatement {
    fn write<W: io::Write>(&self, w: &mut W, i: usize) -> io::Result<()> {
        if self.body.is_empty() {
            write!(w, "{{}}")
        } else {
            write!(w, "{{\n")?;
            for statement in &self.body {
                statement.write(w, i + 1)?;
            }
            write!(w, "}}")
        }
    }
}

impl Expression {
    /// Print an expression at the provided level of indentation. All expressions with a smaller
    /// precedence than `p` will be wrapped in parentheses.
    fn write<W: io::Write>(&self, w: &mut W, i: usize, p: Precedence) -> io::Result<()> {
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
            ExpressionKind::Identifier(identifier) => identifier.write(w)?,
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
    fn write<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        match &self.0 {
            PatternKind::Identifier(identifier) => identifier.write(w)?,
        }
        Ok(())
    }
}

/// Writes some spaces at the specified indentation level.
fn write_indentation<W: io::Write>(w: &mut W, i: usize) -> io::Result<()> {
    for _ in 0..i {
        write!(w, "  ")?;
    }
    Ok(())
}
