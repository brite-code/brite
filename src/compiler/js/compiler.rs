use super::ast as js;
use crate::checker::avt::*;

/// Compiles a Brite module into a JavaScript module. Code compiled into JavaScript should have the
/// same behavior as code compiled into another language, like LLVM.
pub fn compile_module(statements: &Vec<Statement>) -> Vec<js::Statement> {
    statements.iter().map(compile_statement).collect()
}

fn compile_statement(statement: &Statement) -> js::Statement {
    match &statement.kind {
        StatementKind::Expression(expression) => {
            js::Statement::expression(compile_expression(expression))
        }
        StatementKind::Binding(binding) => js::Statement::variable_declaration(
            js::VariableDeclarationKind::Const,
            compile_pattern(&binding.pattern),
            compile_expression(&binding.value),
        ),
    }
}

fn compile_expression(expression: &Expression) -> js::Expression {
    match &expression.kind {
        ExpressionKind::Constant(Constant::Boolean(value)) => {
            js::Expression::boolean_literal(*value)
        }

        ExpressionKind::Constant(Constant::Float(value)) => js::Expression::numeric_literal(*value),

        ExpressionKind::Constant(Constant::Integer(_, _)) => unimplemented!(),

        // TODO: Make sure to check, escape, and deduplicate identifiers.
        ExpressionKind::Reference(identifier) => js::Expression::identifier(
            js::Identifier::new_unchecked(identifier.as_str().to_string()),
        ),

        ExpressionKind::Function(_) => unimplemented!(),
        ExpressionKind::Call(_) => unimplemented!(),
        ExpressionKind::Prefix(_) => unimplemented!(),

        ExpressionKind::Logical(logical) => js::Expression::logical(
            match &logical.operator {
                LogicalOperator::And => js::LogicalOperator::And,
                LogicalOperator::Or => js::LogicalOperator::Or,
            },
            compile_expression(&logical.left),
            compile_expression(&logical.right),
        ),

        ExpressionKind::Block(_) => unimplemented!(),
        ExpressionKind::Error(_) => unimplemented!(),

        ExpressionKind::Unimplemented => unimplemented!(),
    }
}

fn compile_pattern(pattern: &Pattern) -> js::Pattern {
    match &pattern.kind {
        // TODO: Make sure to check, escape, and deduplicate identifiers.
        PatternKind::Binding(identifier) => js::Pattern::identifier(js::Identifier::new_unchecked(
            identifier.as_str().to_string(),
        )),
    }
}
