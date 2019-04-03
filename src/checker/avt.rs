//! The Abstract Value Tree (AVT) is a tree representation of our program after it has been type
//! checked. If there was an error in type checking then we insert an error node into the AVT which
//! will crash at runtime. The AVT contains type information to respond to IDE requests and aid
//! in compilation.

use crate::diagnostics::{DiagnosticRef, TypeKindSnippet};
use crate::parser::{Identifier, Range};
use std::rc::Rc;

pub use crate::parser::ast::{Constant, IntegerBase, LogicalOperator};

/// A function describes some reusable code which may be executed at any time. There are many places
/// in our code where a function may be written.
///
/// - As a `FunctionDeclaration`.
/// - As a `ClassMethodMember`.
/// - As a `FunctionExpression`.
#[derive(Debug)]
pub struct Function {
    /// The parameters of a function describes what the function accepts as input.
    pub parameters: Vec<FunctionParameter>,
    /// The programmer may optionally write a return type. The return type is inferred if it is
    /// not explicit.
    pub return_type: Option<Type>,
    /// The code to be executed when the function is called.
    pub body: Block,
}

/// An input to a function.
#[derive(Debug)]
pub struct FunctionParameter {
    /// The pattern which we match against a function parameter against.
    pub pattern: Pattern,
    /// The type of our function parameter. Most function parameters must be annotated and may not
    /// be inferred.
    pub annotation: Option<Type>,
}

/// A block contains a list of statements which are executed sequentially.
#[derive(Debug)]
pub struct Block {
    /// The range of characters covered by this block.
    pub range: Range,
    /// Statements to be executed in the block.
    pub statements: Vec<Statement>,
}

/// A statement describes some action to be executed in the current scope.
#[derive(Debug)]
pub struct Statement {
    /// The range of our statement in source code.
    pub range: Range,
    /// What kind of statement is this?
    pub kind: StatementKind,
}

/// The kind of a Statement AST node.
#[derive(Debug)]
pub enum StatementKind {
    /// Executes an expression only for the side effects.
    Expression(Expression),
    /// Binds a value to some names in the current scope.
    Binding(BindingStatement),
}

/// Binds a value to some names in the current scope.
#[derive(Debug)]
pub struct BindingStatement {
    /// Binds the value to this pattern in the current scope.
    pub pattern: Pattern,
    /// An optional type annotation. If a type annotation is not added then the type is inferred.
    pub annotation: Option<Type>,
    /// The value being bound.
    pub value: Expression,
}

/// Some execution which returns a value.
#[derive(Debug)]
pub struct Expression {
    /// The range of our expression in source code.
    pub range: Range,
    /// What kind of expression is this?
    pub kind: ExpressionKind,
}

/// The kind of an Expression AVT node.
#[derive(Debug)]
pub enum ExpressionKind {
    /// A constant value in the programmer’s code.
    Constant(Constant),
    /// References a variable bound in this expression’s scope.
    Reference(Identifier),
    /// A higher-order function.
    Function(Function),
    /// Calls a function with some arguments.
    Call(CallExpression),
    /// An operation using prefix syntax.
    Prefix(Box<PrefixExpression>),
    /// A logical operation using infix syntax.
    Logical(Box<LogicalExpression>),
    /// Embeds a block into an expression.
    Block(Block),
    /// Wraps an expression in parentheses with an optional type annotation.
    Wrapped(Box<WrappedExpression>),
    /// When the type checker fails we insert an error expression which will panic at runtime.
    Error(ErrorExpression),
}

/// Calls a function with some arguments.
#[derive(Debug)]
pub struct CallExpression {
    /// The function we want to call.
    pub callee: Box<Expression>,
    /// The arguments we want to call the function with.
    pub arguments: Vec<Expression>,
}

/// An operation using prefix syntax.
#[derive(Debug)]
pub struct PrefixExpression {
    /// The operator which describes this operation.
    pub operator: PrefixOperator,
    /// The operand we are performing the operation on.
    pub operand: Expression,
}

/// The operator of a `PrefixExpression`.
#[derive(Clone, Debug)]
pub enum PrefixOperator {
    /// `!`
    Not,
}

/// A logical operation using infix syntax.
///
/// These operators are separate from `InfixExpression` because logical operators may only
/// conditionally execute their second argument. It’s easy to get this confused with
/// `InfixExpression` which always unconditionally executes both arguments.
#[derive(Debug)]
pub struct LogicalExpression {
    /// The operator which describes this operation.
    pub operator: LogicalOperator,
    /// The left-hand-side operand.
    pub left: Expression,
    /// The right-hand-side operand.
    pub right: Expression,
}

/// Wraps an expression in parentheses with an optional type annotation.
#[derive(Debug)]
pub struct WrappedExpression {
    /// The expression which was wrapped.
    pub expression: Expression,
    /// A wrapped expression may optionally have a type annotation.
    pub annotation: Type,
}

/// When the type checker fails we insert an error expression which will panic at runtime.
#[derive(Debug)]
pub struct ErrorExpression {
    /// The diagnostic our error expression failed with.
    pub error: DiagnosticRef,
    /// The underlying expression the type checker erred on.
    pub expression: Option<Box<Expression>>,
}

impl Expression {
    fn new(range: Range, kind: ExpressionKind) -> Self {
        Expression { range, kind }
    }

    /// Creates a reference expression.
    pub fn reference(range: Range, identifier: Identifier) -> Self {
        Self::new(range, ExpressionKind::Reference(identifier))
    }

    /// Creates an error expression.
    pub fn error(range: Range, error: DiagnosticRef, expression: Option<Expression>) -> Self {
        Self::new(
            range,
            ExpressionKind::Error(ErrorExpression {
                error,
                expression: expression.map(Box::new),
            }),
        )
    }
}

/// A pattern is used for binding a value to some names in the current block scope.
#[derive(Debug)]
pub struct Pattern {
    /// The range of our pattern.
    pub range: Range,
    /// What kind of pattern is this?
    pub kind: PatternKind,
}

/// The kind of a pattern AST node.
#[derive(Debug)]
pub enum PatternKind {
    /// Binds the value to an identifier name in scope.
    Binding(Identifier),
}

/// Describes the values which may be assigned to a particular binding.
///
/// NOTE: The implementation of [`Clone`] needs to stay fairly cheap since we’ll clone a type
/// whenever it is referenced. (Which is itself a form of type reuse.)
#[derive(Clone, Debug)]
pub enum Type {
    /// A normal type which was produced by some bit of valid code. These types will behave soundly
    /// in our type lattice unlike the error type which is unsound.
    ///
    /// We split up “ok” types and “error” types one level above [`TypeKind`] as a very clear
    /// reminder that the programmer must explicitly handle the error case differently from the
    /// happy path.
    Ok {
        /// The range of our type in source code.
        range: Range,
        /// What kind of type is this?
        kind: TypeKind,
    },

    /// The error type exists as an unsound “any” type. It is both the subtype of everything _and_
    /// the supertype of everything combining the behaviors of both the bottom and top types. Of
    /// course this is completely unsound which is why the error type should never exist in a valid
    /// Brite program.
    ///
    /// Error types always carry around the diagnostic which created them. This is important for
    /// figuring out what to blame for the source of an error type.
    Error {
        /// What caused this error type to be created?
        error: DiagnosticRef,
    },
}

// TODO: Having never, void, and null is a bit much. How can we trim it down to maybe one or
// two concepts?

/// The kind of a type.
#[derive(Clone, Debug)]
pub enum TypeKind {
    /// No value that exists at runtime may ever be typed as `Never`. The name comes from the fact
    /// that this type will “never” be reachable at runtime. This is the bottom type in our system.
    /// Written as ⊥ in academic literature.
    ///
    /// We don’t have a top type (written as ⊤ in academic literature) because that would imply
    /// there are operations we may perform on all values. This isn’t true. It would also, probably,
    /// require a dynamic size check which means we couldn’t optimize using types of non-standard
    /// sizes (like zero sized types).
    Never,
    /// Type with only one value, void. This is the “unit” type for Brite.
    Void,
    /// A boolean can either be the value true or false.
    Boolean,
    /// A number is any numeric type. Like an integer or a float. The number type is a supertype of
    /// both integers and floats.
    Number,
    /// An integer is a 32-bit integer type. It’s 32 bits because we need to compile to JavaScript
    /// and that’s the largest integer type we have in JavaScript.
    Integer,
    /// A float is a 64-bit floating point type based on [IEEE 754][1].
    ///
    /// [1]: https://en.wikipedia.org/wiki/IEEE_754
    Float,
    /// The type of a function. Functions may be passed around just like any other value.
    Function(Rc<FunctionType>),
}

/// The type of a function. Functions may be passed around just like any other value.
#[derive(Clone, Debug)]
pub struct FunctionType {
    /// The types of this function’s parameters.
    pub parameters: Vec<Type>,
    /// The return type of this function.
    pub return_: Box<Type>,
}

impl Type {
    /// Creates a never type.
    pub fn never(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Never,
        }
    }

    /// Creates an error type.
    pub fn error(error: DiagnosticRef) -> Self {
        Type::Error { error }
    }

    /// Creates a void type.
    pub fn void(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Void,
        }
    }

    /// Creates a boolean type.
    pub fn boolean(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Boolean,
        }
    }

    /// Creates a number type.
    pub fn number(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Number,
        }
    }

    /// Creates an integer type.
    pub fn integer(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Integer,
        }
    }

    /// Creates a float type.
    pub fn float(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Float,
        }
    }

    /// Creates a function type.
    pub fn function(range: Range, parameters: Vec<Type>, return_: Type) -> Self {
        Self::from_function(range, FunctionType::new(parameters, return_))
    }

    /// Creates a function type from a `FunctionType`.
    pub fn from_function(range: Range, function: FunctionType) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Function(Rc::new(function)),
        }
    }
}

impl FunctionType {
    pub fn new(parameters: Vec<Type>, return_: Type) -> Self {
        FunctionType {
            parameters,
            return_: Box::new(return_),
        }
    }
}

impl TypeKind {
    /// Gets a snippet of a type for error reporting.
    pub fn snippet(&self) -> TypeKindSnippet {
        match self {
            TypeKind::Never => TypeKindSnippet::Never,
            TypeKind::Void => TypeKindSnippet::Void,
            TypeKind::Boolean => TypeKindSnippet::Boolean,
            TypeKind::Number => TypeKindSnippet::Number,
            TypeKind::Integer => TypeKindSnippet::Integer,
            TypeKind::Float => TypeKindSnippet::Float,
            TypeKind::Function(_) => TypeKindSnippet::Function,
        }
    }
}

use crate::parser::ast;

pub fn simple_block_conversion(block: &ast::Block) -> Block {
    Block {
        range: block.range,
        statements: block
            .statements
            .iter()
            .map(simple_statement_conversion)
            .collect(),
    }
}

pub fn simple_statement_conversion(statement: &ast::Statement) -> Statement {
    let kind = match &statement.kind {
        ast::StatementKind::Expression(expression) => {
            StatementKind::Expression(simple_expression_conversion(expression))
        }
        ast::StatementKind::Binding(binding) => {
            if let Some(_) = &binding.annotation {
                unimplemented!();
            }
            StatementKind::Binding(BindingStatement {
                pattern: simple_pattern_conversion(&binding.pattern),
                annotation: None,
                value: simple_expression_conversion(&binding.value),
            })
        }
        ast::StatementKind::Return(_) => unimplemented!(),
    };
    Statement {
        range: statement.range,
        kind,
    }
}

pub fn simple_expression_conversion(expression: &ast::Expression) -> Expression {
    let kind = match &expression.kind {
        ast::ExpressionKind::Constant(constant) => ExpressionKind::Constant(constant.clone()),
        ast::ExpressionKind::Reference(identifier) => ExpressionKind::Reference(identifier.clone()),
        ast::ExpressionKind::This => unimplemented!(),
        ast::ExpressionKind::Function(_) => unimplemented!(),
        ast::ExpressionKind::Call(_) => unimplemented!(),
        ast::ExpressionKind::Construct(_) => unimplemented!(),
        ast::ExpressionKind::Member(_) => unimplemented!(),
        ast::ExpressionKind::Prefix(_) => unimplemented!(),
        ast::ExpressionKind::Infix(_) => unimplemented!(),
        ast::ExpressionKind::Logical(logical) => {
            ExpressionKind::Logical(Box::new(LogicalExpression {
                operator: logical.operator.clone(),
                left: simple_expression_conversion(&logical.left),
                right: simple_expression_conversion(&logical.right),
            }))
        }
        ast::ExpressionKind::Conditional(_) => unimplemented!(),
        ast::ExpressionKind::Block(_) => unimplemented!(),
        ast::ExpressionKind::Wrapped(_) => unimplemented!(),
    };
    Expression {
        range: expression.range,
        kind,
    }
}

pub fn simple_pattern_conversion(pattern: &ast::Pattern) -> Pattern {
    let kind = match &pattern.kind {
        ast::PatternKind::Binding(identifier) => PatternKind::Binding(identifier.clone()),
        ast::PatternKind::Hole => unimplemented!(),
        ast::PatternKind::This => unimplemented!(),
    };
    Pattern {
        range: pattern.range,
        kind,
    }
}
