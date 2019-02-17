use super::document::Range;
use super::source::Identifier;
use num::BigInt;

/// A name is an identifier with the identifier’s range in source code.
pub struct Name {
    /// The range in source code where the identifier appears.
    pub range: Range,
    /// The identifier for this name.
    pub identifier: Identifier,
}

/// A Brite module is a list of declarations. The order of the declarations does not matter.
pub struct Module {
    /// The declarations which make up our module.
    pub declarations: Vec<Declaration>,
}

/// A declaration describes the properties of some identifier.
pub enum Declaration {
    /// A function describes some reusable code which may be executed at any time.
    Function(FunctionDeclaration),
    /// A class is some associated data and functions.
    Class(ClassDeclaration),
}

/// A function describes some reusable code which may be executed at any time.
pub struct FunctionDeclaration {
    /// The name of a function declaration.
    pub name: Name,
    /// Shared function node.
    pub function: Function,
}

/// A function describes some reusable code which may be executed at any time. There are many places
/// in our code where a function may be written.
///
/// - As a `FunctionDeclaration`.
/// - As a `ClassMethodMember`.
/// - As a `FunctionExpression`.
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
pub struct FunctionParameter {
    /// The pattern which we match against a function parameter against.
    pub pattern: Pattern,
    /// The type of our function parameter. Most function parameters must be annotated and may not
    /// be inferred.
    pub type_: Option<Type>,
}

/// A class is some associated data and functions.
pub struct ClassDeclaration {
    /// Is this a base class?
    pub base: bool,
    /// The name of a class.
    pub name: Name,
    /// A class may optionally extend a base class.
    pub extends: Option<Type>,
    /// The members of a class.
    pub members: Vec<ClassMember>,
}

/// A single member of a class. Either data (field) or a function (method).
pub enum ClassMember {
    /// A field declares some data needed by the class.
    Field(FieldClassMember),
    /// A method declares some function behavior that a class may perform.
    Method(MethodClassMember),
    /// A base method is a function which may be overriden in a class which extends the current one.
    BaseMethod(BaseMethodClassMember),
}

/// A field declares some data needed by the class.
pub struct FieldClassMember {
    /// The name of the class field.
    pub name: Name,
    /// The type of the class field’s data.
    pub type_: Type,
}

/// A method declares some function behavior that a class may perform.
pub struct MethodClassMember {
    /// The name of the class method.
    pub name: Name,
    /// Shared function node.
    pub function: Function,
}

/// A base method is a function which may be overriden in a class which extends the current one.
pub struct BaseMethodClassMember {
    /// The name of the base class method.
    pub name: Name,
    /// The parameters which the base method’s implementation must accept.
    pub parameters: Vec<FunctionParameter>,
    /// The type which the base method’s implementation must return.
    pub return_type: Type,
}

/// A block contains a list of statements which are executed sequentially.
pub struct Block {
    /// Statements to be executed in the block.
    pub statements: Vec<Statement>,
}

/// A statement describes some action to be executed in the current scope.
pub enum Statement {
    /// Executes an expression only for the side effects.
    Expression(Expression),
    /// Binds a value to some names in the current scope.
    Binding(BindingStatement),
    /// Returns a value from a block early.
    Return(Option<Expression>),
    /// An empty statement does nothing. Exists only to avoid syntax errors.
    Empty,
}

/// Binds a value to some names in the current scope.
pub struct BindingStatement {
    /// Binds the value to this pattern in the current scope.
    pub pattern: Pattern,
    /// An optional type annotation. If a type annotation is not added then the type is inferred.
    pub annotation: Option<Type>,
    /// The value being bound.
    pub value: Expression,
}

/// A constant value in the programmer’s code.
pub enum Constant {
    /// Either `true` or `false`.
    Boolean(bool),
    /// An integer of arbitrary precision.
    Integer(IntegerBase, BigInt),
    /// A 64-bit floating point number.
    Float(f64),
}

/// The Brite supported integer bases.
pub enum IntegerBase {
    /// Base 2
    Binary,
    /// Base 10
    Decimal,
    /// Base 16
    Hexadecimal,
}

/// Some execution which returns a value.
pub struct Expression {
    /// The range of our expression in source code.
    pub range: Range,
    /// What kind of expression is this?
    pub kind: ExpressionKind,
}

/// The kind of an Expression AST node.
pub enum ExpressionKind {
    /// A constant value in the programmer’s code.
    Constant(Constant),
    /// References a variable bound in this expression’s scope.
    Reference(Identifier),
    /// A special reference to the current class instance.
    This,
    /// A higher-order function.
    Function(Function),
    /// Calls a function with some arguments.
    Call(CallExpression),
    /// Accesses the member of a class instance.
    Member(Box<MemberExpression>),
    /// An operation using prefix syntax.
    Prefix(Box<PrefixExpression>),
    /// An operation using infix syntax.
    Infix(Box<InfixExpression>),
    /// A logical operation using infix syntax.
    Logical(Box<LogicalExpression>),
    /// A conditional expression chooses a branch to take based on a test expression.
    Conditional(Box<ConditionalExpressionIf>),
    /// Embeds a block into an expression.
    Block(Block),
    /// Wraps an expression in parentheses with an optional type annotation.
    Wrapped(Box<WrappedExpression>),
}

/// Calls a function with some arguments.
pub struct CallExpression {
    /// The function we want to call.
    pub callee: Box<Expression>,
    /// The arguments we want to call the function with.
    pub arguments: Vec<Expression>,
}

/// Accesses the member of a class instance.
pub struct MemberExpression {
    /// The object we are accessing a property of.
    pub object: Expression,
    /// The name of the property we are accessing.
    pub property: Identifier,
}

/// An operation using prefix syntax.
pub struct PrefixExpression {
    /// The operator which describes this operation.
    pub operator: PrefixOperator,
    /// The operand we are performing the operation on.
    pub operand: Expression,
}

/// The operator of a `PrefixExpression`.
pub enum PrefixOperator {
    /// `!`
    Not,
    /// `-`
    Negative,
    /// `+`
    Positive,
}

/// An operation using infix syntax.
pub struct InfixExpression {
    /// The operator which describes this operation.
    pub operator: InfixOperator,
    /// The left-hand-side operand.
    pub left: Expression,
    /// The right-hand-side operand.
    pub right: Expression,
}

/// The operator of an `InfixExpression`.
pub enum InfixOperator {
    /// `+`
    Add,
    /// `-`
    Subtract,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `%`
    Remainder,
    /// `^`
    Exponent,
    /// `==`
    Equals,
    /// `!=`
    NotEquals,
    /// `<`
    LessThan,
    /// `<=`
    LessThanOrEqual,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanOrEqual,
}

/// A logical operation using infix syntax.
///
/// These operators are separate from `InfixExpression` because logical operators may only
/// conditionally execute their second argument. It’s easy to get this confused with
/// `InfixExpression` which always unconditionally executes both arguments.
pub struct LogicalExpression {
    /// The operator which describes this operation.
    pub operator: LogicalOperator,
    /// The left-hand-side operand.
    pub left: Expression,
    /// The right-hand-side operand.
    pub right: Expression,
}

/// The operator of a `LogicalExpression`.
pub enum LogicalOperator {
    /// `&&`
    And,
    /// `||`
    Or,
}

/// ```ite
/// if E {
///   // ...
/// }
/// ```
pub struct ConditionalExpressionIf {
    /// The test expression.
    pub test: Expression,
    /// Executes if the test expression is true.
    pub consequent: Block,
    /// Executes if the test expression is false.
    pub alternate: Option<ConditionalExpressionElse>,
}

/// ```ite
/// else {
///   // ...
/// }
/// ```
pub enum ConditionalExpressionElse {
    /// ```ite
    /// else {
    ///   // ...
    /// }
    /// ```
    Else(Block),
    /// ```ite
    /// else if E {
    ///   // ...
    /// }
    /// ```
    ElseIf(Box<ConditionalExpressionIf>),
}

/// Wraps an expression in parentheses with an optional type annotation.
pub struct WrappedExpression {
    /// The expression which was wrapped.
    pub expression: Expression,
    /// A wrapped expression may optionally have a type annotation.
    pub annotation: Option<Type>,
}

/// A pattern is used for binding a value to some names in the current block scope.
pub struct Pattern {
    /// The range of our pattern.
    pub range: Range,
    /// What kind of pattern is this?
    pub kind: PatternKind,
}

/// The kind of a pattern AST node.
pub enum PatternKind {
    /// Binds the value to an identifier name in scope.
    Binding(Identifier),
    /// Binds the value to nothing.
    Hole,
    /// Binds a class instance. May only be used as the first parameter in a method class member.
    This,
}

/// Describes the values which may be assigned to a certain location.
pub struct Type {
    /// The range of our type.
    pub range: Range,
    /// What kind of type is this?
    pub kind: TypeKind,
}

/// The kind of a type AST node. Not to be confused with type kinds in a higher-order type system.
pub enum TypeKind {
    /// References a type in our project.
    Reference(Identifier),
    /// References the current class instance type. If we are in a base class then `this` could be
    /// any of the base class’s children.
    This,
    // TODO
    Function,
}
