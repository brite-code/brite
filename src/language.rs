//! The Abstract Syntax Tree (AST) represents the syntactic source code structure of a
//! Brite program.

use crate::diagnostics::{ExpressionSnippet, PatternSnippet, StatementSnippet, VecSnippet};
use crate::parser::{Document, Identifier, Range};
use crate::utils::lisp::Lisp;
use crate::utils::vecn::Vec2;
use num::BigInt;
use std::cell::RefCell;

/// A name is an identifier with the identifier’s range in source code.
#[derive(Clone, Debug)]
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

/// A class is some associated data and functions.
pub struct ClassDeclaration {
    /// Is this a base class?
    pub base: bool,
    /// The name of a class.
    pub name: Name,
    /// A class may optionally extend a base class.
    pub extends: Option<Name>,
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
    pub value: Type,
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

impl Declaration {
    /// Gets the range for our declaration’s name.
    pub fn name(&self) -> &Name {
        match self {
            Declaration::Function(x) => &x.name,
            Declaration::Class(x) => &x.name,
        }
    }
}

/// A block contains a list of statements which are executed sequentially.
#[derive(Debug)]
pub struct Block {
    /// The range of characters covered by this block.
    pub range: Range,
    /// Statements to be executed in the block.
    pub statements: Vec<Statement>,
}

impl Block {
    /// Gets the range of the brace that opens this block.
    pub fn open_brace_range(&self) -> Range {
        // NOTE: This is currently just an estimation. Since anyone can create a `Block` we aren’t
        // guaranteed that the first character in our range will be the open brace.
        Range::new(self.range.start(), 1)
    }

    /// Gets the range of the statement which returns from this block. If the block is empty then
    /// we return the range of the block itself.
    pub fn return_range(&self) -> Range {
        if let Some(statement) = self.statements.last() {
            statement.range
        } else {
            self.range
        }
    }
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
    /// Returns a value from a block early.
    Return(Option<Expression>),
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

/// A constant value in the programmer’s code.
#[derive(Clone, Debug)]
pub enum Constant {
    /// Either `true` or `false`.
    Boolean(bool),
    /// An integer of arbitrary precision.
    Integer(IntegerBase, BigInt),
    /// A 64-bit floating point number.
    Float(f64),
}

/// The Brite supported integer bases.
#[derive(Clone, Debug)]
pub enum IntegerBase {
    /// Base 2
    Binary,
    /// Base 10
    Decimal,
    /// Base 16
    Hexadecimal,
}

/// Some execution which returns a value.
#[derive(Debug)]
pub struct Expression {
    /// The range of our expression in source code.
    pub range: Range,
    /// What kind of expression is this?
    pub kind: ExpressionKind,
}

/// The kind of an Expression AST node.
#[derive(Debug)]
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
    /// Constructs a class instance with some fields.
    Construct(ConstructExpression),
    /// Accesses a member of a class instance.
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
#[derive(Debug)]
pub struct CallExpression {
    /// The function we want to call.
    pub callee: Box<Expression>,
    /// The arguments we want to call the function with.
    pub arguments: Vec<Expression>,
}

/// Constructs a class instance with some fields.
#[derive(Debug)]
pub struct ConstructExpression {
    /// The class to be constructed.
    pub constructor: Name,
    /// The fields we construct the class with.
    pub fields: Vec<ConstructExpressionField>,
}

/// A field in a [`ConstructExpression`].
#[derive(Debug)]
pub struct ConstructExpressionField {
    /// The name of the class field.
    pub name: Name,
    /// The value of we use for the class field.
    pub value: Expression,
}

/// Accesses a member of a class instance.
#[derive(Debug)]
pub struct MemberExpression {
    /// The object we are accessing a property of.
    pub object: Expression,
    /// The name of the property we are accessing.
    pub property: Name,
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
    /// `-`
    Negative,
    /// `+`
    Positive,
}

/// An operation using infix syntax.
#[derive(Debug)]
pub struct InfixExpression {
    /// The operator which describes this operation.
    pub operator: InfixOperator,
    /// The left-hand-side operand.
    pub left: Expression,
    /// The right-hand-side operand.
    pub right: Expression,
}

/// The operator of an `InfixExpression`.
#[derive(Clone, Debug)]
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
#[derive(Debug)]
pub struct LogicalExpression {
    /// The operator which describes this operation.
    pub operator: LogicalOperator,
    /// The left-hand-side operand.
    pub left: Expression,
    /// The right-hand-side operand.
    pub right: Expression,
}

/// The operator of a `LogicalExpression`.
#[derive(Clone, Debug)]
pub enum LogicalOperator {
    /// `&&`
    And,
    /// `||`
    Or,
}

/// A conditional expression chooses a branch to take based on a test expression.
#[derive(Debug)]
pub struct ConditionalExpressionIf {
    /// The test expression.
    pub test: Expression,
    /// Executes if the test expression is true.
    pub consequent: Block,
    /// Executes if the test expression is false.
    pub alternate: Option<ConditionalExpressionElse>,
}

/// If the test of a [`ConditionalExpressionIf`] fails then we execute this else branch.
#[derive(Debug)]
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

impl ConditionalExpressionIf {
    /// The last block in the conditional expression. Could be the consequent’s block or the
    /// alternate’s block.
    pub fn last_block(&self) -> &Block {
        match &self.alternate {
            None => &self.consequent,
            Some(ConditionalExpressionElse::Else(alternate)) => alternate,
            Some(ConditionalExpressionElse::ElseIf(alternate)) => alternate.last_block(),
        }
    }
}

/// Wraps an expression in parentheses with an optional type annotation.
#[derive(Debug)]
pub struct WrappedExpression {
    /// The expression which was wrapped.
    pub expression: Expression,
    /// A wrapped expression may optionally have a type annotation.
    pub annotation: Option<Type>,
}

impl Expression {
    fn new(range: Range, kind: ExpressionKind) -> Self {
        Expression { range, kind }
    }

    /// Create a logical expression.
    pub fn logical(
        range: Range,
        operator: LogicalOperator,
        left: Expression,
        right: Expression,
    ) -> Self {
        Self::new(
            range,
            ExpressionKind::Logical(Box::new(LogicalExpression {
                operator,
                left,
                right,
            })),
        )
    }

    /// Create an infix expression.
    pub fn infix(
        range: Range,
        operator: InfixOperator,
        left: Expression,
        right: Expression,
    ) -> Self {
        Self::new(
            range,
            ExpressionKind::Infix(Box::new(InfixExpression {
                operator,
                left,
                right,
            })),
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
    /// Binds the value to nothing.
    Hole,
    /// Binds a class instance. May only be used as the first parameter in a method class member.
    This,
}

/// Describes the values which may be assigned to a certain location.
#[derive(Debug)]
pub struct Type {
    /// The range of our type.
    pub range: Range,
    /// What kind of type is this?
    pub kind: TypeKind,
    /// The struct constructor should be private.
    _private: (),
}

/// The kind of a type AST node. Not to be confused with type kinds in a higher-order type system.
#[derive(Debug)]
pub enum TypeKind {
    /// References a type in our project.
    Reference(ReferenceType),
    /// References the current class instance type. If we are in a base class then `this` could be
    /// any of the base class’s children.
    This,
    /// The type of a function. Functions may be passed around just like any other value.
    Function(FunctionType),
}

impl Type {
    /// Do not make this public!
    fn new(range: Range, kind: TypeKind) -> Self {
        Type {
            range,
            kind,
            _private: (),
        }
    }

    pub fn reference(range: Range, identifier: Identifier) -> Self {
        Self::new(range, TypeKind::Reference(ReferenceType::new(identifier)))
    }

    pub fn this(range: Range) -> Self {
        Self::new(range, TypeKind::This)
    }

    pub fn function(range: Range, parameters: Vec<Type>, return_: Type) -> Self {
        Self::new(
            range,
            TypeKind::Function(FunctionType::new(parameters, return_)),
        )
    }
}

/// A reference to some other type in the program.
#[derive(Debug)]
pub struct ReferenceType {
    /// The identifier the programmer wrote to reference their type.
    pub identifier: Identifier,
    /// The type we are referencing. We will not know the actual type until after we check the
    /// program. Until then our reference will be `None`.
    reference: RefCell<Option<Box<Type>>>,
    /// The struct constructor should be private.
    _private: (),
}

impl ReferenceType {
    fn new(identifier: Identifier) -> Self {
        ReferenceType {
            identifier,
            reference: RefCell::new(None),
            _private: (),
        }
    }
}

/// The type of a function. Functions may be passed around just like any other value.
#[derive(Debug)]
pub struct FunctionType {
    /// The types of this function’s parameters.
    pub parameters: Vec<Type>,
    /// The return type of this function.
    pub return_: Box<Type>,
    /// The struct constructor should be private.
    _private: (),
}

impl FunctionType {
    fn new(parameters: Vec<Type>, return_: Type) -> Self {
        FunctionType {
            parameters,
            return_: Box::new(return_),
            _private: (),
        }
    }
}

impl Constant {
    /// Prints an AST constant to a string.
    ///
    /// - Hexadecimal numbers are always printed in uppercase so that all glyphs have a
    ///   consistent height.
    /// - Floats greater than 1e10 are printed in scientific notation.
    pub fn print(&self) -> String {
        match self {
            Constant::Boolean(value) => {
                if *value {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            Constant::Integer(IntegerBase::Decimal, value) => value.to_str_radix(10),
            Constant::Integer(IntegerBase::Binary, value) => format!("0b{}", value.to_str_radix(2)),
            Constant::Integer(IntegerBase::Hexadecimal, value) => {
                format!("0x{}", value.to_str_radix(16).to_uppercase())
            }
            Constant::Float(value) => {
                if *value >= 10_000_000_000. {
                    format!("{:e}", value)
                } else {
                    format!("{}", value)
                }
            }
        }
    }
}

impl Statement {
    /// Gets a snippet of this statement for error message printing.
    pub fn snippet(&self) -> StatementSnippet {
        match &self.kind {
            StatementKind::Expression(expression) => {
                StatementSnippet::Expression(expression.snippet())
            }
            StatementKind::Binding(binding) => {
                StatementSnippet::Binding(binding.pattern.snippet(), binding.value.snippet())
            }
            StatementKind::Return(_) => unimplemented!(),
        }
    }
}

impl Expression {
    /// Gets a snippet of this expression for error message printing.
    pub fn snippet(&self) -> ExpressionSnippet {
        match &self.kind {
            ExpressionKind::Constant(constant) => ExpressionSnippet::Constant(constant.clone()),
            ExpressionKind::Reference(identifier) => {
                ExpressionSnippet::Reference(identifier.clone())
            }
            ExpressionKind::This => unimplemented!(),
            ExpressionKind::Function(function) => {
                ExpressionSnippet::Function(VecSnippet::from_iter(
                    function
                        .parameters
                        .iter()
                        .map(|parameter| parameter.pattern.snippet()),
                ))
            }
            ExpressionKind::Call(call) => ExpressionSnippet::Call(Box::new(call.callee.snippet())),
            ExpressionKind::Construct(_) => unimplemented!(),
            ExpressionKind::Member(_) => unimplemented!(),
            ExpressionKind::Prefix(prefix) => ExpressionSnippet::Prefix(
                prefix.operator.clone(),
                Box::new(prefix.operand.snippet()),
            ),
            ExpressionKind::Infix(_) => unimplemented!(),
            ExpressionKind::Logical(logical) => ExpressionSnippet::Logical(
                Box::new(logical.left.snippet()),
                logical.operator.clone(),
                Box::new(logical.right.snippet()),
            ),
            ExpressionKind::Conditional(_) => unimplemented!(),
            ExpressionKind::Block(_) => ExpressionSnippet::Block,
            ExpressionKind::Wrapped(wrapped) => wrapped.expression.snippet(),
        }
    }
}

impl Pattern {
    /// A snippet of this pattern for error message printing.
    pub fn snippet(&self) -> PatternSnippet {
        match &self.kind {
            PatternKind::Binding(identifier) => PatternSnippet::Binding(identifier.clone()),
            PatternKind::Hole => unimplemented!(),
            PatternKind::This => unimplemented!(),
        }
    }
}

impl Name {
    /// Converts a name into an S-expression for debugging.
    fn lisp(&self, doc: &Document) -> Lisp {
        lisp!("name", self.range.display(doc), &self.identifier)
    }
}

impl Declaration {
    /// Pretty prints a declaration to a lisp-string format with the specified width. We use this
    /// lisp format for debugging purposes only.
    pub fn print_lisp(&self, document: &Document, width: usize) -> String {
        self.lisp(document).print(width)
    }

    /// Converts a declaration into an S-expression for debugging.
    fn lisp(&self, doc: &Document) -> Lisp {
        match self {
            Declaration::Function(function) => function.function.lisp(doc, function.name.lisp(doc)),
            Declaration::Class(class) => class.lisp(doc),
        }
    }
}

impl ClassDeclaration {
    /// Converts a class expression into an S-expression for debugging.
    fn lisp(&self, doc: &Document) -> Lisp {
        let kind = if self.base { "base class" } else { "class" };
        let mut expressions = Vec2::new(kind.into(), self.name.lisp(doc));
        if let Some(extends) = &self.extends {
            expressions.push(lisp!("extends", extends.lisp(doc)));
        }
        for member in &self.members {
            expressions.push(member.lisp(doc));
        }
        Lisp::List(expressions)
    }
}

impl ClassMember {
    /// Converts a class member into an S-expression for debugging.
    fn lisp(&self, doc: &Document) -> Lisp {
        match self {
            ClassMember::Field(field) => {
                lisp!("field", field.name.lisp(doc), field.value.lisp(doc))
            }
            ClassMember::Method(method) => method.function.lisp(doc, method.name.lisp(doc)),
            ClassMember::BaseMethod(method) => {
                let mut expressions = Vec2::new("base fun".into(), method.name.lisp(doc));
                for parameter in &method.parameters {
                    if let Some(annotation) = &parameter.annotation {
                        expressions.push(lisp!(
                            "param",
                            parameter.pattern.lisp(doc),
                            lisp!("type", annotation.lisp(doc))
                        ));
                    } else {
                        expressions.push(lisp!("param", parameter.pattern.lisp(doc)));
                    }
                }
                expressions.push(lisp!("type", method.return_type.lisp(doc)));
                Lisp::List(expressions)
            }
        }
    }
}

impl Function {
    /// Converts a function to a symbolic expression. Accepts a name S-expression parameter for
    /// debugging some name for the function.
    fn lisp(&self, doc: &Document, name: Lisp) -> Lisp {
        let mut expressions = Vec2::new("fun".into(), name);
        for parameter in &self.parameters {
            if let Some(annotation) = &parameter.annotation {
                expressions.push(lisp!(
                    "param",
                    parameter.pattern.lisp(doc),
                    lisp!("type", annotation.lisp(doc))
                ));
            } else {
                expressions.push(lisp!("param", parameter.pattern.lisp(doc)));
            }
        }
        if let Some(return_type) = &self.return_type {
            expressions.push(lisp!("type", return_type.lisp(doc)));
        }
        expressions.push(self.body.lisp(doc));
        Lisp::List(expressions)
    }
}

impl Block {
    /// Converts a block to a symbolic expression.
    fn lisp(&self, doc: &Document) -> Lisp {
        if self.statements.is_empty() {
            lisp!("block")
        } else {
            let mut expressions = Vec::with_capacity(1 + self.statements.len());
            expressions.push("block".into());
            for statement in &self.statements {
                expressions.push(statement.lisp(doc));
            }
            Lisp::List(Vec2::from_vec(expressions))
        }
    }
}

impl Statement {
    /// Converts a statement to a symbolic expression.
    fn lisp(&self, doc: &Document) -> Lisp {
        let range: Lisp = self.range.display(doc).into();
        match &self.kind {
            StatementKind::Expression(expression) => expression.lisp(doc),
            StatementKind::Binding(binding) => {
                if let Some(annotation) = &binding.annotation {
                    lisp!(
                        "let",
                        range,
                        binding.pattern.lisp(doc),
                        lisp!("type", annotation.lisp(doc)),
                        binding.value.lisp(doc)
                    )
                } else {
                    lisp!(
                        "let",
                        range,
                        binding.pattern.lisp(doc),
                        binding.value.lisp(doc)
                    )
                }
            }
            StatementKind::Return(argument) => {
                if let Some(argument) = argument {
                    lisp!("return", range, argument.lisp(doc))
                } else {
                    lisp!("return", range)
                }
            }
        }
    }
}

impl Constant {
    /// Converts a constant to a symbolic expression. The constant’s range may be provided as an
    /// extra parameter.
    fn lisp(&self, range: Lisp) -> Lisp {
        let kind = match self {
            Constant::Boolean(_) => "bool",
            Constant::Integer(IntegerBase::Decimal, _) => "int",
            Constant::Integer(IntegerBase::Binary, _) => "bin",
            Constant::Integer(IntegerBase::Hexadecimal, _) => "hex",
            Constant::Float(_) => "float",
        };
        lisp!(kind, range, self.print())
    }
}

impl Expression {
    /// Converts an expression to a symbolic expression.
    fn lisp(&self, doc: &Document) -> Lisp {
        let range: Lisp = self.range.display(doc).into();
        match &self.kind {
            ExpressionKind::Constant(constant) => constant.lisp(range),
            ExpressionKind::Reference(identifier) => lisp!("var", range, identifier),
            ExpressionKind::This => lisp!("this", range),
            ExpressionKind::Function(function) => function.lisp(doc, range),
            ExpressionKind::Call(call) => {
                let mut expressions = Vec2::new("call".into(), range);
                expressions.push(call.callee.lisp(doc));
                for argument in &call.arguments {
                    expressions.push(argument.lisp(doc));
                }
                Lisp::List(expressions)
            }
            ExpressionKind::Construct(construct) => {
                let mut expressions = Vec2::new("new".into(), range);
                expressions.push(construct.constructor.lisp(doc));
                for field in &construct.fields {
                    expressions.push(lisp!(field.name.lisp(doc), field.value.lisp(doc)));
                }
                Lisp::List(expressions)
            }
            ExpressionKind::Member(member) => {
                // We don’t print the range of a member expression since it should be obvious. We
                // will assert that the range is as we expect instead.
                assert_eq!(member.object.range.union(member.property.range), self.range);
                lisp!("prop", member.object.lisp(doc), member.property.lisp(doc))
            }
            ExpressionKind::Prefix(prefix) => {
                let operator = match &prefix.operator {
                    PrefixOperator::Not => "not",
                    PrefixOperator::Negative => "neg",
                    PrefixOperator::Positive => "pos",
                };
                lisp!(operator, range, prefix.operand.lisp(doc))
            }
            ExpressionKind::Infix(infix) => {
                let operator = match &infix.operator {
                    InfixOperator::Add => "add",
                    InfixOperator::Subtract => "sub",
                    InfixOperator::Multiply => "mul",
                    InfixOperator::Divide => "div",
                    InfixOperator::Remainder => "rem",
                    InfixOperator::Exponent => "exp",
                    InfixOperator::Equals => "eq",
                    InfixOperator::NotEquals => "neq",
                    InfixOperator::LessThan => "lt",
                    InfixOperator::LessThanOrEqual => "lte",
                    InfixOperator::GreaterThan => "gt",
                    InfixOperator::GreaterThanOrEqual => "gte",
                };
                // We don’t print the range of an infix expression since it should be obvious. We
                // will assert that the range is as we expect instead.
                assert_eq!(infix.left.range.union(infix.right.range), self.range);
                lisp!(operator, infix.left.lisp(doc), infix.right.lisp(doc))
            }
            ExpressionKind::Logical(logical) => {
                let operator = match &logical.operator {
                    LogicalOperator::And => "and",
                    LogicalOperator::Or => "or",
                };
                // We don’t print the range of an infix expression since it should be obvious. We
                // will assert that the range is as we expect instead.
                assert_eq!(logical.left.range.union(logical.right.range), self.range);
                lisp!(operator, logical.left.lisp(doc), logical.right.lisp(doc))
            }
            ExpressionKind::Conditional(conditional) => conditional.lisp(doc),
            ExpressionKind::Block(block) => block.lisp(doc),
            ExpressionKind::Wrapped(wrapped) => {
                if let Some(annotation) = &wrapped.annotation {
                    lisp!(
                        "wrap",
                        range,
                        wrapped.expression.lisp(doc),
                        lisp!("type", annotation.lisp(doc))
                    )
                } else {
                    lisp!("wrap", range, wrapped.expression.lisp(doc))
                }
            }
        }
    }
}

impl ConditionalExpressionIf {
    /// Converts a conditional expression to a symbolic expression.
    fn lisp(&self, doc: &Document) -> Lisp {
        match &self.alternate {
            None => lisp!("if", self.test.lisp(doc), self.consequent.lisp(doc)),
            Some(ConditionalExpressionElse::Else(alternate)) => lisp!(
                "if",
                self.test.lisp(doc),
                self.consequent.lisp(doc),
                alternate.lisp(doc)
            ),
            Some(ConditionalExpressionElse::ElseIf(alternate)) => lisp!(
                "if",
                self.test.lisp(doc),
                self.consequent.lisp(doc),
                alternate.lisp(doc)
            ),
        }
    }
}

impl Pattern {
    /// Converts a pattern to a symbolic expression.
    fn lisp(&self, doc: &Document) -> Lisp {
        let range: Lisp = self.range.display(doc).into();
        match &self.kind {
            PatternKind::Binding(identifier) => lisp!("var", range, identifier),
            PatternKind::Hole => lisp!("hole", range),
            PatternKind::This => lisp!("this", range),
        }
    }
}

impl Type {
    /// Converts a type to a symbolic expression.
    fn lisp(&self, doc: &Document) -> Lisp {
        let range: Lisp = self.range.display(doc).into();
        match &self.kind {
            TypeKind::Reference(reference) => lisp!("var", range, &reference.identifier),
            TypeKind::This => lisp!("this", range),
            TypeKind::Function(function) => {
                let mut expressions = Vec::with_capacity(2 + function.parameters.len());
                expressions.push("fun".into());
                for parameter in &function.parameters {
                    expressions.push(lisp!("param", parameter.lisp(doc)));
                }
                expressions.push(function.return_.lisp(doc));
                Lisp::List(Vec2::from_vec(expressions))
            }
        }
    }
}
