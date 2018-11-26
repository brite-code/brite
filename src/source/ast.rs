//! The Abstract Syntax Tree (AST) for Brite source code. The AST is a literal translation of
//! source code. This means it might not be semantically correct. The AVT is a code
//! representation which ensures semantic correctness.
//!
//! We use an AST for:
//!
//! - Type checking.
//! - Pretty printing.

use super::identifier::Identifier;
use super::number::Number;
use super::position::Range;
use super::token::*;

/// A module represents a single Brite file. A module is made up of any number of declarations
/// or statements.
pub struct Module {
    items: Vec<Item>,
}

impl Module {
    /// Converts a Brite module back into the list of tokens it was parsed from.
    #[cfg(test)]
    pub fn into_tokens(self) -> Vec<Token> {
        let mut tokens = Vec::new();
        for item in self.items {
            item.push_tokens(&mut tokens);
        }
        tokens.shrink_to_fit();
        tokens
    }
}

/// An item in a comma list. Every item except the last must have a comma. The last item may
/// optionally have a trailing comma.
pub struct CommaListItem<T> {
    item: T,
    comma: Option<GlyphToken>,
}

/// A block holds some private lexically scoped `Item`s.
pub struct Block {
    brace_open: GlyphToken,
    items: Vec<Item>,
    brace_close: GlyphToken,
}

/// A Brite source code item is either a declarative `Declaration` whose order does not matter or an
/// imperative `Statement` whose order does matter.
pub enum Item {
    Declaration(Declaration),
    Statement(Statement),
}

impl Item {
    #[cfg(test)]
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        match self {
            Item::Declaration(declaration) => declaration.push_tokens(tokens),
            Item::Statement(statement) => statement.push_tokens(tokens),
        }
    }
}

/// A language construct that specifies properties of an identifier. Declarations are declarative.
/// Their order in source code does not matter.
pub enum Declaration {
    /// `fun f(...) { ... }`
    Function(FunctionDeclaration),
}

impl Declaration {
    #[cfg(test)]
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        match self {
            Declaration::Function(declaration) => declaration.push_tokens(tokens),
        }
    }
}

/// ```ite
/// fun f(...) { ... }
/// fun f(...) -> T { ... }
/// ```
pub struct FunctionDeclaration {
    fun: GlyphToken,
    name: IdentifierToken,
    paren_open: GlyphToken,
    parameters: Vec<CommaListItem<FunctionParameter>>,
    paren_close: GlyphToken,
    return_: Option<FunctionReturn>,
    body: Block,
}

pub struct FunctionParameter {
    pattern: Pattern,
    type_: Option<TypeAnnotation>,
}

pub struct FunctionReturn {
    arrow: GlyphToken,
    type_: Type,
}

impl FunctionDeclaration {
    #[cfg(test)]
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        tokens.push(self.fun.into());
        tokens.push(self.name.into());
        tokens.push(self.paren_open.into());

        tokens.push(self.paren_close.into());
    }
}

enum Statement {
    /// Any `Expression`.
    Expression(Expression),
    /// `let x = E;`
    Binding(BindingStatement),
    /// `x = E;`
    Update(UpdateStatement),
    /// `return;`, `return E;`
    Return(ReturnStatement),
    /// `break;`, `break E;`
    Break(BreakStatement),
}

/// ```ite
/// let x = E;
/// ```
pub struct BindingStatement {
    pattern: Pattern,
    type_: Option<Type>,
    value: Expression,
}

/// ```ite
/// x = E;
/// ```
///
/// An update statement performs a local mutation effect. Update statements may only be executed
/// synchronously in the scope which the variable was defined after it was defined.
pub struct UpdateStatement {
    name: Name,
    properties: Vec<Name>,
    value: Expression,
}

/// ```ite
/// return;
/// return E;
/// ```
pub struct ReturnStatement {
    argument: Option<Expression>,
}

/// ```ite
/// break;
/// break E;
/// ```
pub struct BreakStatement {
    argument: Option<Expression>,
}

pub enum Constant {
    /// `true`, `false`
    Boolean(bool),
    /// `0`, `1`, `-42`, `3.1415`
    Number(Number),
}

pub struct Expression {
    range: Range,
    description: ExpressionDescription,
}

enum ExpressionDescription {
    /// `x`
    Variable(Identifier),
    /// `fun(...) { ... }`
    Function(Function),
    /// `f(...)`
    Call(CallExpression),
    /// Any `Constant`.
    Constant(Constant),
    /// `{p: E, ...}`
    Object(ObjectExpression),
    /// `V[E, ...]`
    Variant(VariantExpression),
    /// `E.p`
    Property(Box<PropertyExpression>),
    /// `!E`, `-E`
    Unary(Box<UnaryExpression>),
    /// `E1 + E2`, `E1 - E2`, `E1 * E2`, `E1 / E2`
    Binary(Box<BinaryExpression>),
    /// `E1 && E2`, `E1 || E2`
    Logical(Box<LogicalExpression>),
    /// `if E { ... } else { ... }`
    Conditional(Box<ConditionalExpression>),
    /// `match E { P -> { ... } }`
    Match(MatchExpression),
    /// `loop { ... }`
    Loop(LoopExpression),
    /// `do { ... }`
    Block(Block),
    /// `(X: T)`
    Annotation(Box<AnnotationExpression>),
}

/// ```ite
/// f(...)
/// ```
pub struct CallExpression {
    callee: Box<Expression>,
    arguments: Vec<Expression>,
}

/// ```ite
/// {p: E, ...}
/// ```
///
/// Objects use the design of records in [Extensible records with scoped labels][1]. There are three
/// primitive operations on records defined by that paper.
///
/// - **Extension:** `{l = e | r}`
/// - **Selection:** `(r.l)`
/// - **Restriction:** `(r - l)`
///
/// Using the primitive operations the paper defines two more common operations.
///
/// - **Update:** `{l := x | r}` is the same as `{l = x | r - l}`
/// - **Rename:** `{l <- m | r}` is the same as `{l = r.m | r - m}`
///
/// In Brite we are going for a convenient syntax in the C tradition. We define different syntax for
/// each operation.
///
/// For **Selection** we use the standard dot access syntax `r.l`.
///
/// For **Extension** we use the paper’s syntax `{l: e | r}`. Except we change the equal glyph (`=`)
/// to a colon (`:`) so that object properties are written consistent everywhere. We don’t expect
/// programmers to use this syntax very often. We expect programmers to use the update statement
/// much more often. So why don’t we use this syntax for updates?
///
/// 1. Consistency with types. In type land `{l: t | r}` represents extension. We’d like for the
///    expression behavior to be consistent.
/// 2. Deep updates. It is much easier to do a deep update with the update statement. `a.b.c.d = x;`
///    as opposed to four nestings of the extension syntax.
///
/// For **Restriction** we use pattern matching. Where in the following example `s` is the
/// restricted object.
///
/// ```ite
/// let {l | s} = r;
/// ```
///
/// For **Update** we use an update statement. Unlike in other languages, this does not mutate `r`.
/// It instead creates a new value of `r` and binds `r` to that new value.
///
/// ```ite
/// r.l = x;
/// ```
///
/// Our update statement also checks that `x` is the same type of `r.l`’s current value which is not
/// a feature of the paper’s update syntax. To achieve identical behavior to the paper you may use
/// restriction and extension.
///
/// ```ite
/// let {l | s} = r;
/// {l: x | s}
/// ```
///
/// For **Rename** we don’t provide any convenient syntax. The programmer is expected to combine
/// restriction and extension themselves.
///
/// ```ite
/// let {l | s} = r;
/// {m: l | s}
/// ```
///
/// [1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf
pub struct ObjectExpression {
    properties: Vec<ObjectExpressionProperty>,
    extension: Option<Box<Expression>>,
}

/// ```ite
/// p: E
/// ```
pub struct ObjectExpressionProperty {
    label: Name,
    value: Expression,
}

/// ```ite
/// V[E, ...]
/// ```
///
/// Variants use the design of variants in [Extensible records with scoped labels][1]. There are
/// three primitive operations on variants defined by that paper.
///
/// - **Injection:** `<l = x>`
/// - **Embedding:** `<l | v>`
/// - **Decomposition:** `(l ∈ v ? _ : _)`
///
/// Injection creates a variant tagged with a label. Decomposition allows for matching based on a
/// variant’s tag. Embedding is only necessary for constructing a variant with a duplicated label.
/// Embedding likely will not be used much.
///
/// For **Injection** we use the syntax `L[x]`.
///
/// For **Decomposition** we use pattern matching.
///
/// ```ite
/// match v {
///   L[x] -> { ... }
///   _ -> { ... }
/// }
/// ```
///
/// We don’t yet have syntax for **Embedding**.
///
/// [1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf
pub struct VariantExpression {
    label: Name,
    items: Vec<Expression>,
}

/// ```ite
/// E.p
/// ```
pub struct PropertyExpression {
    object: Expression,
    property: Name,
}

/// ```ite
/// !E
/// -E
/// ```
///
/// An operation on a single expression.
pub struct UnaryExpression {
    operation: UnaryOperation,
    operand: Expression,
}

pub enum UnaryOperation {
    /// `!`
    Not,
    /// `-`
    Negative,
    /// `+`
    Positive,
}

/// ```ite
/// E1 + E2
/// E1 - E2
/// E1 * E2
/// E1 / E2
/// ```
///
/// An operation on two expressions where both operands must be evaluated. For `LogicalExpression`
/// one of the operands may not be evaluated depending on the value of another operand.
pub struct BinaryExpression {
    operation: BinaryOperation,
    left: Expression,
    right: Expression,
}

pub enum BinaryOperation {
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

/// ```ite
/// E1 && E2
/// E1 || E2
/// ```
///
/// A logical expression on two expressions. Unlike `BinaryExpression` one operand may not be
/// evaluated. For example, in `E1 || E2` if `E1` is true then the result will always be true so we
/// don’t need to evaluate `E2`.
///
/// We split up `BinaryExpression` and `LogicalExpression` to make it very obvious that the
/// evaluation semantics are different.
pub struct LogicalExpression {
    operation: LogicalOperation,
    left: Expression,
    right: Expression,
}

pub enum LogicalOperation {
    /// `&&`
    And,
    /// `||`
    Or,
}

/// ```ite
/// if E { ... } else { ... }
/// ```
pub struct ConditionalExpression {
    test: Expression,
    consequent: Expression,
    alternate: Expression,
}

/// ```ite
/// match E { P -> { ... } }
/// ```
pub struct MatchExpression {
    test: Box<Expression>,
    cases: Vec<MatchExpressionCase>,
}

/// ```ite
/// P -> { ... }
/// P -> if E { ... }
/// ```
pub struct MatchExpressionCase {
    match_: Pattern,
    test: Option<Expression>,
    block: Block,
}

/// ```ite
/// loop { ... }
/// ```
pub struct LoopExpression {
    block: Block,
}

/// ```ite
/// (X: T)
/// ```
pub struct AnnotationExpression {
    expression: Expression,
    type_: Type,
}

pub struct Pattern {
    range: Range,
    description: PatternDescription,
}

pub enum PatternDescription {
    /// `_`
    Hole,
    /// `x`
    Variable(Identifier),
    /// Any `Constant`.
    Constant(Constant),
    /// `{p: P, ...}`
    Object(ObjectPattern),
    /// `V[P, ...]`
    Variant(VariantPattern),
}

/// ```ite
/// {p: P, ...}
/// ```
pub struct ObjectPattern {
    properties: Vec<ObjectPatternProperty>,
    extension: Option<Box<Pattern>>,
}

pub struct ObjectPatternProperty {
    label: Name,
    value: Pattern,
}

/// ```ite
/// V[P, ...]
/// ```
pub struct VariantPattern {
    label: Name,
    items: Vec<Pattern>,
}

pub struct Type {
    range: Range,
    description: TypeDescription,
}

pub enum TypeDescription {
    /// `x`
    Variable(Identifier),
    /// `!`
    Bottom,
    /// `<x> T`, `<x: T> U`, `<x = T> U`
    Quantified(QuantifiedType),
    /// `fun(...) -> T`
    Function(FunctionType),
    /// `{p: T, ...}`
    Object(ObjectType),
    /// `V[T, ...] | ...`
    Variant(VariantType),
}

/// ```ite
/// : T
/// ```
pub struct TypeAnnotation {
    colon: GlyphToken,
    type_: Type,
}

/// ```ite
/// <x> T
/// <x: T> U
/// <x = T> U
/// ```
///
/// In Brite we implement [MLF][1] for our type system which is a higher-order type system. That
/// means we can put polymorphic quantifiers wherever we want. This documentation discusses some of
/// our syntax decisions compared to the syntax the [MLF thesis][1] defines.
///
/// ## Bound Kinds
///
/// MLF has two bound kinds. Flexible and rigid. In the paper flexible bounds are denoted with a
/// less than or equal sign (`≥`) and rigid bounds are denoted with an equal sign (`=`).
///
/// Flexible bounds can be instantiated to any instance. Rigid bounds must preserve quantifiers.
/// This description is technically correct but hard to understand, so let’s try visualizing it.
/// Consider these two functions.
///
/// ```ite
/// let f: fun<F: fun<T>(T) -> T>(F) -> void = ...;
/// let g: fun<F = fun<T>(T) -> T>(F) -> void = ...;
/// ```
///
/// Here `f` takes an identity function with a _flexible_ bound (marked by `:`) and `g` takes an
/// identity function with a _rigid_ bound (marked by `=`). Now consider this.
///
/// ```ite
/// let id: fun<T>(T) -> T = fun(x) { x };
/// let add1: fun(number) -> number = fun(n) { n + 1 };
///
/// f(id); // Ok
/// f(add1); // Ok
///
/// g(id); // Ok
/// g(add1); // Error!!!
/// ```
///
/// Here `fun(number) -> number` is an _instance_ of `fun<T>(T) -> T` which means it can be passed
/// to `f` which has a flexible bound of `fun<T>(T) -> T` but _not_ passed to `g` which has a rigid
/// bound of `fun<T>(T) -> T`.
///
/// ## Inline Quantified Types
///
/// In the MLF type syntax quantified types cannot be written inside monotypes. That is you cannot
/// write `{ id: fun<T>(T): T }` since `<T>` would be a quantification inside the object monotype.
/// You’d have to hoist the quantified type up and rewrite the type as `<F: fun<T>(T): T>{ id: F }`.
///
/// To allow writing inline quantified types all quantified types in positive positions (types not
/// in a function parameter) get hoisted to a flexible bound and all quantified types in negative
/// positions (types in a function parameter) get hoisted to a rigid bound.
///
/// This presentation is introduced in Section 2.6 of the paper [“Qualified Types for MLF”][2].
///
/// [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
/// [2]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/qmlf.pdf
pub struct QuantifiedType {
    bounds: Vec<BoundEntry>,
    type_: Box<Type>,
}

pub struct BoundEntry {
    name: Name,
    bound: Bound,
}

pub struct Bound {
    flexibility: BoundFlexibility,
    type_: Type,
}

pub enum BoundFlexibility {
    /// `:`
    Flexible,
    /// `=`
    Rigid,
}

/// ```ite
/// fun(...) -> T
/// ```
pub struct FunctionType {
    bounds: Option<Vec<BoundEntry>>,
    parameters: Vec<Type>,
    body: Box<Type>,
}

/// ```ite
/// {p: T, ...}
/// ```
pub struct ObjectType {
    properties: Vec<ObjectTypeProperty>,
    extension: Option<Box<Type>>,
}

pub struct ObjectTypeProperty {
    label: Name,
    value: Type,
}

/// ```ite
/// V[T, ...] | ...
/// ```
pub struct VariantType {
    items: Vec<VariantTypeItem>,
    extension: Option<Box<Type>>,
}

pub struct VariantTypeItem {
    label: Name,
    items: Vec<Type>,
}
