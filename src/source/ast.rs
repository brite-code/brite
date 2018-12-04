//! The Abstract Syntax Tree (AST) for Brite source code. The AST is a literal translation of
//! source code. This means it might not be semantically correct. The AVT is a code
//! representation which ensures semantic correctness.
//!
//! We use an AST for:
//!
//! - Type checking.
//! - Pretty printing.

use super::token::*;
use crate::diagnostics::DiagnosticRef;

/// A module represents a single Brite file. A module is made up of any number of declarations
/// or statements.
#[derive(Clone, Debug)]
pub struct Module {
    /// All the items in our module.
    items: Vec<Item>,
    /// The final token in our module.
    end: EndToken,
}

impl Module {
    pub fn new(items: Vec<Item>, end: EndToken) -> Self {
        Module { items, end }
    }

    /// Converts the Brite AST back into the list of tokens it was parsed from. By making sure we
    /// are capable of doing this we ensure that our AST contains all the information provided in
    /// our source document.
    ///
    /// This is important for printing where we’ll want to make sure that we print all comments back
    /// out in the same location they were written. We’ll also want to make sure that we can parse
    /// documentation comments on things like function declarations.
    pub fn into_tokens(self) -> Vec<Token> {
        let mut tokens = Vec::new();
        self.items.push_tokens(&mut tokens);
        tokens.push(self.end.into());
        tokens
    }
}

/// Some error occurred while parsing. Errors must be fixed for a Brite program to be deployed!
/// An error may occur anywhere in our AST.
pub struct Error {
    /// The tokens we skipped before arriving at this error. It’s important that we track
    /// of the skipped tokens so that we can turn our AST back into the tokens list it was
    /// parsed from.
    skipped: Vec<Token>,
    /// Some diagnostic we reported for this error. Usually the first diagnostic if there were a
    /// couple reported for this error location. Each token we skip will log a diagnostic,
    /// for instance.
    diagnostic: DiagnosticRef,
}

impl Error {
    pub fn new(skipped: Vec<Token>, diagnostic: DiagnosticRef) -> Self {
        Error {
            skipped,
            diagnostic,
        }
    }
}

impl PushTokens for Error {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        tokens.extend(self.skipped);
    }
}

/// Whenever we parse a node in our AST we might run into an unexpected token. At this point we go
/// into error recovery mode. This is the result of error recovery. Most of the  data in our AST are
/// wrapped in this.
pub enum Recover<T> {
    /// There was no error. We were able to parse the node without a problem.
    Ok(T),
    /// There was an error while parsing our node, but the error was not fatal. For instance, we
    /// might have seen some unexpected tokens, skipped over them, and then parsed our node.
    Error(Error, T),
    /// There was an error while parsing our node and the error was fatal. We were not able to
    /// recover from the error.
    FatalError(Error),
}

impl<T: PushTokens> PushTokens for Recover<T> {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        match self {
            Recover::Ok(x) => x.push_tokens(tokens),
            Recover::Error(e, x) => {
                e.push_tokens(tokens);
                x.push_tokens(tokens);
            }
            Recover::FatalError(e) => e.push_tokens(tokens),
        }
    }
}

/// A Brite source code item is either a declarative `Declaration` whose order does not matter or an
/// imperative `Statement` whose order does matter.
#[derive(Clone, Debug)]
pub enum Item {
    Statement(Statement),
}

impl PushTokens for Item {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        match self {
            Item::Statement(statement) => statement.push_tokens(tokens),
        }
    }
}

/// ```ite
/// { ... }
/// ```
///
/// A block makes it so the items inside are only accessible in the block.
#[derive(Clone, Debug)]
pub struct Block {
    brace_left: GlyphToken,
    items: Vec<Item>,
    brace_right: GlyphToken,
}

impl Block {
    pub fn new(brace_left: GlyphToken, items: Vec<Item>, brace_right: GlyphToken) -> Self {
        Block {
            brace_left,
            items,
            brace_right,
        }
    }
}

impl PushTokens for Block {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.brace_left.push_tokens(tokens);
        self.items.push_tokens(tokens);
        self.brace_right.push_tokens(tokens);
    }
}

/// Represents some imperative action to be carried out.
#[derive(Clone, Debug)]
pub enum Statement {
    /// `E;`
    Expression(ExpressionStatement),
    /// `let x = E;`
    Binding(BindingStatement),
}

impl Into<Item> for Statement {
    #[inline]
    fn into(self) -> Item {
        Item::Statement(self)
    }
}

impl PushTokens for Statement {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        match self {
            Statement::Expression(expression) => expression.push_tokens(tokens),
            Statement::Binding(binding) => binding.push_tokens(tokens),
        }
    }
}

/// ```ite
/// E;
/// ```
#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    expression: Expression,
    semicolon: Option<GlyphToken>,
}

impl ExpressionStatement {
    pub fn new(expression: Expression, semicolon: Option<GlyphToken>) -> Self {
        ExpressionStatement {
            expression,
            semicolon,
        }
    }
}

impl PushTokens for ExpressionStatement {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.expression.push_tokens(tokens);
        self.semicolon.push_tokens(tokens);
    }
}

/// ```ite
/// let x = E;
/// ```
#[derive(Clone, Debug)]
pub struct BindingStatement {
    let_: GlyphToken,
    pattern: Pattern,
    equals: GlyphToken,
    value: Expression,
    semicolon: Option<GlyphToken>,
}

impl BindingStatement {
    pub fn new(
        let_: GlyphToken,
        pattern: Pattern,
        equals: GlyphToken,
        value: Expression,
        semicolon: Option<GlyphToken>,
    ) -> Self {
        BindingStatement {
            let_,
            pattern,
            equals,
            value,
            semicolon,
        }
    }
}

impl PushTokens for BindingStatement {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.let_.push_tokens(tokens);
        self.pattern.push_tokens(tokens);
        self.equals.push_tokens(tokens);
        self.value.push_tokens(tokens);
        self.semicolon.push_tokens(tokens);
    }
}

/// Any constant value in our program.
#[derive(Clone, Debug)]
pub enum Constant {
    /// `true`, `false`
    Boolean(BooleanConstant),
    /// `0`, `1`, `-42`, `3.1415`
    Number(NumberConstant),
}

impl PushTokens for Constant {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        match self {
            Constant::Boolean(boolean) => boolean.push_tokens(tokens),
            Constant::Number(number) => number.push_tokens(tokens),
        }
    }
}

/// ```ite
/// true
/// false
/// ```
#[derive(Clone, Debug)]
pub struct BooleanConstant {
    token: GlyphToken,
    value: bool,
}

impl BooleanConstant {
    pub fn new(token: GlyphToken, value: bool) -> Self {
        BooleanConstant { token, value }
    }
}

impl Into<Constant> for BooleanConstant {
    fn into(self) -> Constant {
        Constant::Boolean(self)
    }
}

impl Into<Expression> for BooleanConstant {
    fn into(self) -> Expression {
        Expression::Constant(Constant::Boolean(self))
    }
}

impl PushTokens for BooleanConstant {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.token.push_tokens(tokens);
    }
}

/// ```ite
/// 0
/// 1
/// -42
/// 3.1415
/// ```
#[derive(Clone, Debug)]
pub struct NumberConstant {
    token: NumberToken,
}

impl NumberConstant {
    pub fn new(token: NumberToken) -> Self {
        NumberConstant { token }
    }
}

impl Into<Constant> for NumberConstant {
    fn into(self) -> Constant {
        Constant::Number(self)
    }
}

impl Into<Expression> for NumberConstant {
    fn into(self) -> Expression {
        Expression::Constant(Constant::Number(self))
    }
}

impl PushTokens for NumberConstant {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.token.push_tokens(tokens);
    }
}

/// Some instructions our programming language interprets to return a value and possibly perform
/// some side effects.
#[derive(Clone, Debug)]
pub enum Expression {
    /// `C`
    Constant(Constant),
    /// `x`
    Variable(VariableExpression),
    /// `f(...)`
    Call(Box<CallExpression>),
    /// `E.p`
    Property(Box<PropertyExpression>),
    /// `if E { ... } else { ... }`
    Conditional(Box<ConditionalExpression>),
    /// `do { ... }`
    Block(BlockExpression),
    /// `(E)`
    Wrapped(Box<WrappedExpression>),
}

impl PushTokens for Expression {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        match self {
            Expression::Constant(constant) => constant.push_tokens(tokens),
            Expression::Variable(variable) => variable.push_tokens(tokens),
            Expression::Call(call) => call.push_tokens(tokens),
            Expression::Property(property) => property.push_tokens(tokens),
            Expression::Conditional(conditional) => conditional.push_tokens(tokens),
            Expression::Block(block) => block.push_tokens(tokens),
            Expression::Wrapped(wrapped) => wrapped.push_tokens(tokens),
        }
    }
}

/// ```ite
/// x
/// ```
#[derive(Clone, Debug)]
pub struct VariableExpression {
    identifier: IdentifierToken,
}

impl VariableExpression {
    pub fn new(identifier: IdentifierToken) -> Self {
        VariableExpression { identifier }
    }
}

impl Into<Expression> for VariableExpression {
    fn into(self) -> Expression {
        Expression::Variable(self)
    }
}

impl PushTokens for VariableExpression {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.identifier.push_tokens(tokens);
    }
}

/// ```ite
/// f(...)
/// ```
#[derive(Clone, Debug)]
pub struct CallExpression {
    callee: Expression,
    paren_left: GlyphToken,
    arguments: Vec<CommaListItem<Expression>>,
    paren_right: GlyphToken,
}

impl CallExpression {
    pub fn new(
        callee: Expression,
        paren_left: GlyphToken,
        arguments: Vec<CommaListItem<Expression>>,
        paren_right: GlyphToken,
    ) -> Self {
        CallExpression {
            callee,
            paren_left,
            arguments,
            paren_right,
        }
    }
}

impl PushTokens for CallExpression {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.callee.push_tokens(tokens);
        self.paren_left.push_tokens(tokens);
        self.arguments.push_tokens(tokens);
        self.paren_right.push_tokens(tokens);
    }
}

/// ```ite
/// E.p
/// ```
#[derive(Clone, Debug)]
pub struct PropertyExpression {
    object: Expression,
    dot: GlyphToken,
    property: IdentifierToken,
}

impl PropertyExpression {
    pub fn new(object: Expression, dot: GlyphToken, property: IdentifierToken) -> Self {
        PropertyExpression {
            object,
            dot,
            property,
        }
    }
}

impl PushTokens for PropertyExpression {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.object.push_tokens(tokens);
        self.dot.push_tokens(tokens);
        self.property.push_tokens(tokens);
    }
}

/// ```ite
/// if E { ... } else { ... }
/// if E { ... }
/// ```
#[derive(Clone, Debug)]
pub struct ConditionalExpression {
    if_: GlyphToken,
    test: Expression,
    consequent: Block,
    alternate: Option<ConditionalExpressionAlternate>,
}

/// ```ite
/// else { ... }
/// ```
#[derive(Clone, Debug)]
pub struct ConditionalExpressionAlternate {
    else_: GlyphToken,
    block: Block,
}

impl ConditionalExpression {
    pub fn new(
        if_: GlyphToken,
        test: Expression,
        consequent: Block,
        alternate: Option<ConditionalExpressionAlternate>,
    ) -> Self {
        ConditionalExpression {
            if_,
            test,
            consequent,
            alternate,
        }
    }
}

impl ConditionalExpressionAlternate {
    pub fn new(else_: GlyphToken, block: Block) -> Self {
        ConditionalExpressionAlternate { else_, block }
    }
}

impl PushTokens for ConditionalExpression {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.if_.push_tokens(tokens);
        self.test.push_tokens(tokens);
        self.consequent.push_tokens(tokens);
        self.alternate.push_tokens(tokens);
    }
}

impl PushTokens for ConditionalExpressionAlternate {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.else_.push_tokens(tokens);
        self.block.push_tokens(tokens);
    }
}

/// ```ite
/// do { ... }
/// ```
#[derive(Clone, Debug)]
pub struct BlockExpression {
    do_: GlyphToken,
    block: Block,
}

impl BlockExpression {
    pub fn new(do_: GlyphToken, block: Block) -> Self {
        BlockExpression { do_, block }
    }
}

impl PushTokens for BlockExpression {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.do_.push_tokens(tokens);
        self.block.push_tokens(tokens);
    }
}

/// ```ite
/// (E)
/// ```
#[derive(Clone, Debug)]
pub struct WrappedExpression {
    paren_left: GlyphToken,
    expression: Expression,
    paren_right: GlyphToken,
}

impl WrappedExpression {
    pub fn new(paren_left: GlyphToken, expression: Expression, paren_right: GlyphToken) -> Self {
        WrappedExpression {
            paren_left,
            expression,
            paren_right,
        }
    }
}

impl Into<Expression> for WrappedExpression {
    fn into(self) -> Expression {
        Expression::Wrapped(Box::new(self))
    }
}

impl PushTokens for WrappedExpression {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.paren_left.push_tokens(tokens);
        self.expression.push_tokens(tokens);
        self.paren_right.push_tokens(tokens);
    }
}

/// The left hand side of a binding statement. Takes a value and deconstructs it into the parts that
/// make it up. Binding those parts to variable names in scope.
#[derive(Clone, Debug)]
pub enum Pattern {
    /// `_`
    Hole(HolePattern),
    /// `x`
    Variable(VariablePattern),
}

impl PushTokens for Pattern {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        match self {
            Pattern::Hole(hole) => hole.push_tokens(tokens),
            Pattern::Variable(variable) => variable.push_tokens(tokens),
        }
    }
}

/// ```ite
/// _
/// ```
#[derive(Clone, Debug)]
pub struct HolePattern {
    hole: GlyphToken,
}

impl HolePattern {
    pub fn new(hole: GlyphToken) -> Self {
        HolePattern { hole }
    }
}

impl PushTokens for HolePattern {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.hole.push_tokens(tokens);
    }
}

/// ```ite
/// x
/// ```
#[derive(Clone, Debug)]
pub struct VariablePattern {
    identifier: IdentifierToken,
}

impl VariablePattern {
    pub fn new(identifier: IdentifierToken) -> Self {
        VariablePattern { identifier }
    }
}

impl PushTokens for VariablePattern {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.identifier.push_tokens(tokens);
    }
}

/// An item in a comma list with an optional trailing comma.
#[derive(Clone, Debug)]
pub struct CommaListItem<T> {
    item: T,
    comma: Option<GlyphToken>,
}

impl<T> CommaListItem<T> {
    pub fn new(item: T, comma: Option<GlyphToken>) -> Self {
        CommaListItem { item, comma }
    }
}

impl<T: PushTokens> PushTokens for CommaListItem<T> {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        self.item.push_tokens(tokens);
        self.comma.push_tokens(tokens);
    }
}

/* ─── Push Tokens ────────────────────────────────────────────────────────────────────────────── */

/// Helps us implement `Module::into_tokens()`.
///
/// Unrelated to authentication “push tokens” 😛
trait PushTokens {
    fn push_tokens(self, tokens: &mut Vec<Token>) -> ();
}

impl<T: Into<Token>> PushTokens for T {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        tokens.push(self.into());
    }
}

impl<T: PushTokens> PushTokens for Option<T> {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        if let Some(value) = self {
            value.push_tokens(tokens);
        }
    }
}

impl<T: PushTokens> PushTokens for Vec<T> {
    fn push_tokens(self, tokens: &mut Vec<Token>) {
        for item in self {
            item.push_tokens(tokens);
        }
    }
}
