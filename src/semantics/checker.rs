use super::types::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection, OperationSnippet};
use crate::syntax::ast;
use crate::syntax::{Identifier, Range};
use crate::utils::vecn::Vec1;
use std::collections::HashMap;

/// Checks the Brite Abstract Syntax Tree (AST) for errors and warnings. Reports diagnostics for any
/// invalid code.
///
/// Deciding what code is invalid is a big part of language design. If you go broad and allow just
/// about any code, then there’s not much a type checker can do to make sure you’re programs are
/// correct and there’s not much a compiler can do to optimize your code. If you go to narrow then
/// your language is really hard to work with.
///
/// It follows that what code is invalid depends a lot on the language runtime. If the language
/// runtime imposes certain constraints on code, then our checker should make sure those constraints
/// are indeed maintained.
pub struct Checker<'errs> {
    /// The collection we report diagnostics to. Please use `Checker::report_diagnostic` instead of
    /// accessing our collection directly.
    _diagnostics: &'errs mut DiagnosticsCollection,
    /// The scope contains all the variables accessible at different points in the program.
    scope: Scope,
}

impl<'errs> Checker<'errs> {
    /// Creates a new type checker context.
    pub fn new(diagnostics: &'errs mut DiagnosticsCollection) -> Self {
        Checker {
            _diagnostics: diagnostics,
            scope: Scope::new(),
        }
    }

    /// Checks an AST module for errors.
    pub fn check_module(&mut self, module: &ast::Module) {
        // Add all our declarations to scope. We need to do this before type checking our
        // declarations because all declarations are mutually recursive.
        for declaration in &module.declarations {
            // Get the name and the scope entry kind of our declaration.
            let (name, entry_kind) = match declaration {
                ast::Declaration::Function(function) => (&function.name, ScopeEntryKind::Function),
                ast::Declaration::Class(class) => {
                    (&class.name, ScopeEntryKind::Class { base: class.base })
                }
            };
            // If we’ve already seen this declaration name then report an error. We’ll still
            // type-check the declaration, but any references will get access to the first
            // declaration we saw.
            if let Some(entry) = self.scope.resolve_maybe(&name.identifier) {
                self.report_diagnostic(Diagnostic::declaration_name_already_used(
                    name.range,
                    name.identifier.clone(),
                    entry.range,
                ));
            } else {
                self.scope.declare(
                    name.identifier.clone(),
                    ScopeEntry {
                        range: name.range,
                        kind: entry_kind,
                    },
                );
            }
        }

        // Now that all of our declarations are in scope, loop through our declaration list again
        // and type check all our declarations.
        for declaration in &module.declarations {
            self.check_declaration(declaration);
        }
    }

    fn check_declaration(&mut self, declaration: &ast::Declaration) {
        match declaration {
            ast::Declaration::Function(function) => self.check_function_declaration(function),
            ast::Declaration::Class(class) => self.check_class_declaration(class),
        }
    }

    fn check_function_declaration(&mut self, function: &ast::FunctionDeclaration) {
        self.check_function(&function.function);
    }

    fn check_function(&mut self, function: &ast::Function) {
        // When checking a function, we want to add parameters to the block. So introduce a level
        // of nesting in the scope.
        self.scope.nest();

        // Add function parameters to our current, nested, scope.
        for _ in &function.parameters {
            unimplemented!()
        }

        // Check our block in the current scope.
        self.check_block_without_nest(&function.body);

        self.scope.unnest();
    }

    fn check_class_declaration(&mut self, class: &ast::ClassDeclaration) {
        // Check to make sure that we are extending a base class.
        if let Some(extends) = &class.extends {
            match self.scope.resolve_name(extends) {
                // If the identifier was not found report our error...
                Err(diagnostic) => {
                    self.report_diagnostic(diagnostic);
                }

                // If the identifier is a base class then yippee skippy!
                Ok(ScopeEntry {
                    kind: ScopeEntryKind::Class { base },
                    ..
                })
                    if *base =>
                {
                    // TODO: Class extension cycle error?
                }

                // If the identifier is not a base class report an error!
                Ok(entry) => {
                    self.report_diagnostic(Diagnostic::can_only_extend_base_class(
                        extends.range,
                        extends.identifier.clone(),
                        entry.range,
                    ));
                }
            }
        }
    }

    fn check_block(&mut self, block: &ast::Block) -> Type {
        self.scope.nest();
        let result = self.check_block_without_nest(block);
        self.scope.unnest();
        result
    }

    /// Checks a block but does not introduce a new level of nesting in the [`Scope`]. Useful for
    /// checking functions which add parameters in their body’s scope.
    fn check_block_without_nest(&mut self, block: &ast::Block) -> Type {
        // The type returned by the block. The last non-empty statement in the block is returned.
        let mut block_type = Type::void(block.range);

        for statement in &block.statements {
            // Skip empty statements entirely.
            if let ast::StatementKind::Empty = &statement.kind {
                continue;
            }
            // Check the statement and assign its type as the last block type.
            block_type = self.check_statement(statement);
        }

        block_type
    }

    fn check_statement(&mut self, statement: &ast::Statement) -> Type {
        match &statement.kind {
            ast::StatementKind::Expression(expression) => self.check_expression(expression),

            ast::StatementKind::Binding(binding) => {
                if let Some(annotation) = &binding.annotation {
                    let type_ = self.check_type(annotation);
                    let type_ = self.check_expression_with_type(
                        OperationSnippet::BindingStatementAnnotation(
                            binding.pattern.snippet(),
                            binding.value.snippet(),
                        ),
                        &binding.value,
                        type_,
                    );
                    self.check_pattern(&binding.pattern, type_);
                } else {
                    let type_ = self.check_expression(&binding.value);
                    self.check_pattern(&binding.pattern, type_);
                }
                Type::void(statement.range)
            }

            ast::StatementKind::Return(_) => unimplemented!(),
            ast::StatementKind::Empty => unimplemented!(),
        }
    }

    fn check_constant(&mut self, range: Range, constant: &ast::Constant) -> Type {
        match constant {
            ast::Constant::Boolean(_) => Type::boolean(range),
            ast::Constant::Integer(ast::IntegerBase::Decimal, _) => Type::number(range),
            ast::Constant::Integer(ast::IntegerBase::Binary, _) => Type::integer(range),
            ast::Constant::Integer(ast::IntegerBase::Hexadecimal, _) => Type::integer(range),
            ast::Constant::Float(_) => Type::float(range),
        }
    }

    fn check_expression(&mut self, expression: &ast::Expression) -> Type {
        match &expression.kind {
            // Check a constant. Provide our range since constants don’t have a range themselves.
            ast::ExpressionKind::Constant(constant) => {
                self.check_constant(expression.range, constant)
            }

            ast::ExpressionKind::Reference(identifier) => {
                match self.scope.resolve(&expression.range, identifier) {
                    // If the identifier was not found report our error and return the unsound
                    // error type.
                    Err(diagnostic) => {
                        Type::error(expression.range, self.report_diagnostic(diagnostic))
                    }

                    Ok(entry) => match &entry.kind {
                        ScopeEntryKind::Type(_) => unimplemented!(),
                        ScopeEntryKind::Function => unimplemented!(),
                        ScopeEntryKind::Class { .. } => unimplemented!(),

                        // If we are referencing a value then return that.
                        ScopeEntryKind::Value(type_) => type_.clone(),
                    },
                }
            }
            ast::ExpressionKind::This => unimplemented!(),
            ast::ExpressionKind::Function(_) => unimplemented!(),
            ast::ExpressionKind::Call(_) => unimplemented!(),
            ast::ExpressionKind::Construct(_) => unimplemented!(),
            ast::ExpressionKind::Member(_) => unimplemented!(),
            ast::ExpressionKind::Prefix(_) => unimplemented!(),
            ast::ExpressionKind::Infix(_) => unimplemented!(),
            ast::ExpressionKind::Logical(_) => unimplemented!(),
            ast::ExpressionKind::Conditional(_) => unimplemented!(),

            // Checking a block is simple.
            ast::ExpressionKind::Block(block) => self.check_block(block),

            // If the wrapped expression does not have an annotation then we may simply check the
            // wrapped expression. If the wrapped expression does have a type annotation then we
            // will check the type annotation and check our wrapped expression with that
            // type annotation.
            ast::ExpressionKind::Wrapped(wrapped) => {
                if let Some(annotation) = &wrapped.annotation {
                    let annotation = self.check_type(annotation);
                    self.check_expression_with_type(
                        OperationSnippet::ExpressionAnnotation(wrapped.expression.snippet()),
                        &wrapped.expression,
                        annotation,
                    )
                } else {
                    self.check_expression(&wrapped.expression)
                }
            }
        }
    }

    /// When we expect an expression to be of a certain type, we don’t just call
    /// [`Self::check_expression`]. Instead we call this function,
    /// [`Self::check_expression_with_type`]. This function will check to make sure that the
    /// expression does indeed match the provided type and will report an error diagnostic if
    /// it doesn’t.
    ///
    /// This function gives us the ability to do type inference for function expression parameters
    /// whose types are immediately known. This function also gives us better error messages.
    ///
    /// Most of the time we will call [`Self::check_expression`] and check if the returned type is
    /// a subtype of our provided type. However, for function expressions we break apart the type.
    fn check_expression_with_type(
        &mut self,
        operation: OperationSnippet,
        expression: &ast::Expression,
        expected: Type,
    ) -> Type {
        match &expression.kind {
            // If we only wrap an expression without adding a type annotation then let’s call
            // `check_expression_with_type` to preserve the benefits of this function.
            ast::ExpressionKind::Wrapped(wrapped) if wrapped.annotation.is_none() => {
                self.check_expression_with_type(operation, &wrapped.expression, expected)
            }

            // We manually write every case in [`ExpressionKind`] instead of using a hole pattern
            // (`_`) so that the Rust compiler will warn us when we add a new case.
            ast::ExpressionKind::Constant(_)
            | ast::ExpressionKind::Reference(_)
            | ast::ExpressionKind::This
            | ast::ExpressionKind::Function(_)
            | ast::ExpressionKind::Call(_)
            | ast::ExpressionKind::Construct(_)
            | ast::ExpressionKind::Member(_)
            | ast::ExpressionKind::Prefix(_)
            | ast::ExpressionKind::Infix(_)
            | ast::ExpressionKind::Logical(_)
            | ast::ExpressionKind::Conditional(_)
            | ast::ExpressionKind::Block(_)
            | ast::ExpressionKind::Wrapped(_) => {
                let actual = self.check_expression(expression);
                let _ = self.subtype(expression.range, operation, &actual, &expected);
                expected
            }
        }
    }

    /// Checks a pattern which is supposed to bind a value with the provided type. If the pattern
    /// is of a different type, say we are trying to bind a number to an object pattern, then we
    /// will report a diagnostic.
    fn check_pattern(&mut self, pattern: &ast::Pattern, type_: Type) {
        match &pattern.kind {
            // Declare a value variable in this scope with the pattern’s binding identifier.
            ast::PatternKind::Binding(identifier) => self.scope.declare(
                identifier.clone(),
                ScopeEntry {
                    range: pattern.range,
                    kind: ScopeEntryKind::Value(type_),
                },
            ),

            ast::PatternKind::Hole => unimplemented!(),
            ast::PatternKind::This => unimplemented!(),
        }
    }

    fn check_type(&mut self, type_: &ast::Type) -> Type {
        match &type_.kind {
            ast::TypeKind::Reference(identifier) => {
                match self.scope.resolve(&type_.range, identifier) {
                    // If the identifier was not found report our error and return the unsound
                    // error type.
                    Err(diagnostic) => Type::error(type_.range, self.report_diagnostic(diagnostic)),

                    Ok(entry) => match &entry.kind {
                        ScopeEntryKind::Value(_) => unimplemented!(),
                        ScopeEntryKind::Function => unimplemented!(),
                        ScopeEntryKind::Class { .. } => unimplemented!(),

                        // If we are referencing a type then return that.
                        ScopeEntryKind::Type(referenced_type) => {
                            let mut referenced_type = referenced_type.clone();
                            referenced_type.range = type_.range;
                            referenced_type
                        }
                    },
                }
            }
            ast::TypeKind::This => unimplemented!(),
            ast::TypeKind::Function(_) => unimplemented!(),
        }
    }

    /// Determines if one type is the subtype of another. If it is determined that the types are
    /// not subtypes then a diagnostic error is reported. Even if an incompatibility is found, we
    /// will continue to try and find as many errors as possible. Returns `Ok` if the two types
    /// uphold the subtyping relationship and returns `Err` with the first diagnostic we reported if
    /// the two types do not uphold the subtyping relationship. In academic literature this
    /// operation is written as `actual <: expected`.
    fn subtype(
        &mut self,
        range: Range,
        operation: OperationSnippet,
        actual: &Type,
        expected: &Type,
    ) -> Result<(), DiagnosticRef> {
        use self::TypeKind::*;
        match (&actual.kind, &expected.kind) {
            // The error type is both the subtype and the supertype of everything. Which totally
            // breaks our type lattice. It is completely unsound and should never appear in a valid
            // program. It is useful for “forgetting” type information when an error occurs.
            (Error(_), _) => Ok(()),
            (_, Error(_)) => Ok(()),

            // The never type is our bottom type and so the subtype of everything but the supertype
            // of nothing.
            (Never, _) => Ok(()),

            // The unknown type is our top type and so the supertype of everything but the subtype
            // of nothing.
            (_, Unknown) => Ok(()),

            // Void is only the subtype of itself.
            (Void, Void) => Ok(()),

            // Boolean is only the subtype of itself.
            (Boolean, Boolean) => Ok(()),

            // Number is the subtype of itself.
            (Number, Number) => Ok(()),

            // Number is the supertype of both integer and float.
            (Integer, Number) => Ok(()),
            (Float, Number) => Ok(()),

            // Integer and float are subtypes of themselves.
            (Integer, Integer) => Ok(()),
            (Float, Float) => Ok(()),

            // Error cases. We don’t use a hole (`_`) because we want the compiler to warn us
            // whenever we are missing a subtyping case.
            (_, Never)
            | (Unknown, _)
            | (Void, _)
            | (Boolean, _)
            | (Number, _)
            | (Integer, _)
            | (Float, _) => Err(self.report_diagnostic(Diagnostic::incompatible_types(
                range,
                operation,
                (actual.range, actual.snippet()),
                (expected.range, expected.snippet()),
            ))),
        }
    }

    /// Reports a diagnostic.
    ///
    /// Written so that we may swap out the implementation at any time.
    fn report_diagnostic(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        self._diagnostics.report(diagnostic)
    }
}

/// The scope of a program contains all the variables accessible at different points in the program.
struct Scope {
    stack: Vec1<HashMap<Identifier, ScopeEntry>>,
}

/// A name bound in our scope.
#[derive(Debug)]
struct ScopeEntry {
    /// The range of the scope entry’s name.
    range: Range,
    /// The kind of scope entry this is.
    kind: ScopeEntryKind,
}

/// The kind of a [`ScopeEntry`].
#[derive(Debug)]
enum ScopeEntryKind {
    /// Some value bound at runtime.
    Value(Type),
    /// A declared type.
    Type(Type),
    /// The name references a function declaration. Very similar to `ScopeEntryKind::Value` with a
    /// function type except we know the exact function which is bound.
    Function,
    /// The name references a class declaration.
    Class { base: bool },
}

impl Scope {
    /// Creates a new scope.
    fn new() -> Self {
        // TODO: Use proper ranges for the prelude.
        // TODO: It should be ok to shadow names in the prelude.
        let mut root = HashMap::new();
        let range = Range::initial();
        insert_root_entry(&mut root, "Never", Type::never(range));
        insert_root_entry(&mut root, "Unknown", Type::unknown(range));
        insert_root_entry(&mut root, "Void", Type::void(range));
        insert_root_entry(&mut root, "Bool", Type::boolean(range));
        insert_root_entry(&mut root, "Num", Type::number(range));
        insert_root_entry(&mut root, "Int", Type::integer(range));
        insert_root_entry(&mut root, "Float", Type::float(range));

        fn insert_root_entry(
            root: &mut HashMap<Identifier, ScopeEntry>,
            name: &'static str,
            type_: Type,
        ) {
            root.insert(
                Identifier::new(name).unwrap(),
                ScopeEntry {
                    range: type_.range,
                    kind: ScopeEntryKind::Type(type_),
                },
            );
        }

        Scope {
            stack: Vec1::new(root),
        }
    }

    /// Add a new level of scope nesting. When [`Scope::unnest`] is called we will remove all
    /// variables added at this nesting level.
    fn nest(&mut self) {
        self.stack.push(HashMap::new());
    }

    /// Remove all variables added in the last level of scope nesting added with [`Scope::nest`].
    /// Never removes variables from the root scope.
    fn unnest(&mut self) {
        self.stack.pop();
    }

    /// Declares an entry in the current scope. If an entry with this name already exists we will
    /// override it.
    ///
    /// If we are in a level of nesting we’ll remove the entry when [`Scope::unnest`] is called.
    fn declare(&mut self, identifier: Identifier, entry: ScopeEntry) {
        self.stack.last_mut().insert(identifier, entry);
    }

    /// Resolves a name in our current scope. If we could not find it then return `None`.
    fn resolve_maybe(&self, identifier: &Identifier) -> Option<&ScopeEntry> {
        for entries in self.stack.iter().rev() {
            if let Some(entry) = entries.get(identifier) {
                return Some(entry);
            }
        }
        None
    }

    /// Resolves a name in our current scope. If we could not find it then return an error.
    fn resolve(&self, range: &Range, identifier: &Identifier) -> Result<&ScopeEntry, Diagnostic> {
        if let Some(entry) = self.resolve_maybe(identifier) {
            Ok(entry)
        } else {
            Err(Diagnostic::identifier_not_found(*range, identifier.clone()))
        }
    }

    /// Resolves a name in our current scope. If we could not find it then return an error.
    fn resolve_name(&self, name: &ast::Name) -> Result<&ScopeEntry, Diagnostic> {
        self.resolve(&name.range, &name.identifier)
    }
}
