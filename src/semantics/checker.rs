use super::avt::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection};
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
                ast::Declaration::Function(function) => {
                    (&function.name, ScopeEntryKind::FunctionDeclaration)
                }
                ast::Declaration::Class(class) => (
                    &class.name,
                    ScopeEntryKind::ClassDeclaration { base: class.base },
                ),
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
                    kind: ScopeEntryKind::ClassDeclaration { base },
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
            ast::StatementKind::Binding(_) => unimplemented!(),
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
            ast::ExpressionKind::Constant(constant) => {
                self.check_constant(expression.range, constant)
            }
            ast::ExpressionKind::Reference(_) => unimplemented!(),
            ast::ExpressionKind::This => unimplemented!(),
            ast::ExpressionKind::Function(_) => unimplemented!(),
            ast::ExpressionKind::Call(_) => unimplemented!(),
            ast::ExpressionKind::Construct(_) => unimplemented!(),
            ast::ExpressionKind::Member(_) => unimplemented!(),
            ast::ExpressionKind::Prefix(_) => unimplemented!(),
            ast::ExpressionKind::Infix(_) => unimplemented!(),
            ast::ExpressionKind::Logical(_) => unimplemented!(),
            ast::ExpressionKind::Conditional(_) => unimplemented!(),
            ast::ExpressionKind::Block(block) => self.check_block(block),
            ast::ExpressionKind::Wrapped(_) => unimplemented!(),
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
struct ScopeEntry {
    /// The range of the scope entry’s name.
    range: Range,
    /// The kind of scope entry this is.
    kind: ScopeEntryKind,
}

/// The kind of a [`ScopeEntry`].
enum ScopeEntryKind {
    /// The name references a function declaration.
    FunctionDeclaration,
    /// The name references a class declaration.
    ClassDeclaration { base: bool },
}

impl Scope {
    /// Creates a new scope.
    fn new() -> Self {
        Scope {
            stack: Vec1::new(HashMap::new()),
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
        self.stack.last().get(identifier)
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
