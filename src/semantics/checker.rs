use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection};
use crate::syntax::ast;
use crate::syntax::{Identifier, Range};
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
    /// All the identifiers currently in scope and the behavior of each identifier in a program.
    scope: HashMap<Identifier, ScopeEntry>,
}

/// The behavior of an identifier in scope.
struct ScopeEntry {
    /// The range of the scope entry’s name.
    range: Range,
    /// The kind of scope entry this is.
    kind: ScopeEntryKind,
}

/// The kind of a [`ScopeEntry`].
enum ScopeEntryKind {
    /// The entry is a function declaration.
    FunctionDeclaration,
    /// The entry is a class declaration.
    ClassDeclaration { base: bool },
}

impl<'errs> Checker<'errs> {
    /// Creates a new type checker context.
    pub fn new(diagnostics: &'errs mut DiagnosticsCollection) -> Self {
        Checker {
            _diagnostics: diagnostics,
            scope: HashMap::new(),
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
            if let Some(entry) = self.scope.get(&name.identifier) {
                self.report_diagnostic(Diagnostic::declaration_name_already_used(
                    name.range,
                    name.identifier,
                    entry.range,
                ));
            } else {
                self.scope.insert(
                    name.identifier,
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
            match declaration {
                ast::Declaration::Function(_) => {}
                ast::Declaration::Class(class) => {
                    if let Some(extends) = &class.extends {
                        if let Some(_) = self.scope.get(&extends.identifier) {

                        } else {
                            self.report_diagnostic(Diagnostic::identifier_not_found(
                                extends.range,
                                extends.identifier,
                            ));
                        }
                    }
                    // Other stuff...
                }
            }
        }
    }

    /// Reports a diagnostic.
    ///
    /// Written so that we may swap out the implementation at any time.
    fn report_diagnostic(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        self._diagnostics.report(diagnostic)
    }
}
