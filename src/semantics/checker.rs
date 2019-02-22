use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection};
use crate::syntax::ast;
use crate::syntax::{Identifier, Range};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
pub struct Checker<'errs, 'ast> {
    /// The collection we report diagnostics to. Please use `Checker::report_diagnostic` instead of
    /// accessing our collection directly.
    _diagnostics: &'errs mut DiagnosticsCollection,
    /// All the identifiers currently in scope and the behavior of each identifier in a program.
    scope: HashMap<Identifier, ScopeEntry<'ast>>,
}

/// The behavior of an identifier in scope.
struct ScopeEntry<'ast> {
    /// The range of the scope entry’s name.
    range: Range,
    /// The kind of scope entry this is.
    kind: ScopeEntryKind<'ast>,
}

enum ScopeEntryKind<'ast> {
    Declaration(Rc<RefCell<CheckerDeclaration<'ast>>>),
}

enum CheckerDeclaration<'ast> {
    Unchecked(&'ast ast::Declaration),
}

impl<'errs, 'ast> Checker<'errs, 'ast> {
    /// Creates a new type checker context.
    pub fn new(diagnostics: &'errs mut DiagnosticsCollection) -> Self {
        Checker {
            _diagnostics: diagnostics,
            scope: HashMap::new(),
        }
    }

    /// Checks an AST module for semantic errors.
    pub fn check_module(&mut self, module: &'ast ast::Module) {
        // A module is made up of many declarations. Type checking declarations is a bit tricky
        // since they’re mutually recursive. (A declaration may reference another declaration
        // ordered _after_ itself in source code.) Here’s how we type check declarations:
        //
        // 1. Add all our declarations to our context in an “unchecked” state.
        // 2. Go through our unchecked declarations list and check them.
        // 3. If we reference a
        let declarations = module
            .declarations
            .iter()
            .map(|declaration| {
                // Get the name of our declaration.
                let name = match declaration {
                    ast::Declaration::Function(function) => &function.name,
                    ast::Declaration::Class(class) => &class.name,
                };
                //
                let declaration = Rc::new(RefCell::new(CheckerDeclaration::Unchecked(declaration)));

                // If we’ve already seen this name then report an error. We’ll still type-check the
                // declaration, but any references will get access to the first declaration we saw.
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
                            kind: ScopeEntryKind::Declaration(Rc::clone(&declaration)),
                        },
                    );
                }
                declaration
            }).collect();
    }

    /// Reports a diagnostic.
    ///
    /// Written so that we may swap out the implementation at any time.
    fn report_diagnostic(&mut self, diagnostic: Diagnostic) -> DiagnosticRef {
        self._diagnostics.report(diagnostic)
    }
}
