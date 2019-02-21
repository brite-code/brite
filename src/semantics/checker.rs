use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection};
use crate::syntax::ast;
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
}

impl<'errs> Checker<'errs> {
    /// Creates a new type checker context.
    pub fn new(diagnostics: &'errs mut DiagnosticsCollection) -> Self {
        Checker {
            _diagnostics: diagnostics,
        }
    }

    /// Checks an AST module for semantic errors.
    pub fn check_module(&mut self, module: &ast::Module) {
        let mut named_declarations = HashMap::new();
        let mut unnamed_declarations = Vec::new();

        // Loop through all our declarations to find our declaration names.
        for declaration in &module.declarations {
            let name = match declaration {
                ast::Declaration::Function(function) => &function.name,
                ast::Declaration::Class(class) => &class.name,
            };
            // If we’ve already seen this name then report an error. We’ll still type-check the
            // declaration, but any references will get access to the first declaration we saw.
            if let Some((range, _)) = named_declarations.get(&name.identifier) {
                self.report_diagnostic(Diagnostic::declaration_name_already_used(
                    name.range,
                    name.identifier,
                    *range,
                ));
                unnamed_declarations.push(declaration);
            } else {
                named_declarations.insert(name.identifier, (name.range, declaration));
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
