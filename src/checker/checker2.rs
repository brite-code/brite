use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection};
use crate::language::*;
use crate::parser::{Identifier, Range};
use std::collections::HashMap;

pub fn precheck_module<'arena>(diagnostics: &mut DiagnosticsCollection, module: &Module<'arena>) {
    let mut declarations: PrecheckDeclarationMap =
        HashMap::with_capacity(module.declarations.len());

    let mut recheck_declarations = Vec::with_capacity(module.declarations.len());

    // Add all the declarations in our module to our `PrecheckDeclarationMap`. If two declarations
    // have the same name then we will report an error diagnostic because that is not allowed!
    //
    // We add a reference to `recheck_declarations` since after we’ve seen all our declarations we
    // will go back and actually type check them all.
    for declaration in &module.declarations {
        let declaration_name = declaration.name();

        let recheck_declaration =
            if let Some(other_declaration) = declarations.get(&declaration_name.identifier) {
                diagnostics.report(Diagnostic::declaration_name_already_used(
                    declaration_name.range,
                    declaration_name.identifier.clone(),
                    other_declaration.name_range(),
                ));
                Err(declaration)
            } else {
                declarations.insert(
                    declaration_name.identifier.clone(),
                    PrecheckDeclarationLazy::unchecked(declaration_name.range, declaration),
                );
                Ok(declaration_name)
            };

        recheck_declarations.push(recheck_declaration);
    }

    // Actually type check our declarations now that we have them all in scope. `Ok` means that the
    // declaration is the only one of its name. `Err` means that there is another declaration with
    // the same name. We check `Err` declarations anyway so the programmer can still see type
    // coverage for the declaration.
    for recheck_declaration in recheck_declarations {
        match recheck_declaration {
            Ok(name) => {
                let result = resolve_precheck_declaration(
                    diagnostics,
                    &mut declarations,
                    &name.range,
                    &name.identifier,
                );
                debug_assert!(
                    result.is_ok(),
                    "The declaration should exist and checking the declaration here should not create a cycle."
                );
            }
            Err(declaration) => {
                precheck_declaration(diagnostics, &mut declarations, declaration);
            }
        }
    }
}

type PrecheckDeclarationMap<'source> = HashMap<Identifier, PrecheckDeclarationLazy<'source>>;

enum PrecheckDeclaration {
    Function(PrecheckFunctionDeclaration),
    Class(PrecheckClassDeclaration),
}

fn precheck_declaration(
    diagnostics: &mut DiagnosticsCollection,
    declarations: &mut PrecheckDeclarationMap,
    declaration: &Declaration,
) -> PrecheckDeclaration {
    match declaration {
        Declaration::Function(declaration) => PrecheckDeclaration::Function(
            precheck_function_declaration(diagnostics, declarations, declaration),
        ),
        Declaration::Class(declaration) => PrecheckDeclaration::Class(precheck_class_declaration(
            diagnostics,
            declarations,
            declaration,
        )),
    }
}

struct PrecheckFunctionDeclaration {
    name_range: Range,
    // name: Identifier,
    // parameters: Vec<(Pattern, Type)>,
    // return_type: Type,
    // body: Block,
}

fn precheck_function_declaration(
    diagnostics: &mut DiagnosticsCollection,
    declarations: &mut PrecheckDeclarationMap,
    declaration: &FunctionDeclaration,
) -> PrecheckFunctionDeclaration {
    // unimplemented!()
    PrecheckFunctionDeclaration {
        name_range: declaration.name.range,
    }
}

struct PrecheckClassDeclaration {
    name_range: Range,
    // name: Identifier,
}

fn precheck_class_declaration(
    diagnostics: &mut DiagnosticsCollection,
    declarations: &mut PrecheckDeclarationMap,
    declaration: &ClassDeclaration,
) -> PrecheckClassDeclaration {
    // unimplemented!()
    PrecheckClassDeclaration {
        name_range: declaration.name.range,
    }
}

/// To type check a module, we first look at all the declarations in our module and add “unchecked“
/// references to the declaration. We then go through each declaration and type check it. If one
/// declaration depends on another (like `class extends`) we will make sure to check that class even
/// if it appears after our own in source code.
enum PrecheckDeclarationLazy<'arena> {
    /// The declaration is unchecked or is currently in the process of type checking.
    Unchecked {
        /// The range of the declaration’s name.
        name_range: Range,
        /// The declaration which needs to be checked. If `None` then we are currently type checking
        /// this declaration.
        declaration: Option<&'arena Declaration<'arena>>,
        /// References to the declaration that we find before we finish checking.
        references: Vec<()>,
    },
    /// We’ve finished checking the declaration.
    Checked(PrecheckDeclaration),
}

impl<'arena> PrecheckDeclarationLazy<'arena> {
    fn unchecked(name_range: Range, declaration: &'arena Declaration<'arena>) -> Self {
        PrecheckDeclarationLazy::Unchecked {
            name_range,
            declaration: Some(declaration),
            references: Vec::new(),
        }
    }

    fn name_range(&self) -> Range {
        match self {
            PrecheckDeclarationLazy::Unchecked { name_range, .. } => *name_range,
            PrecheckDeclarationLazy::Checked(PrecheckDeclaration::Function(declaration)) => {
                declaration.name_range
            }
            PrecheckDeclarationLazy::Checked(PrecheckDeclaration::Class(declaration)) => {
                declaration.name_range
            }
        }
    }

    fn is_checking(&self) -> bool {
        match self {
            PrecheckDeclarationLazy::Unchecked { declaration, .. } => declaration.is_none(),
            _ => false,
        }
    }
}

/// Resolves a precheck declaration. If we are trying to resolve an unchecked declaration then we
/// will check that declaration and return the result. If the declaration does not exist or we try
/// to check ourselves then an error will be returned.
fn resolve_precheck_declaration<'scope>(
    diagnostics: &mut DiagnosticsCollection,
    declarations: &'scope mut PrecheckDeclarationMap,
    range: &Range,
    identifier: &Identifier,
) -> Result<&'scope PrecheckDeclaration, DiagnosticRef> {
    if let Some(declaration_lazy) = declarations.get_mut(identifier) {
        match declaration_lazy {
            // If we’ve already checked this declaration then do nothing...
            PrecheckDeclarationLazy::Checked(_) => {}

            // If we have not yet checked this declaration...
            PrecheckDeclarationLazy::Unchecked {
                name_range,
                declaration,
                references,
            } => {
                // Take the declaration we need to check. This will set the `declaration` field to
                // `None`. That way we can detect circular references.
                if let Some(declaration) = declaration.take() {
                    let declaration = precheck_declaration(diagnostics, declarations, declaration);
                    let declaration_lazy = declarations.get_mut(identifier).unwrap();
                    debug_assert!(declaration_lazy.is_checking());
                    *declaration_lazy = PrecheckDeclarationLazy::Checked(declaration);
                } else {
                    // If we are already checking this declaration then we found a
                    // circular reference!
                    return Err(diagnostics.report(Diagnostic::declaration_cycle_detected(
                        range.clone(),
                        identifier.clone(),
                        name_range.clone(),
                    )));
                }
            }
        }
    } else {
        // Uh oh. We could not find the declaration that was requested.
        return Err(diagnostics.report(Diagnostic::identifier_not_found(
            range.clone(),
            identifier.clone(),
        )));
    }
    match declarations.get(identifier) {
        Some(PrecheckDeclarationLazy::Checked(declaration)) => Ok(declaration),
        _ => unreachable!(),
    }
}

fn check_type(diagnostics: &mut DiagnosticsCollection, type_: &Type) {}
