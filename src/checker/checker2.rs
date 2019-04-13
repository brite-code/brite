use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection};
use crate::language::*;
use crate::parser::{Identifier, Range};
use std::collections::HashMap;

pub fn precheck_module(diagnostics: &mut DiagnosticsCollection, module: &Module) {
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

        let recheck_declaration = if let Some((other_declaration_range, _)) =
            declarations.get(&declaration_name.identifier)
        {
            diagnostics.report(Diagnostic::declaration_name_already_used(
                declaration_name.range,
                declaration_name.identifier.clone(),
                other_declaration_range.clone(),
            ));
            Err(declaration)
        } else {
            declarations.insert(
                declaration_name.identifier.clone(),
                (
                    declaration_name.range,
                    PrecheckDeclarationLazy::Unchecked(declaration),
                ),
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
                let _ = resolve_precheck_declaration(
                    diagnostics,
                    &mut declarations,
                    &name.range,
                    &name.identifier,
                );
            }
            Err(declaration) => {
                precheck_declaration(diagnostics, &mut declarations, declaration);
            }
        }
    }
}

type PrecheckDeclarationMap<'source> =
    HashMap<Identifier, (Range, PrecheckDeclarationLazy<'source>)>;

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
    PrecheckFunctionDeclaration {}
}

struct PrecheckClassDeclaration {
    // name: Identifier,
}

fn precheck_class_declaration(
    diagnostics: &mut DiagnosticsCollection,
    declarations: &mut PrecheckDeclarationMap,
    declaration: &ClassDeclaration,
) -> PrecheckClassDeclaration {
    // unimplemented!()
    PrecheckClassDeclaration {}
}

enum PrecheckDeclarationLazy<'source> {
    Checked(PrecheckDeclaration),
    Checking,
    Unchecked(&'source Declaration),
}

impl<'source> PrecheckDeclarationLazy<'source> {
    fn is_checking(&self) -> bool {
        match self {
            PrecheckDeclarationLazy::Checking => true,
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
    if let Some((declaration_range, declaration_lazy)) = declarations.get_mut(identifier) {
        match declaration_lazy {
            PrecheckDeclarationLazy::Checked(_) => {}
            PrecheckDeclarationLazy::Checking => {
                return Err(diagnostics.report(Diagnostic::declaration_cycle_detected(
                    range.clone(),
                    identifier.clone(),
                    declaration_range.clone(),
                )));
            }
            PrecheckDeclarationLazy::Unchecked(declaration) => {
                let declaration: &Declaration = *declaration;
                *declaration_lazy = PrecheckDeclarationLazy::Checking;
                let declaration = precheck_declaration(diagnostics, declarations, declaration);
                let declaration_lazy = &mut declarations.get_mut(identifier).unwrap().1;
                debug_assert!(declaration_lazy.is_checking());
                *declaration_lazy = PrecheckDeclarationLazy::Checked(declaration);
            }
        }
    } else {
        return Err(diagnostics.report(Diagnostic::identifier_not_found(
            range.clone(),
            identifier.clone(),
        )));
    }
    match declarations.get(identifier) {
        Some((_, PrecheckDeclarationLazy::Checked(declaration))) => Ok(declaration),
        _ => unreachable!(),
    }
}
