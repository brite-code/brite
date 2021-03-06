use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection};
use crate::language::*;
use crate::parser::{Identifier, Range};
use std::collections::HashMap;
use std::mem;

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

type PrecheckDeclarationMap<'src> = HashMap<Identifier, PrecheckDeclarationLazy<'src>>;

enum PrecheckDeclaration {
    Function(PrecheckFunctionDeclaration),
    Class(PrecheckClassDeclaration),
}

impl PrecheckDeclaration {
    fn resolve_type(&self, reference: &ReferenceType) {
        unimplemented!()
    }
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
enum PrecheckDeclarationLazy<'src> {
    /// The declaration is unchecked or is currently in the process of type checking.
    Unchecked {
        /// The range of the declaration’s name.
        name_range: Range,
        /// The declaration which needs to be checked. If `None` then we are currently type checking
        /// this declaration.
        declaration: Option<&'src Declaration>,
        /// References to the declaration that we find before we finish checking.
        references: Vec<&'src ReferenceType>,
    },
    /// We’ve finished checking the declaration.
    Checked(PrecheckDeclaration),
}

impl<'src> PrecheckDeclarationLazy<'src> {
    fn unchecked(name_range: Range, declaration: &'src Declaration) -> Self {
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
                    let references = mem::replace(references, Vec::new());
                    let declaration = precheck_declaration(diagnostics, declarations, declaration);
                    let declaration_lazy = declarations.get_mut(identifier).unwrap();
                    debug_assert!(declaration_lazy.is_checking());
                    for reference in references {
                        declaration.resolve_type(reference);
                    }
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

/// Checks a type using only the declaration information we have during the precheck stage.
///
/// We aren’t guaranteed to precheck the type or any of its components. If the type depends on an
/// unchecked declaration then we will wait until the declaration has been checked to finish
/// checking the type.
fn precheck_type<'src>(
    diagnostics: &mut DiagnosticsCollection,
    declarations: &mut PrecheckDeclarationMap<'src>,
    type_: &'src Type,
) {
    match type_ {
        Type::Reference(reference) => match declarations.get_mut(&reference.identifier) {
            // If we have a checked declaration then use it to resolve our reference type.
            Some(PrecheckDeclarationLazy::Checked(declaration)) => {
                declaration.resolve_type(reference);
            }

            // If there’s a declaration with the referenced name but the declaration hasn’t been
            // checked yet, add our reference to its internal list so that we can resolve the
            // reference once the declaration has type checked.
            Some(PrecheckDeclarationLazy::Unchecked { references, .. }) => {
                references.push(reference);
            }

            // If there is no declaration with the referenced name then resolve our reference type
            // to an error type.
            None => {
                let error = diagnostics.report(Diagnostic::identifier_not_found(
                    reference.range,
                    reference.identifier.clone(),
                ));
                reference.resolve(error.into());
            }
        },

        Type::This(_) => unimplemented!(),

        // Scalar and error types are ok as they are!
        Type::Resolved(ResolvedType::Scalar(_)) => {}
        Type::Resolved(ResolvedType::Error(_)) => {}

        // Iterate through all the component types of our composite type.
        Type::Resolved(ResolvedType::Composite(composite)) => {
            composite.visit(|type_| precheck_type(diagnostics, declarations, type_));
        }
    }
}
