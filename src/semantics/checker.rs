use super::types::*;
use crate::diagnostics::{Diagnostic, DiagnosticRef, DiagnosticsCollection, OperationSnippet};
use crate::syntax::ast;
use crate::syntax::{Identifier, Range};
use crate::utils::vecn::Vec1;
use std::cmp;
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

    /// Checks a function and returns the type of the function.
    fn check_function(&mut self, function: &ast::Function) -> FunctionType {
        // When checking a function, we want to add parameters to the block. So introduce a level
        // of nesting in the scope.
        self.scope.nest();

        // Create our parameter types vector which we will push to as we type-check parameters.
        let mut parameter_types = Vec::with_capacity(function.parameters.len());

        // Add function parameters to our current, nested, scope.
        for parameter in &function.parameters {
            // Get the type for all of our function parameters. If a function parameter is missing
            // an annotation then we will report an error and will create an unsound error type.
            let type_ = match &parameter.annotation {
                Some(annotation) => self.check_type(annotation),
                None => Type::error(
                    parameter.pattern.range,
                    self.report_diagnostic(Diagnostic::missing_function_parameter_type(
                        parameter.pattern.range,
                        parameter.pattern.snippet(),
                    )),
                ),
            };
            // Check the pattern with this parameter’s type annotation.
            self.check_pattern(&parameter.pattern, type_.clone());
            // Add the type of this parameter to our vector.
            parameter_types.push(type_);
        }

        // Get the return type for our function. If our return type was annotated then we need to
        // check that annotation against the function body. If our return type was not annotated
        // then use the inferred type of the function’s body.
        let return_type = match &function.return_type {
            // Check our annotated return type against the body of our function.
            Some(return_type) => {
                let return_type = self.check_type(return_type);
                let operation = OperationSnippet::FunctionReturnAnnotation(
                    function.body.statements.last().map(ast::Statement::snippet),
                );
                self.check_block_without_nest(&function.body, Some((operation, return_type)))
            }

            // Infer a type based on our function body and return that.
            None => self.check_block_without_nest(&function.body, None),
        };

        // Leave the scope we created for this function.
        self.scope.unnest();

        // Return a function type.
        FunctionType::new(parameter_types, return_type)
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
                }) if *base => {
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

    fn check_block(
        &mut self,
        block: &ast::Block,
        block_type: Option<(OperationSnippet, Type)>,
    ) -> Type {
        self.scope.nest();
        let result = self.check_block_without_nest(block, block_type);
        self.scope.unnest();
        result
    }

    /// Checks a block but does not introduce a new level of nesting in the [`Scope`]. Useful for
    /// checking functions which add parameters in their body’s scope.
    fn check_block_without_nest(
        &mut self,
        block: &ast::Block,
        mut expected_block_type: Option<(OperationSnippet, Type)>,
    ) -> Type {
        // The type returned by the block. The last non-empty statement in the block is returned.
        let mut actual_block_type = Type::void(block.range);

        for i in 0..block.statements.len() {
            let statement = &block.statements[i];

            // Check the statement and assign its type as the last block type. For the last
            // statement we will check with the expected type provided to our function.
            actual_block_type = self.check_statement(
                statement,
                if i == block.statements.len() - 1 {
                    expected_block_type.take()
                } else {
                    None
                },
            );
        }

        // If we still have our `expected_block_type` then check it against our `actual_block_type`.
        // This will happen if we have zero block statements!
        if let Some((operation, expected_block_type)) = expected_block_type.take() {
            // We only expect this to happen when we have no block statements. Otherwise by checking
            // the type here we will lose the ability to do type inference on the function returned
            // by a block. In releases, this isn’t panic worthy so only panic in debug.
            debug_assert_eq!(block.statements.len(), 0);

            let _ = self.subtype(
                block.range,
                &operation,
                &actual_block_type,
                &expected_block_type,
            );
        }

        actual_block_type
    }

    fn check_statement(
        &mut self,
        statement: &ast::Statement,
        mut statement_type: Option<(OperationSnippet, Type)>,
    ) -> Type {
        let actual_statement_type = match &statement.kind {
            ast::StatementKind::Expression(expression) => {
                self.check_expression(expression, statement_type.take())
            }

            ast::StatementKind::Binding(binding) => {
                if let Some(annotation) = &binding.annotation {
                    let annotation = self.check_type(annotation);
                    self.check_expression(
                        &binding.value,
                        Some((
                            OperationSnippet::BindingStatementAnnotation(
                                binding.pattern.snippet(),
                                binding.value.snippet(),
                            ),
                            annotation.clone(),
                        )),
                    );
                    self.check_pattern(&binding.pattern, annotation);
                } else {
                    let type_ = self.check_expression(&binding.value, None);
                    self.check_pattern(&binding.pattern, type_);
                }
                Type::void(statement.range)
            }

            ast::StatementKind::Return(_) => unimplemented!(),
        };

        // If we have an expected type then let’s subtype it against our actual type.
        if let Some((operation, expected_statement_type)) = statement_type {
            let _ = self.subtype(
                statement.range,
                &operation,
                &actual_statement_type,
                &expected_statement_type,
            );
        }

        actual_statement_type
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

    /// When we expect an expression to be of a certain type, we call [`Self::check_expression`]
    /// with a type. This will check to make sure that the expression does indeed match the provided
    /// type and will report an error diagnostic if it doesn’t.
    ///
    /// This gives us the ability to do type inference for function expression parameters whose
    /// types are known by the surrounding context. This function also gives us better
    /// error messages.
    ///
    /// The `OperationSnippet` included with the expression type explains the reason why the
    /// expression has this type.
    fn check_expression(
        &mut self,
        expression: &ast::Expression,
        mut expression_type: Option<(OperationSnippet, Type)>,
    ) -> Type {
        let actual_expression_type = match &expression.kind {
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

            ast::ExpressionKind::Function(function) => {
                let function_type = self.check_function(function);
                Type {
                    range: expression.range,
                    kind: TypeKind::Function(function_type),
                }
            }

            ast::ExpressionKind::Call(_) => unimplemented!(),
            ast::ExpressionKind::Construct(_) => unimplemented!(),
            ast::ExpressionKind::Member(_) => unimplemented!(),
            ast::ExpressionKind::Prefix(_) => unimplemented!(),
            ast::ExpressionKind::Infix(_) => unimplemented!(),
            ast::ExpressionKind::Logical(_) => unimplemented!(),
            ast::ExpressionKind::Conditional(_) => unimplemented!(),

            // Checking a block is simple.
            ast::ExpressionKind::Block(block) => self.check_block(block, expression_type.take()),

            // If the wrapped expression does not have an annotation then we may simply check the
            // wrapped expression. If the wrapped expression does have a type annotation then we
            // will check the type annotation and check our wrapped expression with that
            // type annotation.
            ast::ExpressionKind::Wrapped(wrapped) => {
                if let Some(annotation) = &wrapped.annotation {
                    let annotation = self.check_type(annotation);
                    self.check_expression(
                        &wrapped.expression,
                        Some((
                            OperationSnippet::ExpressionAnnotation(wrapped.expression.snippet()),
                            annotation.clone(),
                        )),
                    );
                    annotation
                } else {
                    self.check_expression(&wrapped.expression, expression_type.take())
                }
            }
        };

        // If we have an expected type then let’s subtype it against our actual type.
        if let Some((operation, expected_expression_type)) = expression_type {
            let _ = self.subtype(
                expression.range,
                &operation,
                &actual_expression_type,
                &expected_expression_type,
            );
        }

        actual_expression_type
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

            ast::TypeKind::Function(function) => {
                let parameters = function
                    .parameters
                    .iter()
                    .map(|type_| self.check_type(type_))
                    .collect();
                let return_ = self.check_type(&function.return_);
                Type::function(type_.range, parameters, return_)
            }
        }
    }

    /// Determines if one type is the subtype of another. If it is determined that the types are
    /// not subtypes then a diagnostic error is reported. Even if an incompatibility is found, we
    /// will continue to try and find as many errors as possible. Returns `Ok` if the two types
    /// uphold the subtyping relationship and returns `Err` with the first diagnostic we reported if
    /// the two types do not uphold the subtyping relationship. In academic literature this
    /// operation is written as `type1 <: type2`.
    fn subtype(
        &mut self,
        range: Range,
        operation: &OperationSnippet,
        type1: &Type,
        type2: &Type,
    ) -> Result<(), DiagnosticRef> {
        use self::TypeKind::*;
        match (&type1.kind, &type2.kind) {
            // The error type is both the subtype and the supertype of everything. Which totally
            // breaks our type lattice. It is completely unsound and should never appear in a valid
            // program. It is useful for “forgetting” type information when an error occurs.
            (Error(_), _) => Ok(()),
            (_, Error(_)) => Ok(()),

            // The never type is our bottom type and so the subtype of everything but the supertype
            // of nothing.
            (Never, _) => Ok(()),

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

            // Functions will subtype with other functions.
            (Function(function1), Function(function2)) => {
                let mut result = Ok(());

                // If the two functions have different numbers of parameters then error.
                //
                // NOTE: We could allow `function1` to have less parameters then
                // `function2`. This is something all JavaScript type systems do. Say you
                // have a function of type `fun(Int, Int) -> Int` and you call it as `f(1, 2, 3)`.
                // The third parameter will be ignored at runtime and this type checks as a
                // valid program. Whether or not we want this runtime behavior should be decided
                // at a later time. While completely possible in JavaScript, we’re not sure if this
                // is possible in LLVM or JVM environments.
                if function1.parameters.len() != function2.parameters.len() {
                    result = result.and(Err(self.report_diagnostic(
                        Diagnostic::incompatible_function_parameter_lengths(
                            range,
                            operation.clone(),
                            (type1.range, function1.parameters.len()),
                            (type2.range, function2.parameters.len()),
                        ),
                    )));
                }

                // We want to check all the parameters that are shared in both function types.
                let parameter_len =
                    cmp::min(function1.parameters.len(), function2.parameters.len());

                // Subtype all the function parameters we can with one another. Remember to reverse
                // the subtyping direction! Function parameters are contravariant.
                for i in 0..parameter_len {
                    result = result.and(self.subtype(
                        range,
                        operation,
                        &function2.parameters[i],
                        &function1.parameters[i],
                    ));
                }

                // Finally, make sure to subtype the function return types.
                result = result.and(self.subtype(
                    range,
                    operation,
                    &function1.return_,
                    &function2.return_,
                ));

                result
            }

            // Error cases. We don’t use a hole (`_`) because we want the compiler to warn us
            // whenever we are missing a subtyping case.
            (_, Never)
            | (Void, _)
            | (Boolean, _)
            | (Number, _)
            | (Integer, _)
            | (Float, _)
            | (Function(_), _) => Err(self.report_diagnostic(Diagnostic::incompatible_types(
                range,
                operation.clone(),
                (type1.range, type1.snippet()),
                (type2.range, type2.snippet()),
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
