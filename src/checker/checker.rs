use super::avt::*;
use crate::diagnostics::{
    Diagnostic, DiagnosticRef, DiagnosticsCollection, OperationSnippet, OperatorSnippet,
    TypeKindSnippet,
};
use crate::parser::ast;
use crate::parser::{Identifier, Range};
use crate::utils::vecn::Vec1;
use std::cmp;
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
    pub fn check_module(mut self, module: &ast::Module) -> Module {
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

        let mut declarations = Vec::with_capacity(module.declarations.len());

        // Now that all of our declarations are in scope, loop through our declaration list again
        // and type check all our declarations.
        for declaration in &module.declarations {
            let declaration = self.check_declaration(declaration);
            declarations.push(declaration);
        }

        Module::new(declarations)
    }

    fn check_declaration(&mut self, declaration: &ast::Declaration) -> Declaration {
        match declaration {
            ast::Declaration::Function(function) => {
                let function = self.check_function_declaration(function);
                Declaration::Function(function)
            }
            ast::Declaration::Class(class) => {
                self.check_class_declaration(class);
                Declaration::Unimplemented
            }
        }
    }

    fn check_function_declaration(
        &mut self,
        function: &ast::FunctionDeclaration,
    ) -> FunctionDeclaration {
        let name = function.name.identifier.clone();
        let function = self.check_function(function.name.range, &function.function, None);
        FunctionDeclaration::new(name, function.node)
    }

    /// Checks a function and returns the type of the function.
    ///
    /// The provided range is used for error reporting when we don’t have any better range.
    ///
    /// If we have an expected function type then we’ll check our function AST against that type.
    /// The expected function type can be used to infer the types of function parameters where are
    /// not annotated.
    fn check_function(
        &mut self,
        range: Range,
        function: &ast::Function,
        expected: Option<WithFunctionType>,
    ) -> CheckedFunction {
        // When checking a function, we want to add parameters to the block. So introduce a level
        // of nesting in the scope.
        self.scope.nest();

        // Create our parameter vectors which we will push to as we type-check parameters.
        let mut parameters = Vec::with_capacity(function.parameters.len());
        let mut parameter_types = Vec::with_capacity(function.parameters.len());

        // If we have an expected function type, check to make sure that it has the same number of
        // parameters as our actual function expression.
        if let Some(expected) = &expected {
            if function.parameters.len() != expected.function_type.parameters.len() {
                self.report_diagnostic(Diagnostic::incompatible_function_parameter_lengths(
                    range,
                    expected.operation.clone(),
                    (range, function.parameters.len()),
                    (expected.range, expected.function_type.parameters.len()),
                ));
            }
        }

        // Add function parameters to our current, nested, scope.
        for i in 0..function.parameters.len() {
            let parameter = &function.parameters[i];

            // If we have an expected function type then get our expected function parameter type!
            // If we have more function expression parameters then function type parameters we will
            // return none.
            let expected_parameter_type = if let Some(expected) = &expected {
                if i < expected.function_type.parameters.len() {
                    Some((&expected.operation, &expected.function_type.parameters[i]))
                } else {
                    None
                }
            } else {
                None
            };

            // Get the type for all of our function parameters. If a function parameter is missing
            // an annotation then we will report an error and will create an unsound error type.
            let type_ = match (&parameter.annotation, expected_parameter_type) {
                // If our function parameter has an annotation and no expected type then the type
                // of our parameter is the type of our annotation.
                (Some(actual_type), None) => self.check_type(actual_type),

                // If our function parameter has no annotation, but it does have an expected type
                // then use the expected type as the type of our function parameter.
                //
                // NOTE: This is where we actually “infer” the type of function parameters when we
                // have enough context to do so.
                (None, Some((_, expected_type))) => expected_type.clone(),

                // If we have both a parameter type annotation _and_ an expected type then we need
                // to subtype them. Remember that function parameters are contravariant so we
                // subtype in the opposite direction.
                (Some(actual_type), Some((operation, expected_type))) => {
                    let range = actual_type.range;
                    let actual_type = self.check_type(actual_type);
                    let _ = self.subtype(range, operation, expected_type, &actual_type);
                    actual_type
                }

                // If we have neither a function parameter annotation or an expected function
                // parameter type then complain to the programmer that we are missing a function
                // parameter type annotation.
                (None, None) => Type::error(self.report_diagnostic(
                    Diagnostic::missing_function_parameter_type(
                        parameter.pattern.range,
                        parameter.pattern.snippet(),
                    ),
                )),
            };

            // Check the pattern with this parameter’s type annotation.
            let pattern = self.check_pattern(&parameter.pattern, type_.clone());

            // Add this parameter to our list.
            parameters.push(pattern);
            parameter_types.push(type_);
        }

        // Get the body of our function. If our return type was annotated then we need to
        // check that annotation against the function body. If our return type was not annotated
        // then use the inferred type of the function’s body.
        let body: Checked<Block> = match &function.return_type {
            // Check our annotated return type against the body of our function.
            Some(return_type) => {
                let return_type = self.check_type(return_type);
                let operation = OperationSnippet::FunctionReturnAnnotation(
                    function.body.statements.last().map(ast::Statement::snippet),
                );
                let body = self.check_block_without_nest(
                    &function.body,
                    Some(WithType::new(operation, &return_type)),
                );
                Checked::new(return_type, body.node)
            }

            // Infer a type based on our function body and return that.
            None => self.check_block_without_nest(&function.body, None),
        };

        // If we have an expected function type then make sure we verify that the return type
        // is correct!
        if let Some(expected) = &expected {
            // Report any subtyping errors against the return type range (if we have one) or the
            // range of the last statement in the function body.
            let range = match &function.return_type {
                Some(return_type) => return_type.range,
                None => function.body.return_range(),
            };
            let _ = self.subtype(
                range,
                &expected.operation,
                &body.type_,
                &expected.function_type.return_,
            );
        }

        // Leave the scope we created for this function.
        self.scope.unnest();

        // Return a function and its type.
        CheckedFunction::new(
            FunctionType::new(parameter_types, body.type_),
            Function::new(parameters, body.node),
        )
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

    fn check_block(&mut self, block: &ast::Block, expected: Option<WithType>) -> Checked<Block> {
        self.scope.nest();
        let result = self.check_block_without_nest(block, expected);
        self.scope.unnest();
        result
    }

    /// Checks a block but does not introduce a new level of nesting in the [`Scope`]. Useful for
    /// checking functions which add parameters in their body’s scope.
    fn check_block_without_nest(
        &mut self,
        block: &ast::Block,
        mut expected: Option<WithType>,
    ) -> Checked<Block> {
        // The statements for our block AVT node.
        let mut statements = Vec::with_capacity(block.statements.len());

        // The type returned by the block. The last non-empty statement in the block is returned.
        let mut block_type = None;

        for i in 0..block.statements.len() {
            let statement = &block.statements[i];

            // Check the statement. If this is the last statement then check using our block’s
            // expected type. Set the type returned by our block to the last statement’s type.
            let statement = if i == block.statements.len() - 1 {
                let statement = self.check_statement(statement, expected.take());
                block_type = Some(statement.type_);
                statement.node
            } else {
                let statement = self.check_statement(statement, None);
                statement.node
            };

            // Add the statement to our AVT statement list.
            statements.push(statement);
        }

        // If there were no statements then create a new void type.
        let block_type = match block_type {
            Some(block_type) => block_type,
            None => Type::void(block.range),
        };

        // If we still have our `expected` type then check it against our `actual_type`.
        // This will happen if we have zero block statements!
        if let Some(expected) = expected {
            // We only expect this to happen when we have no block statements. Otherwise by checking
            // the type here we will lose the ability to do type inference on the function returned
            // by a block. In releases, this isn’t panic worthy so only panic in debug.
            debug_assert_eq!(block.statements.len(), 0);

            let _ = self.subtype(
                block.range,
                &expected.operation,
                &block_type,
                expected.type_,
            );
        }

        Checked::new(block_type, Block::new(block.range, statements))
    }

    fn check_statement(
        &mut self,
        statement: &ast::Statement,
        mut expected: Option<WithType>,
    ) -> Checked<Statement> {
        let range = statement.range;

        let statement: Checked<Statement> = match &statement.kind {
            ast::StatementKind::Expression(expression) => {
                let expression =
                    self.check_expression_with_optional_type(expression, expected.take());
                Checked::new(
                    expression.type_,
                    Statement::expression(range, expression.node),
                )
            }

            ast::StatementKind::Binding(binding) => {
                if let Some(annotation) = &binding.annotation {
                    let annotation = self.check_type(annotation);
                    let value = self.check_expression_with_type(
                        OperationSnippet::BindingStatementAnnotation(
                            binding.pattern.snippet(),
                            binding.value.snippet(),
                        ),
                        &binding.value,
                        &annotation,
                    );
                    let pattern = self.check_pattern(&binding.pattern, annotation);
                    Checked::new(
                        Type::void(range),
                        Statement::binding(range, pattern, value.node),
                    )
                } else {
                    let value = self.check_expression(&binding.value);
                    let pattern = self.check_pattern(&binding.pattern, value.type_);
                    Checked::new(
                        Type::void(range),
                        Statement::binding(range, pattern, value.node),
                    )
                }
            }

            ast::StatementKind::Return(_) => unimplemented!(),
        };

        // If we have an expected type then let’s subtype it against our actual type.
        if let Some(expected) = expected {
            let _ = self.subtype(
                range,
                &expected.operation,
                &statement.type_,
                &expected.type_,
            );
        }

        statement
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

    fn check_expression(&mut self, expression: &ast::Expression) -> Checked<Expression> {
        self.check_expression_with_optional_type(expression, None)
    }

    fn check_expression_with_type(
        &mut self,
        operation: OperationSnippet,
        expression: &ast::Expression,
        type_: &Type,
    ) -> Checked<Expression> {
        self.check_expression_with_optional_type(expression, Some(WithType::new(operation, type_)))
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
    fn check_expression_with_optional_type(
        &mut self,
        expression: &ast::Expression,
        mut expected: Option<WithType>,
    ) -> Checked<Expression> {
        let range = expression.range;

        let expression: Checked<Expression> = match &expression.kind {
            // Check a constant. Provide our range since constants don’t have a range themselves.
            ast::ExpressionKind::Constant(constant) => Checked::new(
                self.check_constant(range, constant),
                Expression::constant(range, constant.clone()),
            ),

            ast::ExpressionKind::Reference(identifier) => {
                match self.scope.resolve(&range, identifier) {
                    // If the identifier was not found report our error and return the unsound
                    // error type.
                    Err(diagnostic) => {
                        let diagnostic = self.report_diagnostic(diagnostic);
                        Checked::new(
                            Type::error(diagnostic.clone()),
                            Expression::error(range, diagnostic, None),
                        )
                    }

                    Ok(entry) => match &entry.kind {
                        ScopeEntryKind::Type(_) => unimplemented!(),
                        ScopeEntryKind::Function => unimplemented!(),
                        ScopeEntryKind::Class { .. } => unimplemented!(),

                        // If we are referencing a value then return that.
                        ScopeEntryKind::Value(type_) => Checked::new(
                            type_.clone(),
                            Expression::reference(range, identifier.clone()),
                        ),
                    },
                }
            }

            ast::ExpressionKind::This => unimplemented!(),

            ast::ExpressionKind::Function(function) => {
                // Attempt to narrow our expected type to a function type.
                let expected = match expected.take() {
                    None => None,
                    Some(expected) => match expected.type_ {
                        // An error type is the supertype of everything.
                        Type::Error { .. } => None,

                        // Successfully narrow if this type is a function type.
                        Type::Ok {
                            range,
                            kind: TypeKind::Function(function_type),
                        } => Some(WithFunctionType::new(
                            expected.operation,
                            *range,
                            &**function_type,
                        )),

                        // For everything else, report an error.
                        Type::Ok { range, kind } => {
                            self.report_diagnostic(Diagnostic::incompatible_types(
                                expression.range,
                                expected.operation,
                                (expression.range, TypeKindSnippet::Function),
                                (*range, kind.snippet()),
                            ));
                            None
                        }
                    },
                };

                let function = self.check_function(expression.range, function, expected);
                Checked::new(
                    Type::Ok {
                        range: expression.range,
                        kind: TypeKind::Function(Rc::new(function.type_)),
                    },
                    Expression::unimplemented(range),
                )
            }

            // Call a function type with some arguments...
            ast::ExpressionKind::Call(call) => {
                // We can infer either the type of function we are calling or we can infer the
                // argument types. We choose to infer the argument types using the callee type which
                // is why we don’t provide a type here.
                let callee = self.check_expression(&call.callee);

                // Narrow the callee type down to only function types. Error for any
                // non-function types.
                let callee_type = match callee.type_ {
                    // An error type is the supertype of everything.
                    Type::Error { error } => Err(error),

                    // Function types may actually be called!
                    Type::Ok {
                        range,
                        kind: TypeKind::Function(function_type),
                    } => Ok((range, function_type)),

                    // For everything else, report an error.
                    Type::Ok {
                        range: callee_range,
                        kind: callee_type_kind,
                    } => Err(self.report_diagnostic(Diagnostic::cannot_call(
                        call.callee.range,
                        callee_range,
                        callee_type_kind.snippet(),
                    ))),
                };

                match callee_type {
                    // If we have a function type then make sure to check our stuffs!
                    Ok((callee_type_range, callee_type)) => {
                        // If we called the function with an incorrect number of arguments then
                        // report an error with the correct number of arguments.
                        if call.arguments.len() != callee_type.parameters.len() {
                            self.report_diagnostic(
                                Diagnostic::incompatible_function_parameter_lengths(
                                    call.callee.range,
                                    OperationSnippet::FunctionCall(call.callee.snippet()),
                                    (expression.range, call.arguments.len()),
                                    (callee_type_range, callee_type.parameters.len()),
                                ),
                            );
                        }

                        // Check all the arguments in our call expression for type errors...
                        for i in 0..call.arguments.len() {
                            let argument = &call.arguments[i];

                            // If our expected callee type has a parameter in the same position as
                            // this one then let’s check our expression with that argument type.
                            if i < callee_type.parameters.len() {
                                self.check_expression_with_type(
                                    OperationSnippet::FunctionCall(call.callee.snippet()),
                                    argument,
                                    &callee_type.parameters[i],
                                );
                            } else {
                                self.check_expression(argument);
                            }
                        }

                        // The type of our expression is the type returned by our callee’s
                        // function type!
                        Checked::new(
                            (&*callee_type.return_).clone(),
                            Expression::unimplemented(range),
                        )
                    }

                    // If we have an error type then still make sure to check all our arguments.
                    // Even if we don’t have any expected types for them.
                    Err(error) => {
                        for argument in &call.arguments {
                            self.check_expression(argument);
                        }
                        Checked::new(Type::error(error), Expression::unimplemented(range))
                    }
                }
            }

            ast::ExpressionKind::Construct(_) => unimplemented!(),
            ast::ExpressionKind::Member(_) => unimplemented!(),

            // Make sure the operand to a prefix expression is of the correct type.
            ast::ExpressionKind::Prefix(prefix) => match prefix.operator {
                ast::PrefixOperator::Not => {
                    self.check_expression_with_type(
                        OperationSnippet::OperatorExpression(OperatorSnippet::Not),
                        &prefix.operand,
                        &Type::boolean(prefix.operand.range),
                    );
                    Checked::new(
                        Type::boolean(expression.range),
                        Expression::unimplemented(range),
                    )
                }

                ast::PrefixOperator::Negative => unimplemented!(),
                ast::PrefixOperator::Positive => unimplemented!(),
            },

            ast::ExpressionKind::Infix(_) => unimplemented!(),

            // Make sure both operands to a logical expression are of the correct type.
            ast::ExpressionKind::Logical(logical) => {
                let operation = OperationSnippet::OperatorExpression(match &logical.operator {
                    ast::LogicalOperator::And => OperatorSnippet::And,
                    ast::LogicalOperator::Or => OperatorSnippet::Or,
                });
                let left = self.check_expression_with_type(
                    operation.clone(),
                    &logical.left,
                    &Type::boolean(logical.left.range),
                );
                let right = self.check_expression_with_type(
                    operation.clone(),
                    &logical.right,
                    &Type::boolean(logical.right.range),
                );
                Checked::new(
                    Type::boolean(expression.range),
                    Expression::logical(range, logical.operator.clone(), left.node, right.node),
                )
            }

            ast::ExpressionKind::Conditional(_) => unimplemented!(),

            // Checking a block is simple.
            ast::ExpressionKind::Block(block) => Checked::new(
                self.check_block(block, expected.take()).type_,
                Expression::unimplemented(range),
            ),

            // If the wrapped expression does not have an annotation then we may simply check the
            // wrapped expression. If the wrapped expression does have a type annotation then we
            // will check the type annotation and check our wrapped expression with that
            // type annotation.
            ast::ExpressionKind::Wrapped(wrapped) => {
                if let Some(annotation) = &wrapped.annotation {
                    let annotation = self.check_type(annotation);
                    let expression = self.check_expression_with_type(
                        OperationSnippet::ExpressionAnnotation(wrapped.expression.snippet()),
                        &wrapped.expression,
                        &annotation,
                    );
                    Checked::new(annotation, expression.node)
                } else {
                    self.check_expression_with_optional_type(&wrapped.expression, expected.take())
                }
            }
        };

        // If we have an expected type then let’s subtype it against our actual type.
        if let Some(expected) = expected {
            let _ = self.subtype(
                range,
                &expected.operation,
                &expression.type_,
                &expected.type_,
            );
        }

        expression
    }

    /// Checks a pattern which is supposed to bind a value with the provided type. If the pattern
    /// is of a different type, say we are trying to bind a number to an object pattern, then we
    /// will report a diagnostic.
    fn check_pattern(&mut self, pattern: &ast::Pattern, type_: Type) -> Pattern {
        let range = pattern.range;

        match &pattern.kind {
            // Declare a value variable in this scope with the pattern’s binding identifier.
            ast::PatternKind::Binding(identifier) => {
                self.scope.declare(
                    identifier.clone(),
                    ScopeEntry {
                        range: pattern.range,
                        kind: ScopeEntryKind::Value(type_),
                    },
                );
                Pattern::binding(range, identifier.clone())
            }

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
                    Err(diagnostic) => Type::error(self.report_diagnostic(diagnostic)),

                    Ok(entry) => match &entry.kind {
                        ScopeEntryKind::Value(_) => unimplemented!(),
                        ScopeEntryKind::Function => unimplemented!(),
                        ScopeEntryKind::Class { .. } => unimplemented!(),

                        // If we are referencing a type then return that.
                        ScopeEntryKind::Type(referenced_type) => {
                            let mut referenced_type = referenced_type.clone();

                            // TODO: Find a better way to do this then mutating the type’s range
                            // which is very hacky!
                            if let Type::Ok {
                                ref mut range,
                                kind: _,
                            } = referenced_type
                            {
                                *range = type_.range;
                            }

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

        let ((range1, kind1), (range2, kind2)) = match (type1, type2) {
            // The error type is both the subtype and the supertype of everything. Which totally
            // breaks our type lattice. It is completely unsound and should never appear in a valid
            // program. It is useful for “forgetting” type information when an error occurs.
            (Type::Error { .. }, _) => return Ok(()),
            (_, Type::Error { .. }) => return Ok(()),

            // Unwrap two “ok” types and actually perform subtyping on them below.
            (
                Type::Ok {
                    range: range1,
                    kind: kind1,
                },
                Type::Ok {
                    range: range2,
                    kind: kind2,
                },
            ) => ((range1, kind1), (range2, kind2)),
        };

        match (kind1, kind2) {
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
            //
            // **IMPORTANT:** If you update the subtyping logic of functions down here, also make
            // sure to update the subtyping logic of functions in `check_function()`!
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
                            (*range1, function1.parameters.len()),
                            (*range2, function2.parameters.len()),
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
                (*range1, kind1.snippet()),
                (*range2, kind2.snippet()),
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

/// Struct for representing a type associated with an operation which required checking that type
/// against the actual program’s type.
struct WithType<'a> {
    operation: OperationSnippet,
    type_: &'a Type,
}

impl<'a> WithType<'a> {
    fn new(operation: OperationSnippet, type_: &'a Type) -> Self {
        WithType { operation, type_ }
    }
}

/// A specialization of `WithType` for only `FunctionType`s.
struct WithFunctionType<'a> {
    operation: OperationSnippet,
    range: Range,
    function_type: &'a FunctionType,
}

impl<'a> WithFunctionType<'a> {
    fn new(operation: OperationSnippet, range: Range, function_type: &'a FunctionType) -> Self {
        WithFunctionType {
            operation,
            range,
            function_type,
        }
    }
}

/// A typed AVT node. All AVT nodes carry around enough type information for compilation and IDE
/// tooling. During type checking, though, we want to remember the type of every block.
struct Checked<Node> {
    type_: Type,
    node: Node,
}

impl<Node> Checked<Node> {
    fn new(type_: Type, node: Node) -> Self {
        Checked { type_, node }
    }
}

/// A typed AVT node. Specifically for functions. All AVT nodes carry around enough type information
/// for compilation and IDE tooling. During type checking, though, we want to remember the type of
/// every block.
struct CheckedFunction {
    type_: FunctionType,
    node: Function,
}

impl CheckedFunction {
    fn new(type_: FunctionType, node: Function) -> Self {
        CheckedFunction { type_, node }
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
        insert_root_entry(&mut root, "Never", range, Type::never(range));
        insert_root_entry(&mut root, "Void", range, Type::void(range));
        insert_root_entry(&mut root, "Bool", range, Type::boolean(range));
        insert_root_entry(&mut root, "Num", range, Type::number(range));
        insert_root_entry(&mut root, "Int", range, Type::integer(range));
        insert_root_entry(&mut root, "Float", range, Type::float(range));

        fn insert_root_entry(
            root: &mut HashMap<Identifier, ScopeEntry>,
            name: &'static str,
            range: Range,
            type_: Type,
        ) {
            root.insert(
                Identifier::new(name).unwrap(),
                ScopeEntry {
                    range,
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
