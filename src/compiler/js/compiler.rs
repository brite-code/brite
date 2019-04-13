use super::js;
use crate::language::*;
use crate::parser::Identifier;
use crate::utils::binding::BindingMap;

/// Manages the compilation of a Brite program into JavaScript code.
pub struct Compiler {
    /// Keeps track of all the Brite bindings currently in scope. We can lookup the JavaScript
    /// identifier for a Brite identifier by looking in this map.
    bindings: BindingMap<Identifier, Binding>,

    /// Keeps track of all the JavaScript variables currently in scope so that we don’t create a new
    /// variable name that conflicts with an existing one.
    bindings_js: BindingMap<js::Identifier, ()>,
}

impl Compiler {
    /// Creates a new
    pub fn new() -> Self {
        Compiler {
            bindings: BindingMap::new(),
            bindings_js: BindingMap::new(),
        }
    }

    /// Compiles a Brite module into a JavaScript module. Code compiled into JavaScript should have
    /// the same behavior as code compiled into another language, like LLVM.
    pub fn compile_module(mut self, module: &Module) -> js::Program {
        js::Program::new(
            module
                .declarations
                .iter()
                .map(|declaration| self.compile_declaration(declaration))
                .collect(),
        )
    }

    fn compile_declaration(&mut self, declaration: &Declaration) -> js::Statement {
        match declaration {
            // Compile a function declaration. Always use a JavaScript function declaration instead
            // of an arrow function expression. Some JavaScript developers like to only use arrow
            // functions for their top-level functions, but function declarations better match the
            // aesthetics of the Brite language.
            Declaration::Function(function) => {
                let id = self.scope_declare(&function.name.identifier);
                let (params, body) = self.compile_function(&function.function);
                let body = match body {
                    js::ArrowFunctionBody::Block(block) => block,
                    js::ArrowFunctionBody::Expression(expression) => {
                        js::BlockStatement::new(vec![js::Statement::return_(*expression)])
                    }
                };
                js::Statement::function_declaration(id, params, body)
            }

            Declaration::Class(_) => unimplemented!(),
        }
    }

    fn compile_function(
        &mut self,
        function: &Function,
    ) -> (Vec<js::Pattern>, js::ArrowFunctionBody) {
        self.scope_nest_js(|compiler| {
            compiler.scope_nest(|compiler| compiler.compile_function_without_nest(function))
        })
    }

    /// Compiles a function without introducing any levels of nesting. Call
    /// [`Compiler::compile_function`] if you want to introduce scope nesting.
    fn compile_function_without_nest(
        &mut self,
        function: &Function,
    ) -> (Vec<js::Pattern>, js::ArrowFunctionBody) {
        // Compile our function’s parameters.
        let params = function
            .parameters
            .iter()
            .map(|parameter| self.compile_pattern(&parameter.pattern))
            .collect();

        // Compile our block with a fresh array of JavaScript statements.
        let mut js_statements = Vec::with_capacity(function.body.statements.len());
        let return_expression = self.compile_block_without_nest(&mut js_statements, &function.body);

        // If compiling the block did not add any statements, then let’s use a simple arrow
        // expression body instead of an arrow expression block body.
        let body = if js_statements.is_empty() && !return_expression.is_undefined_literal() {
            js::ArrowFunctionBody::Expression(Box::new(return_expression))
        } else {
            // If the return expression is `undefined` then don’t bother adding a return statement.
            // JavaScript will implicitly return `undefined`.
            //
            // This is a small aesthetic improvement.
            if !return_expression.is_undefined_literal() {
                js_statements.push(js::Statement::return_(return_expression));
            }
            js::ArrowFunctionBody::Block(js::BlockStatement::new(js_statements))
        };

        (params, body)
    }

    /// Compiles the block without introducing a level of nesting. Pushes any statements into the
    /// statement array and returns the final value of the block as a [`js::Expression`].
    fn compile_block_without_nest(
        &mut self,
        js_statements: &mut Vec<js::Statement>,
        block: &Block,
    ) -> js::Expression {
        for i in 0..block.statements.len() {
            let statement = &block.statements[i];

            // If this is the last statement and the last statement is an expression statement then
            // return the compiled expression instead of adding a statement to `js_statements`.
            if i == block.statements.len() - 1 {
                if let StatementKind::Expression(expression) = &statement.kind {
                    return self.compile_expression(js_statements, expression);
                } else {
                    self.compile_statement(js_statements, statement);
                    return js::Expression::undefined_literal();
                }
            } else {
                self.compile_statement(js_statements, statement);
            }
        }
        debug_assert!(
            block.statements.is_empty(),
            "If there are block statements we should return from the above for-loop."
        );
        js::Expression::undefined_literal()
    }

    fn compile_statement(&mut self, js_statements: &mut Vec<js::Statement>, statement: &Statement) {
        match &statement.kind {
            StatementKind::Expression(expression) => {
                let js_expression = self.compile_expression(js_statements, expression);

                // If we compile the expression to only an undefined literal then don’t bother
                // adding it as an expression statement. The literal will have no
                // side-effects anyway.
                //
                // Presumably if the expression did need to discharge side-effects then it would
                // add statements to `js_statements`.
                //
                // We could skip adding expression statements for all lazy expressions, but this
                // optimization would be very shallow and wouldn’t apply to unused variable
                // declarations, for example. This is merely an aesthetic improvement.
                if !js_expression.is_undefined_literal() {
                    js_statements.push(js::Statement::expression(js_expression));
                }
            }

            StatementKind::Binding(binding) => {
                let js_statement = js::Statement::variable_declaration(
                    js::VariableDeclarationKind::Const,
                    self.compile_pattern(&binding.pattern),
                    self.compile_expression(js_statements, &binding.value),
                );
                js_statements.push(js_statement);
            }

            StatementKind::Return(_) => unimplemented!(),
        }
    }

    /// Compiles an expression, possibly adding some JavaScript statements as we compile. For
    /// example, if we have a block expression then the block will want to add its statements to
    /// our JavaScript scope.
    fn compile_expression(
        &mut self,
        js_statements: &mut Vec<js::Statement>,
        expression: &Expression,
    ) -> js::Expression {
        match &expression.kind {
            // A Brite boolean is a JavaScript boolean...
            ExpressionKind::Constant(Constant::Boolean(value)) => {
                js::Expression::boolean_literal(*value)
            }

            // A Brite float is 64 bits which is the same as a JavaScript float which is also
            // 64 bits...
            ExpressionKind::Constant(Constant::Float(value)) => {
                js::Expression::numeric_literal(*value)
            }

            ExpressionKind::Constant(Constant::Integer(_, _)) => unimplemented!(),

            // Resolve the JavaScript identifier we are using to represent the referenced Brite
            // variable. If we can’t resolve a variable then we have an internal error! Unresolved
            // variables should be handled by the checker!
            ExpressionKind::Reference(identifier) => {
                js::Expression::identifier(match self.scope_resolve(identifier) {
                    Some(js_identifier) => js_identifier.clone(),
                    None => unimplemented!(),
                })
            }

            ExpressionKind::This => unimplemented!(),

            // Compile a Brite function expression to a JavaScript arrow function.
            ExpressionKind::Function(function) => {
                let (params, body) = self.compile_function(function);
                js::Expression::arrow_function(params, body)
            }

            ExpressionKind::Call(_) => unimplemented!(),
            ExpressionKind::Construct(_) => unimplemented!(),
            ExpressionKind::Member(_) => unimplemented!(),
            ExpressionKind::Prefix(_) => unimplemented!(),
            ExpressionKind::Infix(_) => unimplemented!(),

            // Compile both operands of a logical expression and create a JavaScript
            // logical expression.
            //
            // NOTE: Remember that logical expressions are short circuiting! That means, say, in
            // `E1 && E2` if `E1` is `false` then `E2` will not be evaluated at all.
            //
            // TODO: `false && do { foo(); true }`
            ExpressionKind::Logical(logical) => js::Expression::logical(
                match &logical.operator {
                    LogicalOperator::And => js::LogicalOperator::And,
                    LogicalOperator::Or => js::LogicalOperator::Or,
                },
                self.compile_expression(js_statements, &logical.left),
                self.compile_expression(js_statements, &logical.right),
            ),

            ExpressionKind::Conditional(_) => unimplemented!(),

            // Add a level of Brite nesting and compile our block...
            ExpressionKind::Block(block) => self
                .scope_nest(|compiler| compiler.compile_block_without_nest(js_statements, block)),

            ExpressionKind::Wrapped(wrapped) => {
                self.compile_expression(js_statements, &wrapped.expression)
            }
        }
    }

    fn compile_pattern(&mut self, pattern: &Pattern) -> js::Pattern {
        match &pattern.kind {
            PatternKind::Binding(identifier) => {
                js::Pattern::identifier(self.scope_declare(identifier))
            }
            PatternKind::Hole => unimplemented!(),
            PatternKind::This => unimplemented!(),
        }
    }

    /// Introduces a new level of nesting for Brite bindings.
    fn scope_nest<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.bindings.manual_nest();
        let x = f(self);
        self.bindings.manual_unnest();
        x
    }

    /// Introduces a new level of nesting for JavaScript bindings.
    fn scope_nest_js<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.bindings_js.manual_nest();
        let x = f(self);
        self.bindings_js.manual_unnest();
        x
    }

    /// Declares a new Brite variable in our current scope and returns the JavaScript identifier we
    /// will use to reference that variable.
    fn scope_declare(&mut self, identifier: &Identifier) -> js::Identifier {
        // Get the number of bindings shallowly declared in this scope with the same name.
        let mut dedupe = match self.bindings.get_shallow(identifier) {
            None => 1,
            Some(binding) => binding.dedupe + 1,
        };

        // Generate the first JavaScript identifier we will attempt to use for this declaration. If
        // this is our first binding then don’t add a dedupe number to the end.
        let js_identifier = if dedupe == 1 {
            // If the JavaScript identifier is a reserved word then add an underscore to the end
            // so we don’t break JavaScript syntax.
            if js::RESERVED_WORDS.contains(identifier.as_str()) {
                let mut js_identifier = String::with_capacity(identifier.as_str().len() + 1);
                js_identifier.push_str(identifier.as_str());
                js_identifier.push('_');
                js_identifier
            } else {
                identifier.as_str().to_string()
            }
        } else {
            format!("{}{}", identifier.as_str(), dedupe)
        };

        // It is ok to create a new JavaScript identifier without checking it first because Brite
        // identifier syntax is a subset of valid JavaScript identifier syntax. Not considering
        // JavaScript reserved words which we check for above.
        let mut js_identifier = js::Identifier::new_unchecked(js_identifier);

        // If a JavaScript identifier with this name already exists then try adding numbers
        // to the end until we find an identifier does not yet exist.
        while self.bindings_js.contains_shallow(&js_identifier) {
            dedupe += 1;
            js_identifier =
                js::Identifier::new_unchecked(format!("{}{}", identifier.as_str(), dedupe));
        }

        // Insert our binding into the bindings map. If another Brite binding uses the same
        // name then we want to start the dedupe counter at a later number.
        self.bindings.insert(
            identifier.clone(),
            Binding {
                dedupe,
                identifier: js_identifier.clone(),
            },
        );

        // Insert the JS identifier we created into our JavaScript binding map so that we don‘t
        // reuse this identifier again.
        self.bindings_js.insert(js_identifier.clone(), ());

        js_identifier
    }

    /// Resolve the JavaScript identifier we use to reference a Brite identifier in the
    /// current scope.
    fn scope_resolve(&self, identifier: &Identifier) -> Option<&js::Identifier> {
        self.bindings
            .get(identifier)
            .map(|binding| &binding.identifier)
    }
}

struct Binding {
    dedupe: usize,
    identifier: js::Identifier,
}
