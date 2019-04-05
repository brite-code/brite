use super::js;
use crate::checker::avt::*;
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
            Declaration::Function(function) => {
                let id = self.scope_declare(&function.name);
                let (params, body) = self.compile_function(&function.function);
                js::Statement::function_declaration(id, params, body)
            }

            Declaration::Unimplemented => unimplemented!(),
        }
    }

    fn compile_function(&mut self, function: &Function) -> (Vec<js::Pattern>, js::BlockStatement) {
        self.scope_nest(|compiler| compiler.compile_function_without_nest(function))
    }

    fn compile_function_without_nest(
        &mut self,
        function: &Function,
    ) -> (Vec<js::Pattern>, js::BlockStatement) {
        let params = function
            .parameters
            .iter()
            .map(|pattern| self.compile_pattern(pattern))
            .collect();
        let body = function
            .body
            .statements
            .iter()
            .map(|statement| self.compile_statement(statement))
            .collect();
        (params, js::BlockStatement::new(body))
    }

    fn compile_statement(&mut self, statement: &Statement) -> js::Statement {
        match &statement.kind {
            StatementKind::Expression(expression) => {
                js::Statement::expression(self.compile_expression(expression))
            }
            StatementKind::Binding(binding) => js::Statement::variable_declaration(
                js::VariableDeclarationKind::Const,
                self.compile_pattern(&binding.pattern),
                self.compile_expression(&binding.value),
            ),
        }
    }

    fn compile_expression(&mut self, expression: &Expression) -> js::Expression {
        match &expression.kind {
            ExpressionKind::Constant(Constant::Boolean(value)) => {
                js::Expression::boolean_literal(*value)
            }

            ExpressionKind::Constant(Constant::Float(value)) => {
                js::Expression::numeric_literal(*value)
            }

            ExpressionKind::Constant(Constant::Integer(_, _)) => unimplemented!(),

            ExpressionKind::Reference(identifier) => {
                js::Expression::identifier(match self.scope_resolve(identifier) {
                    Some(js_identifier) => js_identifier.clone(),
                    None => unimplemented!(),
                })
            }

            ExpressionKind::Function(_) => unimplemented!(),
            ExpressionKind::Call(_) => unimplemented!(),
            ExpressionKind::Prefix(_) => unimplemented!(),

            ExpressionKind::Logical(logical) => js::Expression::logical(
                match &logical.operator {
                    LogicalOperator::And => js::LogicalOperator::And,
                    LogicalOperator::Or => js::LogicalOperator::Or,
                },
                self.compile_expression(&logical.left),
                self.compile_expression(&logical.right),
            ),

            ExpressionKind::Block(_) => unimplemented!(),
            ExpressionKind::Error(_) => unimplemented!(),

            ExpressionKind::Unimplemented => unimplemented!(),
        }
    }

    fn compile_pattern(&mut self, pattern: &Pattern) -> js::Pattern {
        match &pattern.kind {
            PatternKind::Binding(identifier) => {
                js::Pattern::identifier(self.scope_declare(identifier))
            }
        }
    }

    /// Introduces a new level of nesting for Brite bindings.
    fn scope_nest<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.bindings.manual_nest();
        self.bindings_js.manual_nest();
        let x = f(self);
        self.bindings_js.manual_unnest();
        self.bindings.manual_unnest();
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
