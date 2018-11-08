type strategy =
  | Eager
  | Lazy

type serialized_expression = {
  strategy: strategy;
  expression: JsAst.expression;
}

let serialize_variable scope name =
  JsAst.identifier_expression (JsScope.find scope name)

let serialize_constant constant =
  match constant with
  | Expression.Boolean value -> JsAst.boolean_literal value
  | Expression.Number value -> JsAst.numeric_literal value

let rec serialize_to_js_expression scope expression =
  match expression.Expression.description with
  | Variable { name } -> (Del.Empty, { strategy = Lazy; expression = serialize_variable scope name })
  | Constant constant -> (Del.Empty, { strategy = Lazy; expression = serialize_constant constant })
  | Function { parameter; body } -> (Del.Empty, { strategy = Lazy; expression = serialize_function scope parameter body })

  | Call { callee; argument } ->
    let (statements, expression) = serialize_call scope callee argument in
    (statements, { strategy = Eager; expression })

  | Binding { name; value; body } ->
    let (scope, value_statements) = serialize_binding scope name value in
    let (body_statements, body) = serialize_to_js_expression scope body in
    let statements = Del.concat value_statements body_statements in
    (statements, body)

  | Annotation { value; type_ = _ } -> serialize_to_js_expression scope value

  | Conditional { test; consequent; alternate } ->
    let (test_statements, test) = serialize_to_js_expression scope test in
    let (consequent_statements, consequent) = serialize_to_js_expression scope consequent in
    let (alternate_statements, alternate) = serialize_to_js_expression scope alternate in
    if consequent_statements = Empty && alternate_statements = Empty then (
      let expression = JsAst.conditional_expression test.expression consequent.expression alternate.expression in
      let strategy = if (
        test.strategy = Lazy &&
        consequent.strategy = Lazy &&
        alternate.strategy = Lazy
      ) then Lazy else Eager in
      (test_statements, { strategy; expression })
    ) else (
      let (scope, phi) = JsScope.new_name scope "$phi" in
      let phi_pattern = JsAst.identifier_pattern phi in
      let phi_declaration = JsAst.variable_declaration Let phi_pattern None in
      let phi_consequent_assignment = JsAst.expression_statement
        (JsAst.assignment_expression phi_pattern consequent.expression) in
      let phi_alternate_assignment = JsAst.expression_statement
        (JsAst.assignment_expression phi_pattern alternate.expression) in
      let consequent_statements = Del.ConsEnd (consequent_statements, phi_consequent_assignment) in
      let alternate_statements = Del.ConsEnd (consequent_statements, phi_alternate_assignment) in
      let if_statement = JsAst.if_statement test.expression
        (Del.to_list consequent_statements) (Del.to_list alternate_statements) in
      let statements = Del.ConsEnd (Del.ConsEnd (test_statements, phi_declaration), if_statement) in
      (statements, { strategy = Lazy; expression = JsAst.identifier_expression phi })
    )

and serialize_to_js_statements scope expression output =
  match expression.Expression.description with
  | Variable { name } -> Del.Cons (output (serialize_variable scope name), Empty)
  | Constant constant -> Del.Cons (output (serialize_constant constant), Empty)
  | Function { parameter; body } -> Del.Cons (output (serialize_function scope parameter body), Empty)

  | Call { callee; argument } ->
    let (statements, expression) = serialize_call scope callee argument in
    ConsEnd (statements, output expression)

  | Binding { name; value; body } ->
    let (scope, value_statements) = serialize_binding scope name value in
    let body_statements = serialize_to_js_statements scope body output in
    Del.concat value_statements body_statements

  | Annotation { value; type_ = _ } -> serialize_to_js_statements scope value output

  | Conditional { test; consequent; alternate } ->
    let (test_statements, test) = serialize_to_js_expression scope test in
    let (consequent_statements, consequent) = serialize_to_js_expression scope consequent in
    let (alternate_statements, alternate) = serialize_to_js_expression scope alternate in
    if consequent_statements = Empty && alternate_statements = Empty then (
      let expression = JsAst.conditional_expression test.expression consequent.expression alternate.expression in
      Del.Cons (output expression, Empty)
    ) else (
      let consequent_statements = Del.ConsEnd (consequent_statements, output consequent.expression) in
      let alternate_statements = Del.ConsEnd (alternate_statements, output alternate.expression) in
      let if_statement = JsAst.if_statement test.expression
        (Del.to_list consequent_statements) (Del.to_list alternate_statements) in
      Del.ConsEnd (test_statements, if_statement)
    )

and serialize_function scope parameter body =
  let (scope, parameter) = JsScope.add scope parameter in
  let params = [JsAst.identifier_pattern parameter] in
  let statements = Del.to_list (serialize_to_js_statements scope body JsAst.return_statement) in
  let body = JsAst.statements_to_arrow_function_body statements in
  JsAst.arrow_function_expression params body

and serialize_call scope callee argument =
  let (callee_statements, callee) = serialize_to_js_expression scope callee in
  let (argument_statements, argument) = serialize_to_js_expression scope argument in
  if callee.strategy = Eager && argument_statements <> Empty then (
    let (scope, name) = JsScope.new_name scope "$arg" in
    let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name) (Some callee.expression) in
    let statements = Del.concat callee_statements (Cons (statement, argument_statements)) in
    (statements, JsAst.call_expression (JsAst.identifier_expression name) [argument.expression])
  ) else (
    let statements = Del.concat callee_statements argument_statements in
    (statements, JsAst.call_expression callee.expression [argument.expression])
  )

and serialize_binding scope name value =
  let (scope', name') = JsScope.add scope name in
  let output e = JsAst.expression_statement (JsAst.assignment_expression (JsAst.identifier_pattern name') e) in
  let statements = serialize_to_js_statements scope value output in
  let (scope, name) = (scope', name') in
  let value = match statements with
  | Cons (statement, Empty) | ConsEnd (Empty, statement) -> JsAst.assignment_to statement name
  | _ -> None
  in
  match value with
  | Some value ->
    let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name) (Some value) in
    (scope, Cons (statement, Empty))
  | None ->
    let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name) None in
    (scope, Cons (statement, statements))

(* Serializes a Brite expression to a JavaScript program. *)
let serialize expression =
  let (statements, { expression; _ }) = serialize_to_js_expression JsScope.empty expression in
  let statements = Del.to_list (ConsEnd (statements, JsAst.expression_statement expression)) in
  JsAst.program statements
