type strategy =
  | Eager
  | Lazy

type serialized_expression = {
  strategy: strategy;
  expression: JsAst.expression;
}

type serialized_result =
  | Expression of serialized_expression
  | Statement of JsAst.statement

let rec serialize context expression output =
  match expression.Expression.description with
  | Variable { name } ->
    let expression = JsAst.identifier_expression (StringMap.find name context) in
    (Del.Empty, Expression { strategy = Lazy; expression })

  | Boolean value ->
    let expression = JsAst.boolean_literal value in
    (Del.Empty, Expression { strategy = Lazy; expression })

  | Number value ->
    let expression = JsAst.numeric_literal value in
    (Del.Empty, Expression { strategy = Lazy; expression })

  | Function { parameter; body } ->
    JsNamer.block (fun () -> (
      let parameter' = JsNamer.unique parameter in
      let context = StringMap.add parameter parameter' context in
      let params = [JsAst.identifier_pattern parameter'] in
      let body = match serialize context body JsAst.return_statement with
      | (Empty, Expression { expression; _ }) ->
        JsAst.expression_arrow_function_body expression
      | (statements, Expression { expression; _ }) ->
        let statements = Del.ConsEnd (statements, JsAst.return_statement expression) in
        JsAst.block_statement_arrow_function_body (Del.to_list statements)
      | (statements, Statement statement) ->
        let statements = Del.ConsEnd (statements, statement) in
        JsAst.block_statement_arrow_function_body (Del.to_list statements)
      in
      let expression = JsAst.arrow_function_expression params body in
      (Del.Empty, Expression { strategy = Lazy; expression })
    ))

  | Call { callee; argument } ->
    let (callee_statements, callee) = serialize_expression context callee in
    let (argument_statements, argument) = serialize_expression context argument in
    if callee.strategy = Eager && argument_statements <> Del.Empty then (
      let name = JsNamer.unique "$tmp" in
      let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name) (Some callee.expression) in
      let statements = Del.concat callee_statements (Cons (statement, argument_statements)) in
      let expression = JsAst.call_expression (JsAst.identifier_expression name) [argument.expression] in
      (statements, Expression { strategy = Eager; expression })
    ) else (
      let statements = Del.concat callee_statements argument_statements in
      let expression = JsAst.call_expression callee.expression [argument.expression] in
      (statements, Expression { strategy = Eager; expression })
    )

  | Binding { name; value; body } -> (
    let name' = JsNamer.unique name in
    let output' e = JsAst.expression_statement (JsAst.assignment_expression (JsAst.identifier_pattern name') e) in
    let value = serialize context value output' in
    let context = StringMap.add name name' context in
    match value with
    | (value_statements, Expression value) ->
      let (body_statements, body) = serialize context body output in
      let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name') (Some value.expression) in
      let statements = Del.concat value_statements (Cons (statement, body_statements)) in
      (statements, body)
    | (value_statements, Statement value) ->
      let (body_statements, body) = serialize context body output in
      let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name') None in
      let statements = Del.concat value_statements (Cons (statement, Cons (value, body_statements))) in
      (statements, body)
  )

  | Annotation { value; type_ = _ } -> serialize context value output

  | Conditional { test; consequent; alternate } -> (
    let (test_statements, test) = serialize_expression context test in
    let consequent = JsNamer.block (fun () -> serialize context consequent output) in
    let alternate = JsNamer.block (fun () -> serialize context alternate output) in
    match consequent, alternate with
    | (Empty, Expression consequent), (Empty, Expression alternate) ->
      let strategy = if (
        test.strategy = Lazy &&
        consequent.strategy = Lazy &&
        alternate.strategy = Lazy
      ) then Lazy else Eager in
      let expression = JsAst.conditional_expression test.expression consequent.expression alternate.expression in
      (test_statements, Expression { strategy; expression })

    | (consequent_statements, consequent), (alternate_statements, alternate) ->
      let consequent = match consequent with Expression e -> output e.expression | Statement s -> s in
      let alternate = match alternate with Expression e -> output e.expression | Statement s -> s in
      let consequent_statements = Del.to_list (Del.ConsEnd (consequent_statements, consequent)) in
      let alternate_statements = Del.to_list (Del.ConsEnd (alternate_statements, alternate)) in
      let statement = JsAst.if_statement test.expression consequent_statements alternate_statements in
      (test_statements, Statement statement)
  )

and serialize_expression context expression =
  let phi = lazy (JsNamer.unique "$phi") in
  let output e =
    let phi = Lazy.force phi in
    JsAst.expression_statement (JsAst.assignment_expression (JsAst.identifier_pattern phi) e)
  in
  match serialize context expression output with
  | (statements, Expression expression) -> (statements, expression)
  | (statements, Statement statement) ->
    let phi = Lazy.force phi in
    let phi_statement = JsAst.variable_declaration Let (JsAst.identifier_pattern phi) None in
    let statements = Del.ConsEnd (Del.ConsEnd (statements, phi_statement), statement) in
    let expression = JsAst.identifier_expression phi in
    (statements, { strategy = Lazy; expression })

(* Serializes a Brite expression to a JavaScript program. *)
let serialize expression =
  let (statements, { expression; _ }) = JsNamer.block (fun () -> serialize_expression StringMap.empty expression) in
  let statements = Del.to_list (ConsEnd (statements, JsAst.expression_statement expression)) in
  JsAst.program statements
