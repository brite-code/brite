(* The evaluation strategy of an expression. Eager if the expression must be
 * evaluated in exactly the right order. That is if we have a call to `f()` and
 * then a call to `g()` we can’t reorder those calls. Lazy if the expression may
 * be evaluated anywhere.
 *
 * This distinction is important for examples like the following:
 *
 * ```
 * f(print("foo"), (print("bar"); print("qux")))
 * ```
 *
 * In this language we have sequence expression denoted with the semicolon (`;`)
 * operator. A naïve JavScript compiler would turn this into:
 *
 * ```js
 * print("bar");
 * f(print("foo"), print("qux"));
 * ```
 *
 * Since JavaScript does not have a sequence _expression_. Instead JavaScript
 * has statements. The above code incorrectly reorders `print("bar")` over
 * `print("foo")`! We solve this by only reordering expressions with a lazy
 * evaluation strategy. *)
type strategy =
  | Eager
  | Lazy

(* A serialized expression is the evaluation strategy and the
 * JavaScript expression. *)
type serialized_expression = {
  strategy: strategy;
  expression: JsAst.expression;
}

(* The serialization result may be either an expression or a statement. More on
 * that below. *)
type serialized_result =
  | Expression of serialized_expression
  | Statement of JsAst.statement

(* Serializes an expression under the provided context. In our language
 * expressions don’t correspond one-to-one with JavaScript expressions. For
 * example, in our language bindings are expressions. We have block expressions,
 * and so on. So serializing returns a list of JavaScript statements which
 * represent the side-effects of our expression and either an expression or
 * statement which represents the result.
 *
 * If we return a statement it is usually because execution forked. For instance
 * we have an if-statement. The `output` function is used to create the
 * statement that “returns” an expression in these forked execution cases. The
 * `output` function should only be called when we return a statement. Never
 * when we return an expression. *)
let rec serialize context expression output =
  match expression.Expression.description with
  (* Find the true identifier for our variable in the context map. Panic if we
   * don’t find it. Serialization should be given a type checked expression
   * which should have made sure that all variable names will always
   * be available. *)
  | Variable { name } ->
    let expression = JsAst.identifier_expression (StringMap.find name context) in
    (Del.Empty, Expression { strategy = Lazy; expression })

  (* A boolean expression turns into a boolean literal. *)
  | Boolean value ->
    let expression = JsAst.boolean_literal value in
    (Del.Empty, Expression { strategy = Lazy; expression })

  (* A number expression turns into a numeric literal. *)
  | Number value ->
    let expression = JsAst.numeric_literal value in
    (Del.Empty, Expression { strategy = Lazy; expression })

  (* Non-recursive function expressions are turned into arrow functions. We
   * introduce the function parameter into the context and serialize our
   * function’s body using a return statement as the `output` function.
   *
   * If we serialize a single expression with no statements then we use an
   * expression arrow function body. Otherwise we use a block statement arrow
   * function body.
   *
   * We plan to have a minification and bundling step using Node.js tools which
   * is why we emit ES5+ code. *)
  | Function { parameter; body } ->
    (* Create a new JavaScript block. We can reuse names in each block. *)
    JsNamer.block (fun () -> (
      (* Introduce the parameter into context with a JavaScript identifier. *)
      let parameter' = JsNamer.unique parameter in
      let context = StringMap.add parameter parameter' context in
      let params = [JsAst.identifier_pattern parameter'] in
      (* Serialize the body turning it into the smallest possible arrow
       * function body. *)
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
      (* Create the final arrow function expression. Creating an arrow function
       * has no notable side-effects so we use a lazy evaluation strategy. *)
      let expression = JsAst.arrow_function_expression params body in
      (Del.Empty, Expression { strategy = Lazy; expression })
    ))

  (* Calling functions is interesting because we have multiple expressions in
   * the same expression. Which means we need to make sure that we respect our
   * eager/lazy evaluation strategy. *)
  | Call { callee; argument } ->
    (* Serialize the callee and argument to an expression. This will generate a
     * phi node if serialization returns a statement. *)
    let (callee_statements, callee) = serialize_expression context callee in
    let (argument_statements, argument) = serialize_expression context argument in
    (* If our callee is evaluated eagerly but we have some statements then we
     * need to take care to evaluate our argument’s statements _after_ we
     * evaluate the callee. A naïve implementation wouldn’t track eager/lazy
     * evaluation strategies and evaluate the argument statements _before_ the
     * callee which is semantically wrong. *)
    if callee.strategy = Eager && argument_statements <> Del.Empty then (
      (* Create a temporary variable where we can hold the callee evaluation. *)
      let name = JsNamer.unique "$tmp" in
      (* Update the statements list so that we have:
       *
       * 1. The callee statements.
       * 2. The callee execution.
       * 3. The argument statements. *)
      let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name) (Some callee.expression) in
      let statements = Del.concat callee_statements (Cons (statement, argument_statements)) in
      (* Create the call expression. *)
      let expression = JsAst.call_expression (JsAst.identifier_expression name) [argument.expression] in
      (statements, Expression { strategy = Eager; expression })
    ) else (
      (* Create the call expression without worrying about execution order since
       * either callee is lazy or our argument has no statements. *)
      let statements = Del.concat callee_statements argument_statements in
      let expression = JsAst.call_expression callee.expression [argument.expression] in
      (statements, Expression { strategy = Eager; expression })
    )

  (* Serialize a binding’s value and body with the binding name in context. The
   * value will be assigned to the binding name. If serializing the value
   * returns a statement then the output function will assign to the
   * binding name. *)
  | Binding { name; value; body } -> (
    (* Generate a new unique name in our scope. Serialize our value without
     * adding that name to our context. *)
    let name' = JsNamer.unique name in
    let output' e = JsAst.expression_statement (JsAst.assignment_expression (JsAst.identifier_pattern name') e) in
    let value = serialize context value output' in
    let context = StringMap.add name name' context in
    match value with
    (* If serializing the value returned an expression then assign that directly
     * in a variable declaration. *)
    | (value_statements, Expression value) ->
      let (body_statements, body) = serialize context body output in
      let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name') (Some value.expression) in
      let statements = Del.concat value_statements (Cons (statement, body_statements)) in
      (statements, body)

    (* If serializing the value returned a statement then declare our variable
     * but don’t assign anything to it. The variable should be assigned in the
     * statement we serialized. *)
    | (value_statements, Statement value) ->
      let (body_statements, body) = serialize context body output in
      let statement = JsAst.variable_declaration Let (JsAst.identifier_pattern name') None in
      let statements = Del.concat value_statements (Cons (statement, Cons (value, body_statements))) in
      (statements, body)
  )

  (* Types are discarded at runtime. *)
  | Annotation { value; type_ = _ } -> serialize context value output

  (* Conditionals are interesting. For conditionals we either create a
   * conditional expression (`c ? t : f`) or an if statement
   * (`if (c) { t } else { f }`) depending on whether our consequent or
   * alternate have some statements. If we serialize an if expression then
   * execution “forks” and we will need to call our `output` function. *)
  | Conditional { test; consequent; alternate } -> (
    (* Serialize our test expression in the current scope and serialize our
     * consequent and alternate expressions in new block scopes. *)
    let (test_statements, test) = serialize_expression context test in
    let consequent = JsNamer.block (fun () -> serialize context consequent output) in
    let alternate = JsNamer.block (fun () -> serialize context alternate output) in
    match consequent, alternate with
    (* If both consequent and alternate are expressions with no statements then
     * we may serialize a conditional expression. *)
    | (Empty, Expression consequent), (Empty, Expression alternate) ->
      (* The conditional expression is only lazy if all the expressions within
       * it are lazy. *)
      let strategy = if (
        test.strategy = Lazy &&
        consequent.strategy = Lazy &&
        alternate.strategy = Lazy
      ) then Lazy else Eager in
      let expression = JsAst.conditional_expression test.expression consequent.expression alternate.expression in
      (test_statements, Expression { strategy; expression })
    (* Otherwise we want to generate an if statement. *)
    | (consequent_statements, consequent), (alternate_statements, alternate) ->
      (* Convert expressions to statements by calling `output`. *)
      let consequent = match consequent with Expression e -> output e.expression | Statement s -> s in
      let alternate = match alternate with Expression e -> output e.expression | Statement s -> s in
      (* Add the statements to the end of our lists. *)
      let consequent_statements = Del.to_list (Del.ConsEnd (consequent_statements, consequent)) in
      let alternate_statements = Del.to_list (Del.ConsEnd (alternate_statements, alternate)) in
      (* Create the final if statement and return it. *)
      let statement = JsAst.if_statement test.expression consequent_statements alternate_statements in
      (test_statements, Statement statement)
  )

(* Serializes the provided expression and always returns a serialized JavaScript
 * expression instead of sometimes returning a statement like `serialize`.
 * Calls `serialize` under the hood and if `serialize` returns a statement we
 * introduce a “phi” node to join the forked statement. We take the name “phi”
 * from [Static single assignment (SSA) form][1].
 *
 * [1]: https://en.wikipedia.org/wiki/Static_single_assignment_form *)
and serialize_expression context expression =
  (* Only create a new name for our phi variable if we need one. *)
  let phi = lazy (JsNamer.unique "$phi") in
  (* If output is called then we assign to the phi variable. *)
  let output e =
    let phi = Lazy.force phi in
    JsAst.expression_statement (JsAst.assignment_expression (JsAst.identifier_pattern phi) e)
  in
  (* Call serialize... *)
  match serialize context expression output with
  (* ...if it returns an expression then everything is good. *)
  | (statements, Expression expression) -> (statements, expression)
  (* ...if it returns a statement then we return the phi node as
   * our expression. *)
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
