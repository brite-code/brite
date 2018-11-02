(* IMPORTANT: It is expected that all free types in context are bound in the
 * prefix! If this is not true then we will panic.
 *
 * Type inference looks at an expression and produces a principal type for the
 * expression. The core algorithm is described in section 7.1 of the
 * [MLF Thesis][1].
 *
 * In theory:
 *
 * > A type inference problem is a triple `(Q, Γ, a)`, where all free type
 * > variables in `Γ` are bound in `Q`. A pair `(Q', o)` is a solution to this
 * > problem if `Q ⊑ Q'` and `(Q') Γ ⊦ a : o` holds.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
let rec infer prefix context expression =
  match expression.Expression.description with
  (* If the variable exists in context then return its type. Otherwise report an
   * unbound variable error and return the bottom type since the code will panic
   * at runtime. *)
  | Variable { name } -> (
    match StringMap.find_opt name context with
    | Some t -> t
    | None ->
      let _ = Diagnostic.report_error (UnboundVariable { name }) in
      Type.bottom
  )

  (* Return the constant types for constants. *)
  | Boolean _ -> Type.boolean_polytype
  | Number _ -> Type.number_polytype
  | String _ ->  Type.string_polytype

  (* Functions create a fresh type variable for their parameters and then infer
   * the body type with that type variable. It is expected that through
   * unification the parameter type will be solved. At the very end we
   * generalize the type variables created at this level which weren’t updated
   * to a higher level. *)
  | Function { parameter; body } ->
    Prefix.level prefix (fun () -> (
      let parameter_type = Prefix.fresh prefix in
      let context = StringMap.add parameter (Type.to_polytype parameter_type) context in
      let body_type = infer prefix context body in
      let body_type = Prefix.fresh_with_bound prefix (Type.bound Flexible body_type) in
      Prefix.generalize prefix (Type.function_ parameter_type body_type)
    ))

  (* For function calls infer the type of the callee and argument. Then unify
   * the type of the callee with a function type with the argument as the
   * parameter and a fresh type variable as the body. Through unification the
   * fresh type variable should be solved for. At the very end generalize the
   * body type and associated referenced type variables created at this level
   * which weren’t updated to a higher level. *)
  | Call { callee; argument } ->
    let callee_type = infer prefix context callee in
    let argument_type = infer prefix context argument in
    Prefix.level prefix (fun () -> (
      let callee_type = Prefix.fresh_with_bound prefix (Type.bound Flexible callee_type) in
      let argument_type = Prefix.fresh_with_bound prefix (Type.bound Flexible argument_type) in
      let body_type = Prefix.fresh prefix in
      let _ = Unify.unify prefix callee_type (Type.function_ argument_type body_type) in
      Prefix.generalize prefix body_type
    ))

  (* With bindings we infer the value for the type, add it to our context under
   * the provided name, and then infer the type for our body. *)
  | Binding { name; value; body } ->
    let value_type = infer prefix context value in
    let context = StringMap.add name value_type context in
    infer prefix context body
