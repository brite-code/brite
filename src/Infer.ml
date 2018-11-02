(* Converts the monotype AST into a proper monotype. Emits diagnostics while
 * doing so. For example when a variable name is not found. The resulting type
 * is also converted to normal form. *)
let rec convert_monotype t =
  match t.Ast.monotype_description with
  | Variable { name } -> Type.variable name
  | Boolean -> Type.boolean
  | Number -> Type.number
  | String -> Type.string
  | Function { parameter; body } ->
    Type.function_ (convert_monotype parameter) (convert_monotype body)

(* Converts the polytype AST into a proper polytype. Emits diagnostics while
 * doing so. For example when a variable name is not found. The resulting type
 * is also converted to normal form. *)
let rec convert_polytype t =
  let t = match t.Ast.polytype_description with
  | Monotype t -> Type.to_polytype (convert_monotype t)
  | Bottom -> Type.bottom
  | Quantify { bounds; body } ->
    let bounds = List.map (fun (name, bound) -> (
      let bound_kind = match bound.Ast.bound_kind with
      | Flexible -> Type.Flexible
      | Rigid -> Type.Rigid
      in
      (name, Type.bound bound_kind (convert_polytype bound.bound_type))
    )) bounds in
    Type.quantify bounds (Type.to_polytype (convert_monotype body))
  in
  (* We need to report an error for missing bounds! To proceed with type
   * checking we quantify our type locally with bounds for the missing
   * type variables.
   *
   * TODO: It is unsound to do this. Consider: `add1 ("foo": nope)`. Here `nope`
   * has the bottom type. We need to make sure we crash at runtime! *)
  let missing_bounds = List.map (fun name ->
    let _ = Diagnostic.report_error (UnboundTypeVariable { name }) in
    (name, Type.unbound)
  ) (StringSet.elements (Lazy.force t.polytype_free_variables)) in
  Type.quantify missing_bounds t

(* IMPORTANT: It is expected that all free types in context are bound in the
 * prefix! If this is not true then we may panic.
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
  match expression.Ast.expression_description with
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
  | Boolean _ -> Type.to_polytype Type.boolean
  | Number _ -> Type.to_polytype Type.number
  | String _ -> Type.to_polytype Type.string

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

  (* A type annotation tests whether the annotated value has a type equivalent
   * to the annotation. We test the equivalence of the two types with
   * unification.
   *
   * Notably we use a rigid type bound for the annotation. This is because we
   * don’t want to instantiate bounds of the annotation. Otherwise we might let
   * `(add1: ∀a.a → a)` pass type checking! This would allow us to pass _any
   * value_ to the `add1` function whose signature is `number → number`. This
   * would be bad. We need to error. *)
  | Annotation { value; type_ } ->
    let value_type = infer prefix context value in
    let type_ = convert_polytype type_ in
    Prefix.level prefix (fun () -> (
      let value_type = Prefix.fresh_with_bound prefix (Type.bound Flexible value_type) in
      let type_' = Prefix.fresh_with_bound prefix (Type.bound Rigid type_) in
      let _ = Unify.unify prefix value_type type_' in
      type_
    ))
