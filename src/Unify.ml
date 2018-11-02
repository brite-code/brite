(* IMPORTANT: It is expected that all free type variables are bound in the
 * prefix! If this is not true we will panic.
 *
 * The unification algorithm takes a prefix and two monotypes asserting that the
 * types are equivalent under the prefix. However, what makes unification extra
 * special is that at the same time if we encounter flexible bounds in the
 * prefix then we update our prefix so that the types become equivalent under
 * the prefix.
 *
 * In the terms of the [MLF thesis][1] unification is defined as:
 *
 * > Definition 4.1.1 (Unification): A prefix `Q'` unifies `t1` and `t2` under
 * > `Q` if and only if `Q ⊑ Q'` and `(Q') t1 ≡ t2` hold.
 *
 * Our implementation is a bit different then the algorithm proposed in the
 * thesis. Our algorithm _never fails_. Instead our algorithm returns a result.
 * If the result is ok then the two types are equivalent. That is `(Q') t1 ≡ t2`
 * holds. If our algorithm result is an error then the two types are _not_
 * equivalent. That is `(Q') t1 ≡ t2` does _not_ hold. The prefix after
 * unification will always be an instance of the prefix before unification. That
 * is `Q ⊑ Q'` always holds.
 *
 * The caller of the unification algorithm which depends on equivalent types for
 * soundness must handle the error locally.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
let rec unify prefix actual expected =
  match actual.Type.monotype_description, expected.Type.monotype_description with
  (* Variables with the same name unify without any further analysis. We rename
   * variables in actual and expected so that the same variable name does not
   * represent different bounds. *)
  | Variable { name = actual_name }, Variable { name = expected_name }
      when String.equal actual_name expected_name ->
    Ok ()

  (* Unify actual with some other monotype. If the monotype is a variable then
   * we will perform some special handling. *)
  | Variable { name }, expected_description -> (
    let bound = Prefix.lookup prefix name in
    let actual = bound.Type.bound_type in
    match actual.Type.polytype_description, expected_description with
    (* If the bound for our variable is a monotype then recursively call `unify`
     * with that monotype. As per the normal-form monotype bound
     * rewrite rule. *)
    | Monotype actual, _ -> unify prefix actual expected

    (* Two variables with different names were unified with one another. This
     * case is different because we need to merge the two variables together. *)
    | _, Variable { name = expected_name } -> (
      let actual_name = name in
      let actual_bound = Prefix.lookup prefix actual_name in
      let expected_bound = Prefix.lookup prefix expected_name in
      match unify_polytype prefix actual_bound.bound_type expected_bound.bound_type with
      | Error error -> Error error
      | Ok t ->
        let bound_kind = if (
          actual_bound.bound_kind = Flexible &&
          expected_bound.bound_kind = Flexible
        ) then Type.Flexible else Type.Rigid in
        Prefix.update2 prefix actual_name expected_name (Type.bound bound_kind t)
    )

    (* Unify the polymorphic bound type with our other monotype. If the
     * unification is successful then update the type variable in our prefix to
     * our monotype. *)
    | _, _ -> (
      let expected = Type.to_polytype expected in
      match unify_polytype prefix actual expected with
      | Error error -> Error error
      | Ok _ -> Prefix.update prefix name (Type.bound Rigid expected)
    )
  )

  (* Unify expected with some other monotype. The other monotype is guaranteed
   * to not be a variable since that case would be caught by the match
   * case above. *)
  | _, Variable { name } -> (
    let bound = Prefix.lookup prefix name in
    let expected = bound.Type.bound_type in
    match expected.Type.polytype_description with
    (* If the bound for our variable is a monotype then recursively call `unify`
     * with that monotype. As per the normal-form monotype bound rewrite rule. *)
    | Monotype expected -> unify prefix actual expected

    (* Unify the polymorphic bound type with our other monotype. If the
     * unification is successful then update the type variable in our prefix to
     * our monotype. *)
    | _ -> (
      let actual = Type.to_polytype actual in
      match unify_polytype prefix actual expected with
      | Error error -> Error error
      | Ok _ -> Prefix.update prefix name (Type.bound Rigid actual)
    )
  )

  (* Constant intrinsic types unify with each other no problem. *)
  | Boolean, Boolean -> Ok ()
  | Number, Number -> Ok ()
  | String, String -> Ok ()

  (* Functions unify if the parameters and bodies unify.
   *
   * If both the unification of the parameters and bodies fail then we will
   * report two error diagnostics. However, we will only return the first error
   * from unify.
   *
   * We intentionally flip the unification order of actual and expected for our
   * function parameter _only_. This is because the function parameter is in an
   * input position (or is _contravariant_). *)
  | Function { parameter = actual_parameter; body = actual_body },
    Function { parameter = expected_parameter; body = expected_body } -> (
    let result1 = unify prefix expected_parameter actual_parameter in
    let result2 = unify prefix actual_body expected_body in
    match result1, result2 with
    | Error error, _ -> Error error
    | Ok (), Error error -> Error error
    | Ok (), Ok () -> Ok ()
  )

  (* Exhaustive match for failure case. Don’t use `_` since if we add a new type
   * we want an error telling us to add a case for that type to unification. *)
  | Boolean, _
  | Number, _
  | String, _
  | Function _, _ ->
    let actual = Type.to_polytype actual in
    let expected = Type.to_polytype expected in
    Error (Diagnostic.report_error (IncompatibleTypes { actual; expected }))

(* Unifies two polytypes. When the two types are equivalent we return an ok
 * result with a type. This type is an instance of both our input types. That is
 * if `t1` and `t2` are our inputs and `t1 ≡ t2` holds then we return `t3` where
 * both `t1 ⊑ t3` and `t2 ⊑ t3` hold. *)
and unify_polytype prefix actual expected =
  match actual.Type.polytype_description, expected.Type.polytype_description with
  (* If either is bottom then return the other one. *)
  | Bottom, _ -> Ok expected
  | _, Bottom -> Ok actual

  (* If we have two monotypes then unify them. Don’t bother with creating a new
   * level or generalizing. *)
  | Monotype actual, Monotype expected -> (
    match unify prefix actual expected with
    | Ok () -> Ok (Type.to_polytype actual)
    | Error error -> Error error
  )

  (* When two quantified types unify we instantiate their local bounds in the
   * prefix. We consider a monotype to be a quantified type with an empty bounds
   * list. We then unify the body of the two quantified types in the new prefix.
   * If unification is successful then we know that the two types we unified
   * must be equivalent. We then generalize the “actual” type and return it.
   * Since we know our types to be equivalent it does not matter which one we
   * return. If unification returned an error then we also return that error. We
   * do all this in an isolated level so that we don’t quantify type variables
   * that are needed at an earlier level. *)

  | Quantify { bounds = actual_bounds; body = actual }, Monotype expected ->
    Prefix.level prefix (fun () -> (
      let actual = Prefix.instantiate prefix actual_bounds actual in
      match unify prefix actual expected with
      | Ok () -> Ok (Prefix.generalize prefix actual)
      | Error error -> Error error
    ))

  | Monotype actual, Quantify { bounds = expected_bounds; body = expected } ->
    Prefix.level prefix (fun () -> (
      let expected = Prefix.instantiate prefix expected_bounds expected in
      match unify prefix actual expected with
      | Ok () -> Ok (Prefix.generalize prefix actual)
      | Error error -> Error error
    ))

  | Quantify { bounds = actual_bounds; body = actual }, Quantify { bounds = expected_bounds; body = expected } ->
    Prefix.level prefix (fun () -> (
      let actual = Prefix.instantiate prefix actual_bounds actual in
      let expected = Prefix.instantiate prefix expected_bounds expected in
      match unify prefix actual expected with
      | Ok () -> Ok (Prefix.generalize prefix actual)
      | Error error -> Error error
    ))
