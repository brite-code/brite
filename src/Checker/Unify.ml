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
let rec unify prefix type1 type2 =
  match type1.Type.monotype_description, type2.Type.monotype_description with
  (* Variables with the same name unify without any further analysis. We rename
   * variables in type1 and type2 so that the same variable name does not
   * represent different bounds. *)
  | Variable { name = name1 }, Variable { name = name2 } when String.equal name1 name2 ->
    Ok ()

  (* Unify type1 with some other monotype. If the monotype is a variable then
   * we will perform some special handling. *)
  | Variable { name }, description2 -> (
    let bound1 = Prefix.lookup prefix name in
    match bound1.bound_type.polytype_description, description2 with
    (* If the bound for our variable is a monotype then recursively call `unify`
     * with that monotype. As per the normal-form monotype bound
     * rewrite rule. *)
    | Monotype type1, _ -> unify prefix type1 type2

    (* Two variables with different names were unified with one another. This
     * case is different because we need to merge the two variables together. *)
    | _, Variable { name = name2 } -> (
      let bound2 = Prefix.lookup prefix name2 in
      match bound2.bound_type.polytype_description with
      (* If the bound for our variable is a monotype then recursively call
       * `unify` with that monotype. As per the normal-form monotype bound
       * rewrite rule. Make sure we don’t fall into the next case where we try
       * to update the types to each other! *)
      | Monotype type2 -> unify prefix type1 type2

      (* Actually merge the two type variables together. *)
      | _ -> (
        let name1 = name in
        match unify_polytype prefix bound1.bound_type bound2.bound_type with
        | Error error -> Error error
        | Ok t ->
          let bound_kind = if (
            bound1.bound_kind = Flexible &&
            bound2.bound_kind = Flexible
          ) then Type.Flexible else Type.Rigid in
          Prefix.update2 prefix name1 name2 (Type.bound bound_kind t)
      )
    )

    (* Unify the polymorphic bound type with our other monotype. If the
     * unification is successful then update the type variable in our prefix to
     * our monotype. *)
    | _, _ -> (
      let type2 = Type.to_polytype type2 in
      match unify_polytype prefix bound1.bound_type type2 with
      | Error error -> Error error
      | Ok _ -> Prefix.update prefix name (Type.bound Rigid type2)
    )
  )

  (* Unify type2 with some other monotype. The other monotype is guaranteed
   * to not be a variable since that case would be caught by the match
   * case above. *)
  | _, Variable { name } -> (
    let bound2 = Prefix.lookup prefix name in
    match bound2.bound_type.polytype_description with
    (* If the bound for our variable is a monotype then recursively call `unify`
     * with that monotype. As per the normal-form monotype bound rewrite rule. *)
    | Monotype type2 -> unify prefix type1 type2

    (* Unify the polymorphic bound type with our other monotype. If the
     * unification is successful then update the type variable in our prefix to
     * our monotype. *)
    | _ -> (
      let type1 = Type.to_polytype type1 in
      match unify_polytype prefix type1 bound2.bound_type with
      | Error error -> Error error
      | Ok _ -> Prefix.update prefix name (Type.bound Rigid type1)
    )
  )

  (* Constant intrinsic types unify with each other no problem. *)
  | Boolean, Boolean -> Ok ()
  | Number, Number -> Ok ()

  (* Functions unify if the parameters and bodies unify.
   *
   * If both the unification of the parameters and bodies fail then we will
   * report two error diagnostics. However, we will only return the first error
   * from unify. *)
  | Function { parameter = parameter1; body = body1 },
    Function { parameter = parameter2; body = body2 } -> (
    let result1 = unify prefix parameter1 parameter2 in
    let result2 = unify prefix body1 body2 in
    match result1, result2 with
    | Error error, _ -> Error error
    | Ok (), Error error -> Error error
    | Ok (), Ok () -> Ok ()
  )

  (* Exhaustive match for failure case. Don’t use `_` since if we add a new type
   * we want an error telling us to add a case for that type to unification. *)
  | Boolean, _
  | Number, _
  | Function _, _ ->
    let type1 = Type.to_polytype type1 in
    let type2 = Type.to_polytype type2 in
    Error (Diagnostics.report_error (IncompatibleTypes { type1; type2 }))

(* Unifies two polytypes. When the two types are equivalent we return an ok
 * result with a type. This type is an instance of both our input types. That is
 * if `t1` and `t2` are our inputs and `t1 ≡ t2` holds then we return `t3` where
 * both `t1 ⊑ t3` and `t2 ⊑ t3` hold. *)
and unify_polytype prefix type1 type2 =
  match type1.Type.polytype_description, type2.Type.polytype_description with
  (* If either is bottom then return the other one. *)
  | Bottom, _ -> Ok type2
  | _, Bottom -> Ok type1

  (* If we have two monotypes then unify them. Don’t bother with creating a new
   * level or generalizing. *)
  | Monotype type1, Monotype type2 -> (
    match unify prefix type1 type2 with
    | Ok () -> Ok (Type.to_polytype type1)
    | Error error -> Error error
  )

  (* When two quantified types unify we instantiate their local bounds in the
   * prefix. We consider a monotype to be a quantified type with an empty bounds
   * list. We then unify the body of the two quantified types in the new prefix.
   * If unification is successful then we know that the two types we unified
   * must be equivalent. We then generalize type1 and return it. Since we know
   * our types to be equivalent it does not matter which one we return. If
   * unification returned an error then we also return that error. We do all
   * this in an isolated level so that we don’t quantify type variables that are
   * needed at an earlier level. *)

  | Quantify { bounds = bounds1; body = body1 }, Monotype type2 ->
    Prefix.level prefix (fun () -> (
      let type1 = Prefix.instantiate prefix bounds1 body1 in
      match unify prefix type1 type2 with
      | Ok () -> Ok (Prefix.generalize prefix type1)
      | Error error -> Error error
    ))

  | Monotype type1, Quantify { bounds = bounds2; body = body2 } ->
    Prefix.level prefix (fun () -> (
      let type2 = Prefix.instantiate prefix bounds2 body2 in
      match unify prefix type1 type2 with
      | Ok () -> Ok (Prefix.generalize prefix type1)
      | Error error -> Error error
    ))

  | Quantify { bounds = bounds1; body = body1 }, Quantify { bounds = bounds2; body = body2 } ->
    Prefix.level prefix (fun () -> (
      let type1 = Prefix.instantiate prefix bounds1 body1 in
      let type2 = Prefix.instantiate prefix bounds2 body2 in
      match unify prefix type1 type2 with
      | Ok () -> Ok (Prefix.generalize prefix type1)
      | Error error -> Error error
    ))
