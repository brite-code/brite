(* A data-type is needed here to make the type recursive. *)
type locals = Locals of (locals * Type.bound) StringMap.t [@@unboxed]

(* Checks if the projections of two types are equal. The projection of a type is
 * the type with all quantifications inlined no matter the flexibility of the
 * quantification bound. The projection function is written in Definition 1.3.2
 * of the [MLF thesis][1]. This function efficiently compares the projection of
 * two types under a prefix and returns true if the projections are equal to
 * one another.
 *
 * The prefix function is expected to always return a bound. It may panic if a
 * bound can’t be found for the provided name. It is expected that all type
 * variables in the prefix are unique. That is if we get a bound from the prefix
 * then we may use the same prefix function to get all the bounds for all of its
 * free type variables. Multiple calls to the prefix function should also return
 * the exact same type.
 *
 * This function is used to implement abstraction checks as according to
 * Property 2.1.3 (i) of the instance relation if one type abstracts another
 * then the types have the same skeletons.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
let projections_equal prefix t1 t2 =
  (* Adds bounds from a quantification to a map of locals. Each bound includes
   * the locals map at the point it was added. This is because if we need to
   * check equivalence with a bound then the bound will have a different set of
   * locals in scope then the point the bound was referenced at. *)
  let add_bounds_to_locals locals bounds =
    List.fold_left (fun locals (name, bound) -> (
      Locals (StringMap.add name (locals, bound) (let Locals locals = locals in locals))
    )) locals bounds
  in
  let rec monotype_projections_equal locals1 locals2 t1 t2 =
    match t1.Type.monotype_description, t2.Type.monotype_description with
    (* Unwrap variable from locals. Note that each local uses its own locals
     * map. This is so we can correctly check types with shadowed bounds
     * like `∀(x, x = x).x ≡ ⊥`. *)
    | Variable { name = name1 }, _ when StringMap.mem name1 (let Locals locals1 = locals1 in locals1) ->
      let (locals1, bound1) = StringMap.find name1 (let Locals locals1 = locals1 in locals1) in
      polytype_projections_equal locals1 locals2 bound1.Type.bound_type (Type.to_polytype t2)

    (* Unwrap variable from locals. Note that each local uses its own locals
     * map. This is so we can correctly check types with shadowed bounds
     * like `∀(x, x = x).x ≡ ⊥`. *)
    | _, Variable { name = name2 } when StringMap.mem name2 (let Locals locals2 = locals2 in locals2) ->
      let (locals2, bound2) = StringMap.find name2 (let Locals locals2 = locals2 in locals2) in
      polytype_projections_equal locals1 locals2 (Type.to_polytype t1) bound2.Type.bound_type

    (* Optimization: If neither variable exists as a local type and the names
     * equal each other then they are equal to the same type in the prefix. We
     * call the prefix function just in case the name does not exist and we need
     * to panic. *)
    | Variable { name = name1 }, Variable { name = name2 } when String.equal name1 name2 ->
      let _ = prefix name1 in
      true

    (* Unwrap variable from prefix. Note that we set an empty locals map when we
     * use a variable from the prefix. This is to avoid shadowed bounds like in
     * the relation `(x) ∀(x = x).x ≡ ⊥` *)
    | Variable { name = name1 }, _ ->
      let bound1 = prefix name1 in
      polytype_projections_equal (Locals StringMap.empty) locals2 bound1.Type.bound_type (Type.to_polytype t2)

    (* Unwrap variable from prefix. Note that we set an empty locals map when we
     * use a variable from the prefix. This is to avoid shadowed bounds like in
     * the relation `(x) ∀(x = x).x ≡ ⊥` *)
    | _, Variable { name = name2 } ->
      let bound2 = prefix name2 in
      polytype_projections_equal locals1 (Locals StringMap.empty) (Type.to_polytype t1) bound2.Type.bound_type

    (* Two functions are only equivalent if both their parameters and bodies
     * are equivalent. *)
    | Function { parameter = parameter1; body = body1 }, Function { parameter = parameter2; body = body2 } ->
      monotype_projections_equal locals1 locals2 parameter1 parameter2 &&
      monotype_projections_equal locals1 locals2 body1 body2

    (* Scalars are only equivalent with each other. *)
    | Boolean, Boolean
    | Number, Number
      -> true

    (* TODO *)
    | Row _, Row _ -> failwith "TODO"

    (* Exhaustive failure cases. *)
    | Function _, _
    | Boolean, _
    | Number, _
    | Row _, _
      -> false

  and polytype_projections_equal locals1 locals2 t1 t2 =
    match t1.Type.polytype_description, t2.Type.polytype_description with
    (* NOTE: This section for matching `Variable` is copied from
     * `monotype_projections_equal` above. We copy it since we need to unwrap
     * variables _before_ some of our polytype cases like the `Bottom` case.
     * Consider the equivalence `∀a.a ≡ ∀b.b`. Here `proj(∀a.a) = ⊥` and
     * `proj(∀b.b) = ⊥`. Since `⊥ = ⊥` our relation `∀a.a ≡ ∀b.b` holds. But in
     * order for our function to produce this result we need to unwrap variables
     * before our `Bottom` case. *)

    (* Unwrap variable from locals. See comment above. *)
    | Monotype { monotype_description = Variable { name = name1 }; _ }, _
        when StringMap.mem name1 (let Locals locals1 = locals1 in locals1) ->
      let (locals1, bound1) = StringMap.find name1 (let Locals locals1 = locals1 in locals1) in
      polytype_projections_equal locals1 locals2 bound1.Type.bound_type t2

    (* Unwrap variable from locals. See comment above. *)
    | _, Monotype { monotype_description = Variable { name = name2 }; _ }
        when StringMap.mem name2 (let Locals locals2 = locals2 in locals2) ->
      let (locals2, bound2) = StringMap.find name2 (let Locals locals2 = locals2 in locals2) in
      polytype_projections_equal locals1 locals2 t1 bound2.Type.bound_type

    (* Optimization: If neither variable exists as a local type and the names
     * equal each other then they are equal to the same type in the prefix. We
     * call the prefix function just in case the name does not exist and we need
     * to panic. *)
    | Monotype { monotype_description = Variable { name = name1 }; _ },
      Monotype { monotype_description = Variable { name = name2 }; _ }
        when String.equal name1 name2 ->
      let _ = prefix name1 in
      true

    (* Unwrap variable from prefix. See comment above. *)
    | Monotype { monotype_description = Variable { name = name1 }; _ }, _ ->
      let bound1 = prefix name1 in
      polytype_projections_equal (Locals StringMap.empty) locals2 bound1.Type.bound_type t2

    (* Unwrap variable from prefix. See comment above. *)
    | _, Monotype { monotype_description = Variable { name = name2 }; _ } ->
      let bound2 = prefix name2 in
      polytype_projections_equal locals1 (Locals StringMap.empty) t1 bound2.Type.bound_type

    (* Bottom is only equivalent with itself. *)
    | Bottom, Bottom -> true
    | Bottom, _ | _, Bottom -> false

    (* Perform a monotype equivalence check when we have two monotypes. *)
    | Monotype t1, Monotype t2 -> monotype_projections_equal locals1 locals2 t1 t2

    (* Unwrap quantifications and check monotype equivalence. *)
    | Quantify { bounds = bounds1; body = t1 }, Monotype t2 ->
      let locals1 = add_bounds_to_locals locals1 bounds1 in
      monotype_projections_equal locals1 locals2 t1 t2

    (* Unwrap quantifications and check monotype equivalence. *)
    | Monotype t1, Quantify { bounds = bounds2; body = t2 } ->
      let locals2 = add_bounds_to_locals locals2 bounds2 in
      monotype_projections_equal locals1 locals2 t1 t2

    (* Unwrap quantifications and check monotype equivalence. *)
    | Quantify { bounds = bounds1; body = t1 }, Quantify { bounds = bounds2; body = t2 } ->
      let locals1 = add_bounds_to_locals locals1 bounds1 in
      let locals2 = add_bounds_to_locals locals2 bounds2 in
      monotype_projections_equal locals1 locals2 t1 t2
  in
  polytype_projections_equal (Locals StringMap.empty) (Locals StringMap.empty) t1 t2

type state = X | Y | Z

module Polynomial = Map.Make(struct
  type t = (int * int * int)
  let compare (xs1, ys1, zs1) (xs2, ys2, zs2) =
    let k = xs1 - xs2 in
    if k <> 0 then k else
    let k = ys1 - ys2 in
    if k <> 0 then k else
    zs1 - zs2
end)

(* This is an interesting little function. In this function we detect if the
 * _quantity_ of flexible binders are different in the two types we are
 * provided. It can be tricky to understand _why_ this is important. Consider
 * the program:
 *
 * ```
 * // add1: number → number
 * let auto = λ(x: ∀a.a → a).x x in // auto: ∀(x = ∀a.a → a).x → x
 * auto add1
 * ```
 *
 * We better reject this program since we can’t call `add1 add1`! That program
 * would be ill-formed. This is what rigid bindings represent. The refusal to
 * instantiate a given type bound to a new type. We can’t update the bound
 * `b = ∀a.a → a` to `b = number → number` since that would be an instantiation
 * of `a`.
 *
 * This program is rejected by observing `projections_equal` returns false.
 * Here’s a program which needs `flexible_binders_unchanged` to be rejected:
 *
 * ```
 * // add1: number → number
 * let auto = λ(x: ∀a.a → a).x x in // auto: ∀(x = ∀a.a → a).x → x
 * (auto: ∀(x ≥ ∀a.a → a).x → x) add1
 * ```
 *
 * In this program we try to “loosen” the type of `auto` to
 * `∀(x ≥ ∀a.a → a).x → x` so that we can apply `add1`. Similarly, we must
 * reject this program or else we execute `add1 add1` which is unsound! To
 * reject this program `flexible_binders_unchanged` observes that we have a new
 * flexible binder in the annotation.
 *
 * So how does it work? Formally the [MLF thesis][1] describes this operation in
 * Lemma 2.7.8 as:
 *
 * > X ∉ w(o1) − w(o2)
 *
 * What is `X`? What is `w()`? Why are we subtracting? How is `X` an element of
 * a number? This makes sense if you read section 2.7.1, section 2.7.2, and
 * lemma 2.7.8.
 *
 * What’s happening is that `w(t)` calculates a “weight” number based on the
 * position of bindings in `t`. Except the “weight” isn’t really a number. It’s
 * a polynomial. A polynomial with three abstract variables `X`, `Y`, and `Z`.
 * So a weight looks like `X + X² + XY + Y² + YZ`. Except you can think of each
 * polynomial as a path. So `X * X` is actually the path to a bottom (`⊥`)
 * quantification of `≥≥`.
 *
 * Ok, look, I tried explaining it, but you should really just read section 2.7.
 * It has nice and helpful pictures for understanding the theory of this
 * function. Once you’re done with that come back here.
 *
 * Oh good, you’re back! In the implementation of `flexible_binders_unchanged`
 * we represent a polynomial as a map which takes a triple as the key. The
 * triple consists of the power of `X`, `Y`, and `Z` respectively. The value
 * represents the integer coefficient of the polynomial entry. For example the
 * polynomial `X + X² + XY + 2Z²` is represented by the map:
 *
 * ```
 * (X, Y, Z) -> n
 * --------------
 * (1, 0, 0) -> 1
 * (2, 0, 0) -> 1
 * (1, 1, 0) -> 1
 * (0, 0, 2) -> 2
 * ```
 *
 * Except we don’t care about entries where the power of `X` is 0 so the map we
 * build does not contain those.
 *
 * Instead of trying to subtract polynomials and then checking
 * `X ∉ w(o1) − w(o2)` we check to see that the two maps are equal. If they are
 * equal that is the same as running our more formal check.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
let flexible_binders_unchanged =
  (* Create a polynomial for the provided type. *)
  let rec loop state acc xs ys zs t =
    (* If we will never have the opportunity to add another `X` to our
     * polynomial and we have no `xs` then we can short-circuit since we know,
     * logically, no more entries will be added to `acc`. *)
    if not (state = X) && xs = 0 then acc else
    match t.Type.polytype_description with
    (* Monotypes do not contain binders. *)
    | Monotype _ -> acc

    (* If we find a bottom type and we have some `xs` then add this path to our
     * polynomial. The short-circuit above depends on our `xs = 0` check here
     * for correctness. *)
    | Bottom ->
      if xs = 0 then (
        acc
      ) else (
        acc |> Polynomial.update (xs, ys, zs) (function None -> Some 1 | Some n -> Some (n + 1))
      )

    (* For quantifications add one to the correct polynomial and recurse into
     * the polynomial’s bound. Also make sure to proceed to the next
     * state correctly. *)
    | Quantify { bounds; body = _ } ->
      List.fold_left (fun acc (_, bound) -> (
        match state, bound.Type.bound_flexibility with
        | X, Flexible -> loop X acc (xs + 1) ys zs bound.Type.bound_type
        | X, Rigid -> loop Y acc xs (ys + 1) zs bound.Type.bound_type
        | Y, Flexible -> loop Z acc xs ys (zs + 1) bound.Type.bound_type
        | Y, Rigid -> loop Y acc xs (ys + 1) zs bound.Type.bound_type
        | Z, _ -> loop Z acc xs ys (zs + 1) bound.Type.bound_type
      )) acc bounds
  in
  fun t1 t2 ->
    (* Assert that both types are in normal form. This is so that unused type
     * variables have been thrown away. *)
    assert (t1.Type.polytype_normal && t2.Type.polytype_normal);
    (* Create the polynomials for both of our types and check to make sure they
     * are equal. Remember that these polynomials exclude entries which do not
     * have an `X`. *)
    let p1 = loop X Polynomial.empty 0 0 0 t1 in
    let p2 = loop X Polynomial.empty 0 0 0 t2 in
    Polynomial.equal (fun n1 n2 -> n1 = n2) p1 p2

(* IMPORTANT: We assume that `(Q) t1 ⊑ t2` holds. Do not call this function with
 * two types which do not uphold this relation!
 *
 * The abstraction check algorithm defined in Figure 4.1 of the [MLF Thesis][1].
 * Also see Lemma 2.7.8 as it provides more context as to why this algorithm
 * is correct.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
let check prefix t1 t2 =
  let rec loop t1 t2 =
    match t1.Type.polytype_description, t2.Type.polytype_description with
    (* If the left type is a monotype then we may shortcut the following steps
     * and return true. *)
    | Monotype _, _ -> true

    (* If the right type is a variable our algorithm won’t work so try to find
     * the bound of this variable and recurse with that bound’s type. If the
     * bound is not rigid then our check fails.
     *
     * In the presentation of the abstraction-check algorithm in figure 4.1
     * every time we recurse we also execute check whether or not the
     * projections are equal again. However, this check is computationally
     * wasteful. Projections will always be equal so we don’t perform that
     * check again. *)
    | _, Monotype { monotype_description = Variable { name }; _ } ->
      let bound = prefix name in
      if bound.Type.bound_flexibility = Type.Rigid then loop t1 bound.Type.bound_type else false

    (* Finally, we expect the flexible binders to not have changed between the
     * two types. *)
    | _, _ -> flexible_binders_unchanged t1 t2
  in
  (* If the projections of the two types are not equal then the check fails by
   * Property 2.1.3 (i) *)
  if not (projections_equal prefix t1 t2) then false else loop t1 t2
