(* Checks a parsed monotype to make sure it is well formed under the provided
 * context. Converts to a proper kinded monotype and emits error diagnostics. *)
let rec check_monotype context kind t =
  let t = match t.Type.monotype_description with
  (* Variables get their kind from our context. If no variable exists in our
   * context then report an error and use the expected kind. *)
  | Variable { name; kind = () } -> (
    match StringMap.find_opt name context with
    | Some kind -> Type.variable_with_kind kind name
    | None ->
      let error = Diagnostics.report_error (UnboundTypeVariable { name }) in
      Type.error kind error
  )

  (* Constant types stay the same. *)
  | Boolean -> Type.boolean
  | Number -> Type.number

  (* Check the parameters and body of a function type and then return a new
   * function type with the checked types. *)
  | Function { parameter; body } ->
    let parameter = check_monotype context Kind.value parameter in
    let body = check_monotype context Kind.value body in
    Type.function_ parameter body

  (* Constant types stay the same. *)
  | RowEmpty -> Type.row_empty

  (* Check the entries of a row extension with the value kind. Check the
   * extension of a row extension with the row kind. This check will disallow
   * things like “numbers” or “booleans” in a row extension. *)
  | RowExtension { entries; extension } ->
    let entries = Nel.map (fun (label, type_) -> (label, check_monotype context Kind.value type_)) entries in
    let extension = check_monotype context Kind.row extension in
    Type.row_extension entries extension

  (* Use the provided kind for error types. *)
  | Error { error; kind = () } ->
    Type.error kind error
  in
  (* Unify the checked monotype’s kind with the provided kind. If everything is
   * ok then we can return the type. Otherwise we need to return an error type. *)
  match Kind.unify (Type.monotype_kind t) kind with
  | Ok () -> t
  | Error error -> Type.error kind error

(* Checks a parsed polytype to make sure it is well formed under the provided
 * context. Converts to a proper kinded polytype and emits error diagnostics. *)
let rec check_polytype context kind t =
  let t = match t.Type.polytype_description with
  (* Forward monotypes to the `check_monotype` function. *)
  | Monotype t -> Type.to_polytype (check_monotype context kind t)

  (* Bottom types get the provided kind. *)
  | Bottom { kind = () } -> Type.bottom_with_kind kind

  (* For quantified types we check each bound not knowing what the kind will be,
   * building up a context along the way. When a bound is used we unify its
   * kind with the expected kind. *)
  | Quantify { bounds; body } ->
    let (context, rev_bounds) = List.fold_left (fun (context, rev_bounds) (name, bound) ->
      let bound_type = check_polytype context (Kind.unknown ()) bound.Type.bound_type in
      let bound = Type.bound bound.bound_flexibility bound_type in
      let context = StringMap.add name (Type.kind bound_type) context in
      (context, (name, bound) :: rev_bounds)
    ) (context, []) (Nel.to_list bounds) in
    let bounds = Nel.from_list (List.rev rev_bounds) in
    let body = check_monotype context kind body in
    Type.quantify bounds body
  in
  (* Unify the checked polytype’s kind with the provided kind. If everything is
   * ok then we can return the type. Otherwise we need to return an error type. *)
  match Kind.unify (Type.kind t) kind with
  | Ok () -> t
  | Error error -> Type.to_polytype (Type.error kind error)

let check = check_polytype
