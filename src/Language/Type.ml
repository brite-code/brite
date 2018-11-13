type 'k base_monotype = {
  monotype_free_variables: StringSet.t Lazy.t;
  monotype_description: 'k monotype_description;
}

and 'k monotype_description =
  (* `x` *)
  | Variable of { name: string; kind: 'k }

  (* `boolean` *)
  | Boolean

  (* `number` *)
  | Number

  (* `T1 → T2` *)
  | Function of { parameter: 'k base_monotype; body: 'k base_monotype }

  (* `(||)` *)
  | RowEmpty

  (* `(| l: T1 | T2 |)` *)
  (* TODO: Kind analysis of parsed types. *)
  | RowExtension of { entries: (string * 'k base_monotype) Nel.t; extension: 'k base_monotype }

  (* The programmer might write a type annotation incorrectly. In that case we
   * insert an error node into the type tree. *)
  | Error of { kind: 'k; error: Diagnostics.t }

type bound_flexibility = Flexible | Rigid

type 'k base_polytype = {
  polytype_normal: bool;
  polytype_free_variables: StringSet.t Lazy.t;
  polytype_description: 'k polytype_description;
}

and 'k polytype_description =
  (* Inherits from monotype. *)
  | Monotype of 'k base_monotype

  (* `⊥` *)
  | Bottom of { kind: 'k }

  (* `∀x.T`, `∀(x = T1).T2`, `∀(x ≥ T1).T2` *)
  | Quantify of { bounds: (string * ('k base_bound)) Nel.t; body: 'k base_monotype }

and 'k base_bound = {
  bound_flexibility: bound_flexibility;
  bound_type: 'k base_polytype;
}

type parse_monotype = unit base_monotype
type parse_polytype = unit base_polytype
type parse_bound = unit base_bound
type monotype = Kind.t base_monotype
type polytype = Kind.t base_polytype
type bound = Kind.t base_bound

(* Creates a new variable monotype. *)
let variable name =
  {
    monotype_free_variables = lazy (StringSet.singleton name);
    monotype_description = Variable { name; kind = () };
  }

(* Creates a new variable monotype with a kind. *)
let variable_with_kind kind name =
  {
    monotype_free_variables = lazy (StringSet.singleton name);
    monotype_description = Variable { name; kind };
  }

(* Boolean monotype. *)
let boolean =
  {
    monotype_free_variables = lazy StringSet.empty;
    monotype_description = Boolean;
  }

(* Number monotype. *)
let number =
  {
    monotype_free_variables = lazy StringSet.empty;
    monotype_description = Number;
  }

(* Creates a new function monotype. *)
let function_ parameter body =
  {
    monotype_free_variables = lazy (StringSet.union
      (Lazy.force parameter.monotype_free_variables) (Lazy.force body.monotype_free_variables));
    monotype_description = Function { parameter; body }
  }

(* Empty row monotype. *)
let row_empty =
  {
    monotype_free_variables = lazy StringSet.empty;
    monotype_description = RowEmpty;
  }

(* Creates a new row extension monotype. *)
let row_extension entries extension =
  {
    monotype_free_variables = lazy (
      let free = Lazy.force extension.monotype_free_variables in
      Nel.fold_left (fun free (_, type_) -> (
        StringSet.union free (Lazy.force type_.monotype_free_variables)
      )) free entries
    );
    monotype_description = RowExtension { entries; extension };
  }

(* Converts a monotype into a polytype. *)
let to_polytype t =
  {
    polytype_normal = true;
    polytype_free_variables = t.monotype_free_variables;
    polytype_description = Monotype t;
  }

(* Bottom polytype. *)
let bottom =
  {
    polytype_normal = true;
    polytype_free_variables = lazy StringSet.empty;
    polytype_description = Bottom { kind = () };
  }

(* Bottom polytype with the provided kind. *)
let bottom_with_kind kind =
  {
    polytype_normal = true;
    polytype_free_variables = lazy StringSet.empty;
    polytype_description = Bottom { kind };
  }

(* Creates a type bound. *)
let bound flexibility type_ = { bound_flexibility = flexibility; bound_type = type_ }

(* A flexible bottom bound. *)
let unbounded = bound Flexible bottom

(* A flexible bottom bound with a value kind. *)
let unbounded_value = bound Flexible (bottom_with_kind Kind.value)

(* Quantifies a monotype by some bounds. The free type variables of quantified
 * types will not include the free type variables of unused bounds. This is to
 * be consistent with the normal form of the quantified type. *)
let quantify bounds body =
  {
    polytype_normal = false;
    polytype_free_variables = lazy (Nel.fold_right (fun (name, bound) free -> (
      if not (StringSet.mem name free) then free else
      free |> StringSet.remove name |> StringSet.union (Lazy.force bound.bound_type.polytype_free_variables)
    )) bounds (Lazy.force body.monotype_free_variables));
    polytype_description = Quantify { bounds; body };
  }

(* An error type which we insert when the form of a type is incorrect, but we
 * want to continue type checking. *)
let error kind error =
  {
    monotype_free_variables = lazy StringSet.empty;
    monotype_description = Error { kind; error };
  }

(* Returns the kind of the provided monotype. *)
let kind_monotype t =
  match t.monotype_description with
  | Variable { kind; _ } -> kind
  | Boolean -> Kind.value
  | Number -> Kind.value
  | Function _ -> Kind.value
  | RowEmpty -> Kind.row
  | RowExtension _ -> Kind.row
  | Error { kind; _ } -> kind

(* Returns the kind of the provided polytype. *)
let kind t =
  match t.polytype_description with
  | Monotype t -> kind_monotype t
  | Bottom { kind } -> kind
  | Quantify { bounds = _; body } -> kind_monotype body

(* Determines if a type needs some substitutions by looking at the types free
 * variables. If a substitution exists for any free variable then the type does
 * need a substitution. *)
let needs_substitution substitutions free_variables =
  (* NOTE: If the size of substitutions is smaller then the size of free
   * variables it would be faster to iterate through substitutions. However,
   * looking up the size of maps/sets in OCaml is O(n). *)
  if StringMap.is_empty substitutions then false else
  StringSet.exists (fun name -> StringMap.mem name substitutions) free_variables

(* Substitutes the free variables of the provided type with a substitution if
 * one was made available in the substitutions map. Returns nothing if no
 * substitution was made. *)
let rec substitute_monotype substitutions t =
  match t.monotype_description with
  (* If a substitution exists for this variable then replace our variable with
   * that substitution. *)
  | Variable { name } -> StringMap.find_opt name substitutions
  (* Types with no type variables will never be substituted. *)
  | Boolean
  | Number
  | RowEmpty
    -> None
  (* TODO *)
  | Error _ -> failwith "TODO"
  (* Look at the free variables for our type. If we don’t need a substitution
   * then just return the type. We put this here before
   * unconditionally recursing. *)
  | _ when not (needs_substitution substitutions (Lazy.force t.monotype_free_variables)) -> None
  (* Unconditionally substitute function types since we know in some child there
   * is a type variable which should be substituted. *)
  | Function { parameter; body } ->
    let parameter = match substitute_monotype substitutions parameter with Some t -> t | None -> parameter in
    let body = match substitute_monotype substitutions body with Some t -> t | None -> body in
    Some (function_ parameter body)
  (* Unconditionally substitute row types since we know in some child there is a
   * type variable which should be substituted. *)
  | RowExtension { entries; extension } ->
    let entries = Nel.map (fun entry -> (
      let (label, type_) = entry in
      match substitute_monotype substitutions type_ with
      | Some type_ -> (label, type_)
      | None -> entry
    )) entries in
    let extension = match substitute_monotype substitutions extension with Some t -> t | None -> t in
    Some (row_extension entries extension)

(* Substitutes the free variables of the provided type with a substitution if
 * one was made available in the substitutions map. Does not substitute
 * variables bound locally if they shadow a substitution. Returns nothing if no
 * substitution was made. *)
let rec substitute_polytype substitutions t =
  match t.polytype_description with
  (* Monotypes are substituted with a different function. *)
  | Monotype t -> (match substitute_monotype substitutions t with Some t -> Some (to_polytype t) | None -> None)
  (* No free type variables in the bottom type! *)
  | Bottom _ -> None
  (* Look at the free variables for our type. If we don’t need a substitution
   * then just return the type. We put this here before
   * unconditionally recursing. *)
  | _ when not (needs_substitution substitutions (Lazy.force t.polytype_free_variables)) -> None
  (* Substitute the quantified bounds and the quantified body. *)
  | Quantify { bounds; body } ->
    let (substitutions, bounds) = List.fold_left (fun (substitutions, bounds) entry -> (
      let (name, bound) = entry in
      let bound_type = substitute_polytype substitutions bound.bound_type in
      let substitutions = StringMap.remove name substitutions in
      match bound_type with
      | None -> (substitutions, entry :: bounds)
      | Some bound_type -> (substitutions, (name, { bound with bound_type }) :: bounds)
    )) (substitutions, []) (Nel.to_list bounds) in
    let body = match substitute_monotype substitutions body with Some t -> t | None -> body in
    (* Create the final quantified type. It is in normal form if the type we
     * originally substituted was in normal form. Substitution only expands
     * variables to other monotypes so a substitution will never affect whether
     * or not we are in normal form. *)
    let t' = quantify (Nel.from_list (List.rev bounds)) body in
    let t' = { t' with polytype_normal = t.polytype_normal } in
    Some t'

(* Converts a type to normal form as described in Definition 1.5.5 of the [MLF
 * thesis][1]. Returns nothing if the type is already in normal form.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
let rec normal (t: polytype) =
  if t.polytype_normal then None else
  match t.polytype_description with
  (* These polytypes should always have `polytype_normal = true`. *)
  | Bottom _ -> assert false
  | Monotype _ -> assert false

  | Quantify { bounds; body } ->
    (* Loops through the bounds of a quantified type in reverse order and
     * removes unused bounds and if the body is a variable then we inline the
     * bound referenced by that variable. Tail recursive and processes the
     * reversed bounds list created by the `loop` function defined below.
     *
     * The arguments are defined as:
     *
     * - `free`: The free type variables at this point in the iteration. We drop
     *   bounds which are not contained in this set.
     * - `bounds`: The forward list of bounds we are accumulating.
     * - `bounds_rev`: The reverse list of bounds we are iterating.
     * - `body`: The current body of the quantified type we are normalizing. *)
    let rec loop_rev free bounds bounds_rev body =
      match bounds_rev, body.monotype_description with
      (* If we have no more bounds to iterate then construct our
       * final polytype. *)
      | [], _ ->
        if bounds = [] then (
          to_polytype body
        ) else (
          let bounds = Nel.from_list bounds in
          {
            polytype_normal = true;
            polytype_free_variables = lazy free;
            polytype_description = Quantify { bounds; body }
          }
        )

      (* If our body is a variable and our last bound is referenced by the
       * variable then replace our body with that bound. *)
      | (name, bound) :: bounds_rev, Variable { name = name' }
          when name = name' && bounds = [] -> (
        let body = bound.bound_type in
        let free = Lazy.force body.polytype_free_variables in
        match body.polytype_description with
        | Bottom _ -> body
        | Monotype body -> loop_rev free [] bounds_rev body
        | Quantify { bounds; body } -> loop_rev free (Nel.to_list bounds) bounds_rev body
      )

      (* If our bound is unused then don’t add it to our final bounds list.
       * Otherwise add the bound and its free variables. *)
      | (name, bound) :: bounds_rev, _ ->
        if StringSet.mem name free then (
          let free = free
            |> StringSet.remove name
            |> StringSet.union (Lazy.force bound.bound_type.polytype_free_variables)
          in
          loop_rev free ((name, bound) :: bounds) bounds_rev body
        ) else (
          loop_rev free bounds bounds_rev body
        )
    in
    (* Loops through the bounds of a quantified type inlining monotype bounds.
     * Tail recursive and accumulates a reversed list of bounds which we pass
     * to the `loop_rev` function defined above.
     *
     * The arguments are defined as:
     *
     * - `seen`: A set containing the names of bounds we have seen while
     *   iterating which have not been renamed.
     * - `captured`: A set containing the names of bounds which have been
     *   captured by the substitutions map. All the names in this set must
     *   continue to be available at the point in which we apply the
     *   substitution. Therefore if we see a bound which shadows a name in
     *   `captured` we must rename that bound.
     * - `substitutions`: A map of variable names to monotypes which should be
     *   substituted for those variable names.
     * - `bounds`: The tail of the current bounds we are iterating through.
     *   Every iteration removes the head from the bounds list and processes it.
     * - `bounds_rev`: An accumulator. We add all of the bounds that we remove
     *   from `bounds` to this list in reverse. *)
    let rec loop seen captured substitutions bounds bounds_rev =
      match bounds with
      (* If we have processed all our bounds then return the new quantified
       * type in normal form. *)
      | [] ->
        let body = match substitute_monotype substitutions body with Some body -> body | None -> body in
        let free = Lazy.force body.monotype_free_variables in
        loop_rev free [] bounds_rev body

      (* Process a the next bound in the list. *)
      | entry :: bounds -> (
        (* Convert the bound’s type to normal form. *)
        let entry = match normal (snd entry).bound_type with
        | Some t -> let (name, bound) = entry in (name, { bound with bound_type = t })
        | None -> entry
        in
        match (snd entry).bound_type.polytype_description with
        (* If our bound is a monotype then we want to inline that monotype
         * wherever a reference appears. Ignoring the bound flexibility. We also
         * want to rename any free variables that this bound captures in
         * subsequent bounds. *)
        | Monotype t ->
          let (name, _) = entry in
          let t = match substitute_monotype substitutions t with Some t -> t | None -> t in
          let substitutions = StringMap.add name t substitutions in
          let captured = StringSet.union captured (Lazy.force t.monotype_free_variables) in
          loop seen captured substitutions bounds bounds_rev

        (* Process a bound to be added to the resulting list. *)
        | _ ->
          (* If the name of this bound is captured in the substitutions map then
           * we need to generate a new name. Then we need to substitute that new
           * name for the old one.
           *
           * If the name of this bound is not captured then we need to make sure
           * we don’t substitute this name anymore and that we add the name to
           * `seen` so we don’t override it when generating a new name. *)
          let (seen, captured, substitutions, entry) = if StringSet.mem (fst entry) captured then (
            let (name, bound) = entry in
            let name' = Namer.unique (fun name -> StringSet.mem name seen || StringSet.mem name captured) name in
            let entry = (name', bound) in
            let substitutions = StringMap.add name (variable_with_kind (kind bound.bound_type) name') substitutions in
            let captured = StringSet.add name' captured in
            (seen, captured, substitutions, entry)
          ) else (
            let (name, _) = entry in
            let substitutions = StringMap.remove name substitutions in
            let seen = StringSet.add name seen in
            (seen, captured, substitutions, entry)
          ) in
          (* Substitute the bound type. *)
          let entry = match substitute_polytype substitutions (snd entry).bound_type with
          | Some t -> let (name, bound) = entry in (name, { bound with bound_type = t })
          | None -> entry
          in
          (* Add the entry to our reverse bounds list and return. *)
          let bounds_rev = entry :: bounds_rev in
          loop seen captured substitutions bounds bounds_rev
      )
    in
    let t = loop StringSet.empty StringSet.empty StringMap.empty (Nel.to_list bounds) [] in
    Some t

(* Merges two row list entries. Returns three lists.
 *
 * 1. A list of all the row entries which appear in both lists. (In reverse
 *    alphabetical order.)
 * 2. A list of the remaining entries of `entries1`. (In alphabetical order.)
 * 3. A list of the remaining entries of `entries2`. (In alphabetical order.) *)
let merge_rows entries1 entries2 =
  if entries1 = [] || entries2 = [] then ([], entries1, entries2) else
  let entries1 = List.stable_sort (fun (a, _) (b, _) -> String.compare a b) entries1 in
  let entries2 = List.stable_sort (fun (a, _) (b, _) -> String.compare a b) entries2 in
  let rec loop pairs rev_entries1 rev_entries2 entries1 entries2 =
    match entries1, entries2 with
    | [], _ -> (pairs, List.rev rev_entries1, List.rev_append rev_entries2 entries2)
    | _, [] -> (pairs, List.rev_append rev_entries1 entries1, List.rev rev_entries2)
    | ((label1, type1) as entry1) :: entries1', ((label2, type2) as entry2) :: entries2' ->
      if label1 = label2 then (
        loop ((label1, type1, type2) :: pairs) rev_entries1 rev_entries2 entries1' entries2'
      ) else if label1 < label2 then (
        loop pairs (entry1 :: rev_entries1) rev_entries2 entries1' entries2
      ) else (
        loop pairs rev_entries1 (entry2 :: rev_entries2) entries1 entries2'
      )
  in
  loop [] [] [] entries1 entries2
