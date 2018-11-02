type monotype = {
  monotype_free_variables: StringSet.t Lazy.t;
  monotype_description: monotype_description;
}

and monotype_description =
  | Variable of { name: string }
  | Boolean
  | Number
  | String
  | Function of { parameter: monotype; body: monotype }

type bound_kind = Flexible | Rigid

type bound = {
  bound_kind: bound_kind;
  bound_type: polytype;
}

(* Polytypes are always in normal form. This means you will lose some fidelity
 * with source code when constructing polytypes. *)
and polytype = {
  polytype_free_variables: StringSet.t Lazy.t;
  polytype_description: polytype_description;
}

and polytype_description =
  | Monotype of monotype
  | Bottom
  | Quantify of { bounds: (string * bound) list; body: monotype }

(* Our type constructors always create types in normal form according to
 * Definition 1.5.5 of the [MLF thesis][1]. Practically this means only
 * quantified types need to be transformed into their normal form variants.
 *
 * These constructors are not suitable for representing parsed types. As we will
 * perform structural transformations on the types which changes the AST. For
 * example, monotype bounds are inlined. However, the user may be interested in
 * sharing a monotype in a bound since the type is long to write out by hand.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)

(* Creates a new variable monotype. *)
let variable name =
  {
    monotype_free_variables = lazy (StringSet.singleton name);
    monotype_description = Variable { name };
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

(* String monotype. *)
let string =
  {
    monotype_free_variables = lazy StringSet.empty;
    monotype_description = String;
  }

(* Creates a new function monotype. *)
let function_ parameter body =
  {
    monotype_free_variables = lazy (StringSet.union
      (Lazy.force parameter.monotype_free_variables) (Lazy.force body.monotype_free_variables));
    monotype_description = Function { parameter; body }
  }

(* Converts a monotype into a polytype. *)
let to_polytype =
  (* Allocate a constant boolean, number, and string so we don’t have to
   * allocate a new one each time. *)
  let boolean = { polytype_description = Monotype boolean; polytype_free_variables = boolean.monotype_free_variables } in
  let number = { polytype_description = Monotype number; polytype_free_variables = number.monotype_free_variables } in
  let string = { polytype_description = Monotype string; polytype_free_variables = string.monotype_free_variables } in
  fun t ->
    match t.monotype_description with
    | Boolean -> boolean
    | Number -> number
    | String -> string
    | _ ->
      {
        polytype_free_variables = t.monotype_free_variables;
        polytype_description = Monotype t;
      }

(* Bottom polytype. *)
let bottom =
  {
    polytype_free_variables = lazy StringSet.empty;
    polytype_description = Bottom;
  }

(* Creates a type bound. *)
let bound kind type_ = { bound_kind = kind; bound_type = type_ }

(* A flexible bottom bound. *)
let unbound = bound Flexible bottom

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
  | String
    -> None
  (* Look at the free variables for our type. If we don’t need a substitution
   * then just return the type. We put this here before
   * unconditionally recursing. *)
  | _ when not (needs_substitution substitutions (Lazy.force t.monotype_free_variables)) -> None
  (* Composite types are unconditionally substituted since according to the
   * check above their free type variables overlap with the type variables which
   * need to be substituted. *)
  | Function { parameter; body } ->
    let parameter = match substitute_monotype substitutions parameter with Some t -> t | None -> parameter in
    let body = match substitute_monotype substitutions body with Some t -> t | None -> body in
    Some (function_ parameter body)

(* Returns a string set with all the free variables in the provided bounds
 * list. If a bound does not appear in the free variable set then it will not
 * add its free variables to the set. Since in normal form the bound
 * would disappear. *)
let rec free_variables_of_bounds free bounds =
  match bounds with
  | [] -> free
  | (name, bound) :: bounds ->
    let free = free_variables_of_bounds free bounds in
    if StringSet.mem name free then (
      free
        |> StringSet.remove name
        |> StringSet.union (Lazy.force bound.bound_type.polytype_free_variables)
    ) else (
      free
    )

(* Substitutes the free variables of the provided type with a substitution if
 * one was made available in the substitutions map. Does not substitute
 * variables bound locally if they shadow a substitution. Returns nothing if no
 * substitution was made. *)
let rec substitute_polytype substitutions t =
  match t.polytype_description with
  (* Monotypes are substituted with a different function. *)
  | Monotype t -> (match substitute_monotype substitutions t with Some t -> Some (to_polytype t) | None -> None)
  (* No free type variables in the bottom type! *)
  | Bottom -> None
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
    )) (substitutions, []) bounds in
    let body = match substitute_monotype substitutions body with Some t -> t | None -> body in
    let bounds = List.rev bounds in
    (* We create our own polytype manually instead of using the constructor
     * because we know the resulting quantified type will be in normal form.
     * We only transform free variables into monotypes. Since monotypes are
     * always in normal form substituting a monotype for another monotype
     * maintains normal form. No quantifications will become unused because
     * we don’t substitute bound variables. Only free variables. *)
    Some {
      polytype_free_variables = lazy (free_variables_of_bounds (Lazy.force body.monotype_free_variables) bounds);
      polytype_description = Quantify { bounds; body };
    }

(* Quantifies a polytype with some bounds. Also converts the types and its
 * bounds into normal form. Which involves:
 *
 * 1. Inlining immediate variable bodies. e.g. `nf(∀(a ◇ o).a) = o`.
 * 2. Inlining monotype bounds. e.g. `nf(∀(a ◇ int).a → a) = int → int`.
 * 3. Dropping unused bounds. e.g. `nf(∀(a ◇ o).int) = int`.
 *
 * It is generally not suitable to construct parsed types with this function
 * since one loses the structure of the source code. *)
let quantify =
  (* Iterate through our bounds in order. Inlining and dropping bounds where
   * necessary. This function is not tail-recursive! *)
  let rec loop clear substitutions bounds body =
    match bounds with
    (* If we have no more new bounds then substitute our body and return the
     * components for our new polytype. *)
    | [] -> (
      let body = match substitute_polytype substitutions body with Some t -> t | None -> body in
      let free = Lazy.force body.polytype_free_variables in
      match body.polytype_description with
      (* If the bound is bottom then return nothing. Bottom has no bounds or
       * free variables. *)
      | Bottom -> None
      (* If the bound is a monotype then inline it into the body. *)
      | Monotype body -> Some ([], body, free)
      (* If the bound is a quantification then we assume the quantification is
       * already in normal form. We set our bounds list to the bounds of our
       * quantification. We will from now on append to the beginning of
       * this list. *)
      | Quantify { bounds; body } -> Some (bounds, body, free)
    )

    (* When we inline a monotype bound we add all of its type variables to a
     * “clear” set. This way those type variables can’t be shadowed and given a
     * different meaning. This case catches type variables trying to shadow a
     * name in the clear set. If we find such a name then we need to rename it. *)
    | (name, bound) :: bounds when StringSet.mem name clear ->
      let rec find n =
        let name' = name ^ string_of_int n in
        if StringSet.mem name' clear then find (n + 1) else name'
      in
      let name' = find 2 in
      let substitutions = StringMap.add name (variable name') substitutions in
      let clear = StringSet.add name' clear in
      loop clear substitutions ((name', bound) :: bounds) body

    (* If we have a monotype bound then we want to remove it from our bounds
     * list and inline the monotype. Make sure to apply substitutions to the
     * bound first! *)
    | (name, { bound_type = { polytype_description = Monotype t } }) :: bounds ->
      let t = match substitute_monotype substitutions t with Some t -> t | None -> t in
      let substitutions = StringMap.add name t substitutions in
      let clear = StringSet.union clear (Lazy.force t.monotype_free_variables) in
      loop clear substitutions bounds body

    (* Otherwise we need to handle this bound when moving in reverse order... *)
    | ((name, bound) as entry) :: bounds -> (
      (* Remove this name from the substitutions map in case we shadow
       * a substitution. Don’t override the original substitutions map though
       * since we lazily substitute the bound type. *)
      let substitutions' = StringMap.remove name substitutions in
      (* Recurse! Notably this is not a tail recursion. *)
      match loop clear substitutions' bounds body with
      (* Propagate none which is equivalent to bottom... *)
      | None -> None

      (* If this is the last bound _and_ the body is a variable which references
       * our bound then we want to inline the bound as our body. *)
      | Some ([], { monotype_description = Variable { name = name' } }, _) when name = name' ->
        loop clear substitutions [] bound.bound_type

      (* Otherwise we want to add our bound and its type variables to the bounds
       * list if it exists in the free type variables up to this point.
       * Otherwise we drop the bound. *)
      | Some (bounds, body, free) ->
        if StringSet.mem name free then (
          (* Substitute the bound and keep track of whether it was changed as
           * an optimization. *)
          let (changed, bound_type) = match substitute_polytype substitutions bound.bound_type with
          | None -> (false, bound.bound_type)
          | Some bound_type -> (true, bound_type)
          in
          (* Update our free type variables set. *)
          let free = free
            |> StringSet.remove name
            |> StringSet.union (Lazy.force bound_type.polytype_free_variables)
          in
          (* Use the old entry if nothing changed and use an updated bound if
           * the bound type did change. *)
          if not changed then (
            Some (entry :: bounds, body, free)
          ) else (
            Some ((name, { bound with bound_type }) :: bounds, body, free)
          )
        ) else (
          Some (bounds, body, free)
        )
    )
  in
  (* Execute our loop: if we returned none then we have a bottom type, if we did
   * not have any bounds then return the body type alone, or otherwise construct
   * the quantified polytype. *)
  fun bounds body ->
    if bounds = [] then body else
    match loop StringSet.empty StringMap.empty bounds body with
    | None -> bottom
    | Some ([], body, _) -> to_polytype body
    | Some (bounds, body, free) ->
      {
        polytype_free_variables = lazy free;
        polytype_description = Quantify { bounds; body };
      }
