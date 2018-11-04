type monotype = {
  monotype_free_variables: StringSet.t Lazy.t;
  monotype_description: monotype_description;
}

and monotype_description =
  (* `x` *)
  | Variable of { name: string }

  (* `boolean` *)
  | Boolean

  (* `number` *)
  | Number

  (* `string` *)
  | String

  (* `T1 → T2` *)
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
  (* Inherits from monotype. *)
  | Monotype of monotype

  (* `⊥` *)
  | Bottom

  (* `∀x.T`, `∀(x = T1).T2`, `∀(x ≥ T1).T2` *)
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
let unbounded = bound Flexible bottom

(* Quantifies a monotype by some bounds. The free type variables of quantified
 * types will not include the free type variables of unused bounds. This is to
 * be consistent with the normal form of the quantified type. *)
let quantify bounds body =
  if bounds = [] then to_polytype body else
  {
    polytype_free_variables = lazy (List.fold_right (fun (name, bound) free -> (
      if not (StringSet.mem name free) then free else
      free |> StringSet.remove name |> StringSet.union (Lazy.force bound.bound_type.polytype_free_variables)
    )) bounds (Lazy.force body.monotype_free_variables));
    polytype_description = Quantify { bounds; body };
  }

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
    Some (quantify (List.rev bounds) body)
