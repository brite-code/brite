type t =
  (* Sometimes, we don’t yet know what our kind is. For instance, bottom types.
   * If we are processing `∀x.T` then we don’t know the kind of `x` until we
   * look at `T`. `x` could also be used in two inconsistent positions in `T`.
   * In that case we need to error. *)
  | Unknown of unknown

  (* Types that represent runtime values have this kind. Like numbers, booleans,
   * functions, or objects. *)
  | Value

  (* Row types have this kind. *)
  | Row

and unknown = {
  mutable seen: bool;
  mutable kind: t option;
}

(* The value kind. *)
let value = Value

(* The row kind. *)
let row = Row

(* An unknown kind. This kind will be solved through unification. *)
let unknown () = Unknown { seen = false; kind = None }

(* Unifies two kinds. Returns ok if successful and an error if not successful.
 * If one of the kinds is unknown then we solve for that kind. *)
let rec unify kind1 kind2 =
  match kind1, kind2 with
  (* Two referentially equal unknowns always unify together. *)
  | Unknown unknown1, Unknown unknown2
      when unknown1 == unknown2 ->
    Ok ()

  (* Unify an unknown kind. Check for cycles and resolved kinds. *)
  | Unknown unknown1, _ -> (
    if unknown1.seen then Error (Diagnostics.report_error InfiniteKind) else
    match unknown1.kind with
    | Some kind1 ->
      unknown1.seen <- true;
      let result = unify kind1 kind2 in
      unknown1.seen <- false;
      result
    | None ->
      unknown1.kind <- Some kind2;
      Ok ()
  )

  (* Unify an unknown kind. Check for cycles and resolved kinds. *)
  | _, Unknown unknown2 -> (
    if unknown2.seen then Error (Diagnostics.report_error InfiniteKind) else
    match unknown2.kind with
    | Some kind2 ->
      unknown2.seen <- true;
      let result = unify kind1 kind2 in
      unknown2.seen <- false;
      result
    | None ->
      unknown2.kind <- Some kind1;
      Ok ()
  )

  (* Constant kinds unify together. *)
  | Value, Value -> Ok ()
  | Row, Row -> Ok ()

  (* We have two incompatible kinds. *)
  | Value, Row -> Error (Diagnostics.report_error (IncompatibleKinds { kind1 = "value"; kind2 = "row" }))
  | Row, Value -> Error (Diagnostics.report_error (IncompatibleKinds { kind1 = "row"; kind2 = "value" }))
