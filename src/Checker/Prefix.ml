type level = {
  index: int;
  names: (string, unit) Hashtbl.t;
}

type entry = {
  mutable level: level;
  mutable bound: Type.bound;
}

type t = {
  mutable counter: int;
  mutable levels: level list;
  entries: (string, entry) Hashtbl.t;
}

(* Creates a new prefix. *)
let create () =
  {
    counter = 1;
    levels = [];
    entries = Hashtbl.create 1000;
  }

(* Introduces a new level for the execution of the provided function. Cleans up
 * the level after the function finishes. Any type variables created in the
 * level will be removed from the prefix unless they were re-ordered in
 * an update. *)
let level prefix f =
  (* Add a new level to the prefix. *)
  let index = match prefix.levels with [] -> 0 | level :: _ -> level.index + 1 in
  prefix.levels <- { index; names = Hashtbl.create 16 } :: prefix.levels;
  let counter = prefix.counter in
  (* Execute our function with the new level. *)
  let result = try Ok (f ()) with e -> Error e in
  (* Remove the new level from the prefix. *)
  let level = List.hd prefix.levels in
  prefix.levels <- List.tl prefix.levels;
  prefix.counter <- counter;
  (* Delete all type variables at this level from the prefix. *)
  Hashtbl.iter (fun name () -> Hashtbl.remove prefix.entries name) level.names;
  (* If this was the last level then make sure there are no more entries in
   * the prefix. *)
  if prefix.levels = [] && not (Hashtbl.length prefix.entries = 0) then (
    failwith "Expected all type variables to be cleaned up."
  );
  (* Return the result from executing our function. *)
  match result with
  | Ok result -> result
  | Error error -> raise error

(* Private function for adding a type variable to the current level. If there is
 * no current level then we panic. *)
let add_to_level prefix name bound =
  (* Panic if we are not in a level or if a type variable with the same name
   * already exists in the prefix. *)
  if prefix.levels = [] then failwith "Type variable must be added to the prefix in a level.";
  if Hashtbl.mem prefix.entries name then failwith "Type variable with the same name already exists in the prefix.";
  (* Check to make sure that every free type variable has a corresponding entry
   * in the prefix. *)
  StringSet.iter
    (fun name -> ignore (Hashtbl.find prefix.entries name))
    (Lazy.force bound.Type.bound_type.polytype_free_variables);
  (* Add the type variable to the current level. *)
  let level = List.hd prefix.levels in
  Hashtbl.add level.names name ();
  (* Add the type variable to the prefix. *)
  Hashtbl.add prefix.entries name { level; bound }

(* IMPORTANT: It is expected that all free type variables are bound in the
 * prefix! If this is not true then we will panic.
 *
 * Creates a type variable with a unique name in the prefix and the provided
 * bound. Returns the new unique name. *)
let fresh_with_bound prefix bound =
  (* Directly return monotypes since they are always inlined in normal form. *)
  match bound.Type.bound_type.polytype_description with
  | Monotype bound_type -> bound_type
  | _ ->
    let rec unique () =
      let name = "t" ^ string_of_int prefix.counter in
      prefix.counter <- prefix.counter + 1;
      if Hashtbl.mem prefix.entries name then unique () else name
    in
    let name = unique () in
    add_to_level prefix name bound;
    Type.variable_with_kind name (Type.kind bound.bound_type)

(* Creates a type variable with a unique name in the prefix with a bottom bound.
 * Returns the new unique name. *)
let fresh prefix = fresh_with_bound prefix Type.unbounded

(* Adds a bound to the prefix with the provided name. If the name already exists
 * in the prefix then generate a new name so that all type variables in the
 * prefix are unique. Return the new name if we had to generate it. *)
let add prefix name bound =
  let name' = Namer.unique (Hashtbl.mem prefix.entries) name in
  add_to_level prefix name' bound;
  if name = name' then None else Some (Type.variable_with_kind name' (Type.kind bound.bound_type))

(* Finds the bound for the provided name in the prefix. If no type variable
 * could be found then we panic. The bound returned will always be in
 * normal form. *)
let lookup prefix name =
  let entry = Hashtbl.find prefix.entries name in
  if not entry.bound.bound_type.polytype_normal then (
    match Type.normal entry.bound.bound_type with
    | None -> ()
    | Some t -> entry.bound <- Type.bound entry.bound.bound_flexibility t
  );
  entry.bound

(* Merges the bounds of a quantified type into the prefix. If any of the bound
 * names already exist in the prefix then we need to generate new names. Those
 * names will be substituted in the bounds and the provided body type. The new
 * body type with substituted names will be returned. *)
let instantiate prefix bounds body =
  let substitutions = Nel.fold_left (fun substitutions (name, bound) -> (
    (* If we have some substitutions then apply them to our bound. *)
    let bound = match Type.substitute_polytype substitutions bound.Type.bound_type with
    | None -> bound
    | Some bound_type -> Type.bound bound.bound_flexibility bound_type
    in
    (* Add the bound to our prefix. If we had to generate a new name then add a
     * substitution to our map.
     *
     * It is worth considering how we handle the case
     * `∀a.∀(a = a).∀(a = a). ...`. In this type the second binding for `a`
     * will be generated a unique name and we will add a substitution for that
     * name. In the third binding for `a` we will also generate a unique name
     * and we will override the previous substitution. The issue we want to
     * avoid here is not updating the substitutions map for the third `a`. *)
    let new_type = add prefix name bound in
    let substitutions = match new_type with
    | Some new_type -> StringMap.add name new_type substitutions
    | None -> substitutions
    in
    substitutions
  )) StringMap.empty bounds in
  (* Substitute the body type and return if we have some substitutions. *)
  match Type.substitute_monotype substitutions body with Some t -> t | None -> body

(* Generalizes a type at the current level of the prefix. If the type references
 * variables in the prefix at the current level then we will quantify the
 * provided monotype with those type variables. If the type references variables
 * at another level then we will not quantify those. The returned type will
 * be in normal form. *)
let generalize prefix t =
  (* Get the current prefix level. Panic if we are not in a level. *)
  if prefix.levels = [] then failwith "May only generalize in a level.";
  let level = List.hd prefix.levels in
  (* Create a hash table for tracking the type variables we’ve visited. Also
   * create a list of the bounds we are going to quantify. *)
  let visited = Hashtbl.create 10 in
  let bounds = ref [] in
  (* Our visitor will look at each free type variable and possibly add it to our
   * bounds list and recurse. *)
  let rec visit name =
    (* If we have not yet visited this type variable... *)
    if not (Hashtbl.mem visited name) then (
      Hashtbl.add visited name ();
      (* If the type variable is on our current level... *)
      let entry = Hashtbl.find prefix.entries name in
      if entry.level.index >= level.index then (
        (* Recurse and visit all free variables in our bound because the bound
         * might have some dependencies on this level. *)
        StringSet.iter visit (Lazy.force entry.bound.bound_type.polytype_free_variables);
        (* Add our bound to the list we will use to quantify. It is important
         * we do this after recursing! Since the bound we add depends on the
         * bounds we might add while recursing. *)
        bounds := (name, entry.bound) :: !bounds
      )
    )
  in
  (* Visit all the free variables in this type. *)
  StringSet.iter visit (Lazy.force t.Type.monotype_free_variables);
  (* Quantify the type by the list of bounds we collected. Also convert the type
   * to normal form. *)
  let t = if !bounds = [] then Type.to_polytype t else Type.quantify (Nel.from_list (List.rev !bounds)) t in
  let t = match Type.normal t with Some t -> t | None -> t in
  t

exception Occurs

(* Checks to see if the provided name occurs anywhere in the type or in the
 * bounds of any free type variables recursively. If it does then we can’t
 * update the type variable at `name` with the provided type.
 *
 * NOTE: This function may get a bit hot. Could be a good function to make
 * incremental and improve performance. *)
let occurs prefix name t =
  (* Don’t allocate a hash table if there are no free variables in `t`. *)
  if StringSet.is_empty (Lazy.force t.Type.polytype_free_variables) then (
    false
  ) else (
    (* Cache for type variables we have seen before so we don’t need to check
     * them twice. Without this cache the occurs algorithm is worst case O(n²)
     * instead of O(n). Consider `∀(a, b = a → a).b → b`. Without caching the
     * fact that we’ve seen `b` before we might visit `a` four times. Two times
     * each time we visit `b`. *)
    let seen = Hashtbl.create 10 in
    let rec loop t =
      Lazy.force t.Type.polytype_free_variables |> StringSet.iter (fun name' -> (
        if String.equal name name' then (
          raise Occurs
        ) else if not (Hashtbl.mem seen name') then (
          Hashtbl.add seen name' ();
          let entry = Hashtbl.find prefix.entries name' in
          loop (entry.bound.Type.bound_type)
        )
      ))
    in
    (* Break out of our loop with an exception. *)
    try loop t; false with Occurs -> true
  )

(* “Re-orders” a type and its dependencies in the prefix. The prefix is ordered
 * by levels. All the type variable in a given level will be generalized
 * together. So if our new level comes before the level of our type’s
 * dependencies then we need to move our type’s dependencies to the new level.
 * Otherwise we’ll have dangling pointers when the type’s dependencies’ level
 * is removed from our prefix.
 *
 * Oh, and yes, pun intended. *)
let rec level_up prefix new_level t =
  Lazy.force t.Type.polytype_free_variables |> StringSet.iter (fun name -> (
    let entry = Hashtbl.find prefix.entries name in
    let old_level = entry.level in
    (* We know our old level will be removed before our new level if the index
     * is larger. *)
    if old_level.index > new_level.index then (
      (* Change our type variable’s level. *)
      entry.level <- new_level;
      Hashtbl.remove old_level.names name;
      Hashtbl.add new_level.names name ();
      (* Recurse because we might have dependencies in our bound that also need
       * to be updated. *)
      level_up prefix new_level (entry.bound.Type.bound_type)
    )
  ))

(* Performs the checks which must pass for an update to be safe. *)
let update_check prefix name old_bound new_bound =
  if (
    (* Run an “occurs” check to make sure that we aren’t creating an infinite
     * type with this update. If we would create an infinite type then return
     * an error. *)
    occurs prefix name new_bound.Type.bound_type
  ) then (
    Error (Diagnostics.report_error (InfiniteType { name; type_ = new_bound.bound_type }))
  ) else if (
    (* If the old bound is rigid then we check to make sure that the new type
     * is an abstraction of the old type. If it is not then we have an
     * invalid update!
     *
     * NOTE: We re-use the `IncompatibleTypes` error here, but maybe we want a
     * more descriptive error? *)
    old_bound.Type.bound_flexibility == Rigid &&
    not (Abstraction.check (lookup prefix) old_bound.bound_type new_bound.bound_type)
  ) then (
    let type1 = old_bound.bound_type in
    let type2 = new_bound.bound_type in
    Error (Diagnostics.report_error (IncompatibleTypes { type1; type2 }))
  ) else (
    (* The update is ok. You may proceed to commit changes... *)
    Ok ()
  )

(* IMPORTANT: We assume that `(Q) t1 ⊑ t2` holds. Do not call this function with
 * two types which do not uphold this relation!
 *
 * The update algorithm replaces the bound of a type variable in our prefix with
 * a new one. This operation only succeeds if the prefix after updating is
 * strictly an instance of the prefix before updating. In other words `Q ⊑ Q'`
 * must hold. We also must not add a circular dependency to our prefix as this
 * breaks the theoretical prefix ordering. So our prefix has these invariants:
 *
 * 1. If `name` does not already exist in the prefix we panic.
 * 2. If `bound` or any of the referenced type variables in `bound` contain a
 *    reference to `name` we return an error.
 * 3. If `Q ⊑ Q'` would not hold after applying this update we return an error.
 *
 * We assume that the new type is an instance of the old type. However, we do
 * check to ensure that the new type is an abstraction of the old type. *)
let update prefix name bound =
  let entry = Hashtbl.find prefix.entries name in
  (* Make sure our update is safe. *)
  match update_check prefix name entry.bound bound with
  (* If the update is not safe then return an error without
   * committing anything. *)
  | Error error -> Error error
  (* If the update is safe then update our entry and update the levels of our
   * new type’s dependencies. *)
  | Ok () ->
    entry.bound <- bound;
    level_up prefix entry.level bound.bound_type;
    Ok ()

(* IMPORTANT: We assume that `(Q) t1 ⊑ t3` and `(Q) t2 ⊑ t3` hold. Do not call
 * this function with types which do not uphold this relation!
 *
 * Updates two types to the same bound. Corresponds to the merge algorithm in
 * the [MLF Thesis][1] along with two calls to update. It is important we do
 * these two updates together in a transaction so a single error means we don’t
 * commit anything. *)
let update2 prefix name1 name2 bound =
  let entry1 = Hashtbl.find prefix.entries name1 in
  let entry2 = Hashtbl.find prefix.entries name2 in
  (* Make sure both our updates are safe. *)
  match (
    update_check prefix name1 entry1.bound bound,
    update_check prefix name2 entry2.bound bound
  ) with
  (* If the update is not safe then return an error without
   * committing anything. *)
  | Error error, _ -> Error error
  | _, Error error -> Error error
  (* If the update is safe then update our entry and update the levels of our
   * new type’s dependencies. *)
  | Ok (), Ok () ->
    (* Update one of the entries to equal the provided bound. Then, link the
     * other entry to updated entry. We let the entry which will live longer
     * be the one to hold our bound. We let the entry which will live shorter
     * be our link. *)
    if entry1.level.index <= entry2.level.index then (
      entry1.bound <- bound;
      entry2.bound <- Type.bound Type.Rigid (Type.to_polytype (Type.variable_with_kind name1 (Type.kind bound.bound_type)));
      level_up prefix entry1.level bound.bound_type;
      Ok ()
    ) else (
      entry2.bound <- bound;
      entry1.bound <- Type.bound Type.Rigid (Type.to_polytype (Type.variable_with_kind name2 (Type.kind bound.bound_type)));
      level_up prefix entry2.level bound.bound_type;
      Ok ()
    )

(* Collects all the current bounds of the prefix into the list. The bounds are
 * sorted in dependency order. That is, dependents are listed after
 * their dependencies. *)
let bounds prefix =
  let visited: (string, unit) Hashtbl.t = Hashtbl.create (Hashtbl.length prefix.entries) in
  let rec visit (bounds: (string * Type.bound) list) name =
    if Hashtbl.mem visited name then (
      bounds
    ) else (
      Hashtbl.add visited name ();
      let { bound; _ } = Hashtbl.find prefix.entries name in
      let bounds = StringSet.fold
        (fun name bounds -> visit bounds name)
        (Lazy.force bound.bound_type.polytype_free_variables)
        bounds
      in
      let bounds = (name, bound) :: bounds in
      bounds
    )
  in
  let bounds = Hashtbl.fold (fun name _ bounds -> visit bounds name) prefix.entries [] in
  List.rev bounds
