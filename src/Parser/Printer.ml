let rec print_monotype t =
  match t.Type.monotype_description with
  | Variable { name } -> name
  | Boolean -> "boolean"
  | Number -> "number"

  | Function { parameter = { monotype_description = Function _ } as parameter; body } ->
    Printf.sprintf "(%s) → %s" (print_monotype parameter) (print_monotype body)

  | Function { parameter; body } ->
    Printf.sprintf "%s → %s" (print_monotype parameter) (print_monotype body)

let rec print_polytype t =
  match t.Type.polytype_description with
  | Monotype t -> print_monotype t
  | Bottom -> "⊥"

  | Quantify { bounds = [(name, { bound_flexibility = Flexible; bound_type = { polytype_description = Bottom; _ } })]; body } ->
    let body = print_monotype body in
    Printf.sprintf "∀%s.%s" name body

  | Quantify { bounds; body } ->
    let body = print_monotype body in
    let bounds = List.map print_bound bounds in
    Printf.sprintf "∀(%s).%s" (String.concat ", " bounds) body

and print_bound (name, bound) =
  match bound with
  | { Type.bound_flexibility = Flexible; bound_type = { polytype_description = Bottom; _ } } -> name
  | { bound_flexibility; bound_type } ->
    let bound_flexibility = match bound_flexibility with Flexible -> "≥" | Rigid -> "=" in
    let bound_type = print_polytype bound_type in
    Printf.sprintf "%s %s %s" name bound_flexibility bound_type

let print_prefix prefix =
  match List.map print_bound (Prefix.bounds prefix) with
  | [] -> "(∅)"
  | bounds -> Printf.sprintf "(%s)" (String.concat ", " bounds)

let print_diagnostic diagnostic =
  match diagnostic with
  | Diagnostics.Error error -> (
    match error with
    | UnboundVariable { name } ->
      Printf.sprintf "Unbound variable `%s`." name

    | UnboundTypeVariable { name } ->
      Printf.sprintf "Unbound variable `%s`." name

    | IncompatibleTypes { type1; type2 } ->
      Printf.sprintf "%s ≢ %s" (print_polytype type1) (print_polytype type2)

    | InfiniteType { name; type_ } ->
      Printf.sprintf "Infinite type since `%s` occurs in `%s`." name (print_polytype type_)
  )
