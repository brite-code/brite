let rec print_monotype t =
  match t.Type.monotype_description with
  | Variable { name } -> name
  | Boolean -> "boolean"
  | Number -> "number"
  | String -> "string"

  | Function { parameter = { monotype_description = Function _ } as parameter; body } ->
    Printf.sprintf "(%s) → %s" (print_monotype parameter) (print_monotype body)

  | Function { parameter; body } ->
    Printf.sprintf "%s → %s" (print_monotype parameter) (print_monotype body)

let rec print_polytype t =
  match t.Type.polytype_description with
  | Monotype t -> print_monotype t
  | Bottom -> "⊥"

  | Quantify { bounds = [(name, { bound_kind = Flexible; bound_type = { polytype_description = Bottom; _ } })]; body } ->
    let body = print_monotype body in
    Printf.sprintf "∀%s.%s" name body

  | Quantify { bounds; body } ->
    let body = print_monotype body in
    let bounds = List.map (fun ((name, bound): (string * Type.bound)) ->
      match bound with
      | { bound_kind = Flexible; bound_type = { polytype_description = Bottom; _ } } -> name
      | { bound_kind; bound_type } ->
        let bound_kind = match bound_kind with Flexible -> "≥" | Rigid -> "=" in
        let bound_type = print_polytype bound_type in
        Printf.sprintf "%s %s %s" name bound_kind bound_type
    ) bounds in
    Printf.sprintf "∀(%s).%s" (String.concat ", " bounds) body
