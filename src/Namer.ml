(* And Taborlin the Great said to the stone: "BREAK!" and the stone broke... *)

(* Gets the integer suffix of the provided name string. For instance, for `x` we
 * return nothing, but for `x42` we return the integer 42. Also returns the
 * string before the suffix *)
let int_suffix name =
  let rec loop acc i =
    match String.get name i with
    | '0' .. '9' as c ->
      let acc = c :: acc in
      if i = 0 then ("", acc) else loop acc (i - 1)

    | _ -> (String.sub name 0 (i + 1), acc)
  in
  let length = String.length name in
  let (prefix, digits) = loop [] (length - 1) in
  if digits = [] then (
    None
  ) else (
    let digits = Array.of_list digits in
    let digits = String.init (Array.length digits) (Array.get digits) in
    Some (prefix, int_of_string digits)
  )

(* Creates a unique name based on the provided name. If the name is already
 * unique we return it. If the name is not unique we increments an integer
 * suffix count on the name to create new names. For instance, `x` becomes
 * `x2`, `x2` becomes `x3`, `x3` becomes `x4` and so on. Uses the provided
 * function to check if a name already exists. *)
let unique exists name =
  if not (exists name) then (
    name
  ) else (
    let (i, name) = match int_suffix name with
    | None -> (2, name)
    | Some (prefix, int) -> (int + 1, prefix)
    in
    let rec loop i =
      let name = name ^ string_of_int i in
      if exists name then loop (i + 1) else name
    in
    loop i
  )
