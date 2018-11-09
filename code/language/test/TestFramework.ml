let depth = ref 0
let successes = ref 0
let failures = ref 0

let success_mark = "\027[32m✔\027[39m"
let failure_mark = "\027[31m✘\027[39m"

let indentation () = String.make (!depth * 2) ' '

exception Assert_equal_failure of string * string

let assert_equal a b =
  if a <> b then raise (Assert_equal_failure (a, b))

let test name f =
  flush stdout;
  let result = try (
    f ();
    Ok ()
  ) with
  | Not_found -> Error "Not_found"
  | Failure reason -> Error (Printf.sprintf "Failure(%S)" reason)
  | Invalid_argument reason -> Error (Printf.sprintf "Invalid_argument(%S)" reason)
  | Assert_failure (file, line, col) -> Error (Printf.sprintf "Assert_failure(%S, %i, %i)" file line col)
  | Assert_equal_failure (a, b) -> Error (Printf.sprintf "%s \027[90m≠\027[39m %s" a b)
  | Stream.Error reason -> Error (Printf.sprintf "Stream.Error(%S)" reason)
  | Stream.Failure -> Error "Stream.Failure"
  in
  let (mark, failure_reason) = match result with
  | Ok () ->
    successes := !successes + 1;
    (success_mark, "")
  | Error reason ->
    failures := !failures + 1;
    (failure_mark, Printf.sprintf " \027[31m── error:\027[39m %s" reason)
  in
  Printf.printf "%s%s \027[90m%s\027[39m%s\n"
    (indentation ())
    mark
    name
    failure_reason

let suite name f =
  if !depth = 0 then Printf.printf "\n";
  Printf.printf "%s%s\n" (indentation ()) name;
  depth := !depth + 1;
  f ();
  depth := !depth - 1

let exit_tests () =
  Printf.printf
    "\nTests finished with %i success%s and %i failure%s.\n\n"
    !successes
    (if !successes = 1 then "" else "es")
    !failures
    (if !failures = 1 then "" else "s");
  exit !failures
