(* Some diagnostic in our system. *)
type t =
  | Error of Error.error

(* Reported diagnostics. *)
let diagnostics: t list option ref = ref None

let get_diagnostics () =
  match !diagnostics with
  | None -> failwith "Expected a diagnostics context."
  | Some diagnostics -> diagnostics

(* Collects all the diagnostics reported during the execution of the provided
 * function in reverse order. *)
let collect f =
  let previous_diagnostics = !diagnostics in
  diagnostics := Some [];
  let result = f () in
  let all_diagnostics = get_diagnostics () in
  diagnostics := previous_diagnostics;
  (result, all_diagnostics)

(* Report a diagnostic. Panics if a diagnostics context has not yet been
 * established. *)
let report diagnostic =
  diagnostics := Some (diagnostic :: get_diagnostics ());
  diagnostic

(* Report an error diagnostic. Panics if a diagnostics context has not yet been
 * established. *)
let report_error error = report (Error error)
