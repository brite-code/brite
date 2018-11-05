type error =
  (* A variable referenced some non-existent value. *)
  | UnboundVariable of { name: string }

  (* A type variable referenced some non-existent type bound. *)
  | UnboundTypeVariable of { name: string }

  (* We tried to unify two incompatible types. *)
  | IncompatibleTypes of { type1: Type.polytype; type2: Type.polytype }

  (* When trying to update type variable `name` with the polytype `type_` we
   * discovered that `type_` includes `name` somewhere within itself. Performing
   * the update would result in an infinite type which is not allowed! Instead
   * we produce this error. An example of this error firing is for the
   * auto-application lambda `λx.x x`. *)
  | InfiniteType of { name: string; type_: Type.polytype }

(* Some diagnostic in our system. *)
type t =
  | Error of error

(* Reported diagnostics. *)
let diagnostics: t list option ref = ref None

let get_diagnostics () =
  match !diagnostics with
  | None -> failwith "Expected a diagnostics context."
  | Some diagnostics -> diagnostics

(* Collects all the diagnostics reported during the execution of the
 * provided function. *)
let collect f =
  let previous_diagnostics = !diagnostics in
  diagnostics := Some [];
  let result = try f () with e -> (
    diagnostics := previous_diagnostics;
    raise e
  ) in
  let all_diagnostics = List.rev (get_diagnostics ()) in
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

(* Unwraps an error from a diagnostic. Panics if the diagnostic is not
 * an error. *)
let unwrap_error diagnostic =
  match diagnostic with
  | Error error -> error
