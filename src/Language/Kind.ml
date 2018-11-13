type t =
  (* A type with the bottom kind has no kind at all. This is the kind assigned
   * to bottom types. *)
  | Bottom

  (* Types that represent runtime values have this kind. Like numbers, booleans,
   * functions, or objects. *)
  | Value

  (* Row types have this kind. *)
  | Row

(* The bottom kind. *)
let bottom = Bottom

(* The value kind. *)
let value = Value

(* The row kind. *)
let row = Row
