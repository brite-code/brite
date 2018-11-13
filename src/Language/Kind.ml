type t =
  (* Types that represent runtime values have this kind. Like numbers, booleans,
   * functions, or objects. *)
  | Value

  (* Row types have this kind. *)
  | Row

(* The value kind. *)
let value = Value

(* The row kind. *)
let row = Row
