type error =
  | UnboundVariable of { name: string }
  | UnboundTypeVariable of { name: string }
  | IncompatibleTypes of { type1: Type.polytype; type2: Type.polytype }
  | InfiniteType of { name: string; type_: Type.polytype }

type t = private
  | Error of error

val collect: (unit -> 'a) -> ('a * t list)
val report_error: error -> t
