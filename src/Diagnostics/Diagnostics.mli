type error =
  | UnboundVariable of { name: string }
  | UnboundTypeVariable of { name: string }
  | IncompatibleTypes of { type1: string; type2: string }
  | InfiniteType of { name: string; type_: string }

type t = private
  | Error of error

val collect: (unit -> 'a) -> ('a * t list)
val report_error: error -> t
