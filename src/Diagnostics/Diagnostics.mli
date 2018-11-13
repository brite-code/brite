type error =
  | UnboundVariable of { name: string }
  | UnboundTypeVariable of { name: string }
  | IncompatibleTypes of { type1: string; type2: string }
  | InfiniteType of { name: string; type_: string }
  | IncompatibleKinds of { kind1: string; kind2: string }
  | InfiniteKind

type t = private
  | Error of error

val collect: (unit -> 'a) -> ('a * t list)
val report_error: error -> t
val equal: t -> t -> bool
