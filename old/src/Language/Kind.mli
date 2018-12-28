type t

val value: t
val row: t
val unknown: unit -> t
val unify: t -> t -> (unit, Diagnostics.t) result
