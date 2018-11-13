type t

val create: unit -> t
val level: t -> (unit -> 'a) -> 'a
val fresh_with_bound: t -> Type.bound -> Type.monotype
val fresh: t -> Type.monotype
val add: t -> string -> Type.bound -> Type.monotype option
val lookup: t -> string -> Type.bound
val instantiate: t -> (string * Type.bound) Nel.t -> Type.monotype -> Type.monotype
val generalize: t -> Type.monotype -> Type.polytype
val update: t -> string -> Type.bound -> (unit, Diagnostics.t) result
val update2: t -> string -> string -> Type.bound -> (unit, Diagnostics.t) result
val bounds: t -> (string * Type.bound) list
