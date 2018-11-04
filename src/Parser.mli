type token

val tokenize: char Stream.t -> token Stream.t
val parse_monotype: token Stream.t -> Type.monotype
val parse_polytype: token Stream.t -> Type.polytype
