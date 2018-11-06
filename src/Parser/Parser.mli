type glyph =
  | Arrow
  | Bottom
  | Colon
  | Comma
  | Dot
  | EmptySet
  | Equals
  | ForAll
  | Lambda
  | LessThanOrEqual
  | ParenthesesLeft
  | ParenthesesRight
  | Let
  | In
  | True
  | False
  | Boolean
  | Number
  | String

type token =
  | Identifier of string
  | Number of float
  | Glyph of glyph

val tokenize: char Stream.t -> token Stream.t
val parse_monotype: token Stream.t -> Type.monotype
val parse_polytype: token Stream.t -> Type.polytype
val parse_prefix: token Stream.t -> (string * Type.bound) list
val parse_expression: token Stream.t -> Expression.t
val parse_context: token Stream.t -> (string * Type.polytype) list
