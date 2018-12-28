type glyph =
  | Arrow
  | Bar
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
  | ParenthesesBarLeft
  | ParenthesesBarRight

  (* Keywords *)
  | Boolean
  | Number
  | Let
  | In
  | True
  | False
  | If
  | Then
  | Else

type token =
  | Identifier of string
  | Number of float
  | Glyph of glyph

val tokenize: char Stream.t -> token Stream.t
val parse_monotype: token Stream.t -> Type.parse_monotype
val parse_polytype: token Stream.t -> Type.parse_polytype
val parse_prefix: token Stream.t -> (string * Type.parse_bound) list
val parse_expression: token Stream.t -> Expression.t
val parse_context: token Stream.t -> (string * Type.parse_polytype) list
