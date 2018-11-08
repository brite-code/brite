type constant = private
  | Boolean of bool
  | Number of float

type expression = private {
  description: expression_description;
}

and expression_description = private
  | Variable of { name: string }
  | Constant of constant
  | Function of { parameter: string; body: expression }
  | Call of { callee: expression; argument: expression }
  | Binding of { name: string; value: expression; body: expression }
  | Annotation of { value: expression; type_: Type.polytype }
  | Conditional of { test: expression; consequent: expression; alternate: expression }

type t = expression

val variable: string -> expression
val boolean: bool -> expression
val number: float -> expression
val function_: string -> expression -> expression
val call: expression -> expression -> expression
val binding: string -> expression -> expression -> expression
val annotation: expression -> Type.polytype -> expression
val conditional: expression -> expression -> expression -> expression
