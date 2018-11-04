type expression = {
  description: expression_description;
}

and expression_description =
  (* `x` *)
  | Variable of { name: string }

  (* `true`, `false` *)
  | Boolean of bool

  (* `0`, `42`, `3.1415`, `-1` *)
  | Number of float

  (* `""`, `"foo"` *)
  | String of string

  (* `λx.E` *)
  | Function of { parameter: string; body: expression }

  (* `E1 E2` *)
  | Call of { callee: expression; argument: expression }

  (* `let x = E1 in E2` *)
  | Binding of { name: string; value: expression; body: expression }

  (* `(E: T)` *)
  | Annotation of { value: expression; type_: Type.polytype }

type t = expression

let variable name =
  { description = Variable { name } }

let boolean value =
  { description = Boolean value }

let number value =
  { description = Number value }

let string value =
  { description = String value }

let function_ parameter body =
  { description = Function { parameter; body } }

let call callee argument =
  { description = Call { callee; argument } }

let binding name value body =
  { description = Binding { name; value; body } }

let annotation value type_ =
  { description = Annotation { value; type_ } }
