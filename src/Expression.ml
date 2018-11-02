type expression = {
  description: expression_description;
}

and expression_description =
  (* References some binding introduced by a lambda or let binding. *)
  | Variable of { name: string }

  (* A boolean constant value. *)
  | Boolean of bool

  (* A numeric constant value. In Brite, all numbers are floats
   * for now. *)
  | Number of float

  (* A string constant value. *)
  | String of string

  (* A function takes a parameter and uses it for some computation. In lambda
   * calculus terms this is an abstraction. *)
  | Function of { parameter: string; body: expression }

  (* Calls a function with an argument. In lambda calculus terms this is
   * an application. *)
  | Call of { callee: expression; argument: expression }

  (* Binds a value to the provided name in an expression. In lambda calculus
   * terms this is an abstraction over `body` followed be an application with
   * `value`. Represented as a separate term because it is treated differently
   * in the type systems. Just like in other Hindley-Milner languages. *)
  | Binding of { name: string; value: expression; body: expression }

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
