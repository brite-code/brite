(* The abstract syntax tree represents the data structure we turn parsed
 * strings into. This abstract syntax tree will be converted into a presentation
 * that is easier to work with during type inference. *)

type monotype = {
  monotype_description: monotype_description;
}

and monotype_description =
  (* `x` *)
  | Variable of { name: string }

  (* `boolean` *)
  | Boolean

  (* `number` *)
  | Number

  (* `string` *)
  | String

  (* `T1 → T2` *)
  | Function of { parameter: monotype; body: monotype }

type bound_kind = Flexible | Rigid

type bound = {
  bound_kind: bound_kind;
  bound_type: polytype;
}

and polytype = {
  polytype_description: polytype_description;
}

and polytype_description =
  (* Inherits from monotype. *)
  | Monotype of monotype

  (* `⊥` *)
  | Bottom

  (* `∀x.T`, `∀(x = T1).T2`, `∀(x ≥ T1).T2` *)
  | Quantify of { bounds: (string * bound) list; body: monotype }

type expression = {
  expression_description: expression_description;
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
  | Annotation of { value: expression; type_: polytype }
