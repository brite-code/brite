type monotype = private {
  monotype_free_variables: StringSet.t Lazy.t;
  monotype_description: monotype_description;
}

and monotype_description = private
  | Variable of { name: string }
  | Boolean
  | Number
  | String
  | Function of { parameter: monotype; body: monotype }

type bound_kind = Flexible | Rigid

type bound = private {
  bound_kind: bound_kind;
  bound_type: polytype;
}

and polytype = private {
  polytype_free_variables: StringSet.t Lazy.t;
  polytype_description: polytype_description;
}

and polytype_description = private
  | Monotype of monotype
  | Bottom
  | Quantify of { bounds: (string * bound) list; body: monotype }

val variable: string -> monotype
val boolean: monotype
val number: monotype
val string: monotype
val function_: monotype -> monotype -> monotype
val to_polytype: monotype -> polytype
val bottom: polytype
val bound: bound_kind -> polytype -> bound
val unbounded: bound
val quantify: (string * bound) list -> monotype -> polytype
val substitute_monotype: monotype StringMap.t -> monotype -> monotype option
val substitute_polytype: monotype StringMap.t -> polytype -> polytype option
