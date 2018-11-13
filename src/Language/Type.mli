type 'k base_monotype = private {
  monotype_free_variables: StringSet.t Lazy.t;
  monotype_description: 'k monotype_description;
}

and 'k monotype_description = private
  | Variable of { name: string; kind: 'k }
  | Boolean
  | Number
  | Function of { parameter: 'k base_monotype; body: 'k base_monotype }
  | RowEmpty
  | RowExtension of { entries: (string * ('k base_monotype)) Nel.t; extension: 'k base_monotype }
  | Error of { kind: 'k; error: Diagnostics.t }

type bound_flexibility = Flexible | Rigid

type 'k base_polytype = private {
  polytype_normal: bool;
  polytype_free_variables: StringSet.t Lazy.t;
  polytype_description: 'k polytype_description;
}

and 'k polytype_description = private
  | Monotype of 'k base_monotype
  | Bottom of { kind: 'k }
  | Quantify of { bounds: (string * ('k base_bound)) Nel.t; body: 'k base_monotype }

and 'k base_bound = private {
  bound_flexibility: bound_flexibility;
  bound_type: 'k base_polytype;
}

type parse_monotype = unit base_monotype
type parse_polytype = unit base_polytype
type parse_bound = unit base_bound
type monotype = Kind.t base_monotype
type polytype = Kind.t base_polytype
type bound = Kind.t base_bound

val variable: string -> parse_monotype
val variable_with_kind: Kind.t -> string -> monotype
val boolean: 'k base_monotype
val number: 'k base_monotype
val function_: 'k base_monotype -> 'k base_monotype -> 'k base_monotype
val row_empty: 'k base_monotype
val row_extension: (string * ('k base_monotype)) Nel.t -> 'k base_monotype -> 'k base_monotype
val to_polytype: 'k base_monotype -> 'k base_polytype
val bottom: parse_polytype
val bottom_with_kind: Kind.t -> polytype
val bound: bound_flexibility -> 'k base_polytype -> 'k base_bound
val unbounded: parse_bound
val unbounded_value: bound
val quantify: (string * ('k base_bound)) Nel.t -> 'k base_monotype -> 'k base_polytype
val error: Kind.t -> Diagnostics.t -> monotype
val kind_monotype: monotype -> Kind.t
val kind: polytype -> Kind.t
val substitute_monotype: monotype StringMap.t -> monotype -> monotype option
val substitute_polytype: monotype StringMap.t -> polytype -> polytype option
val normal: polytype -> polytype option

val merge_rows:
  (string * monotype) list ->
  (string * monotype) list ->
  ((string * monotype * monotype) list * (string * monotype) list * (string * monotype) list)
