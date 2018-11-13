type monotype = private {
  monotype_free_variables: StringSet.t Lazy.t;
  monotype_description: monotype_description;
}

and monotype_description = private
  | Variable of { name: string }
  | Boolean
  | Number
  | Function of { parameter: monotype; body: monotype }
  | RowEmpty
  | RowExtension of { entries: row_entry Nel.t; extension: monotype }

and row_entry = string * monotype

type bound_flexibility = Flexible | Rigid

type bound = private {
  bound_flexibility: bound_flexibility;
  bound_type: polytype;
}

and polytype = private {
  polytype_normal: bool;
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
val function_: monotype -> monotype -> monotype
val row_empty: monotype
val row_extension: row_entry Nel.t -> monotype -> monotype
val to_polytype: monotype -> polytype
val bottom: polytype
val bound: bound_flexibility -> polytype -> bound
val unbounded: bound
val quantify: (string * bound) list -> monotype -> polytype
val substitute_monotype: monotype StringMap.t -> monotype -> monotype option
val substitute_polytype: monotype StringMap.t -> polytype -> polytype option
val normal: polytype -> polytype option
val merge_rows: row_entry list -> row_entry list -> ((string * monotype * monotype) list * row_entry list * row_entry list)
