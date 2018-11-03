val convert_monotype: Ast.monotype -> Type.monotype
val convert_polytype: Ast.polytype -> Type.polytype
val infer: Prefix.t -> Type.polytype StringMap.t -> Ast.expression -> Type.polytype
