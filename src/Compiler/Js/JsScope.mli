type t

val empty: t
val find: t -> string -> JsAst.identifier
val new_name: t -> string -> (t * JsAst.identifier)
val set_variable: t -> string -> JsAst.identifier -> t
val add: t -> string -> (t * JsAst.identifier)
