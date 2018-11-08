type t = {
  dedupe: int StringMap.t;
  variables: JsAst.identifier StringMap.t;
}

type name = JsAst.identifier

(* An empty JavaScript scope. *)
let empty =
  {
    dedupe = StringMap.empty;
    variables = StringMap.empty;
  }

(* Resolves the provided name in the scope and returns the referenced variable.
 * If no such variable exists then we panic! The checker should ensure that
 * variable references are well formed. *)
let find scope name =
  StringMap.find name scope.variables

(* All the [reserved words][1] of the ECMAScript specification.
 *
 * [1]: https://tc39.github.io/ecma262/#sec-reserved-words *)
let reserved_words = StringSet.of_list [
  "null"; "true"; "false"; "if"; "in"; "do"; "var"; "for"; "new"; "try"; "this"; "else"; "case"; "void"; "with"; "enum";
  "while"; "break"; "catch"; "throw"; "const"; "yield"; "class"; "super"; "return"; "typeof"; "delete"; "switch"; "export";
  "import"; "default"; "finally"; "extends"; "function"; "continue"; "debugger"; "instanceof"; "implements"; "interface";
  "package"; "private"; "protected"; "public"; "static"; "let";
]

(* Creates a new name in the scope that does not collide with any other names
 * in the scope. Returns the updated scope along with the new name. *)
let new_name scope name =
  let dedupe = match StringMap.find_opt name scope.dedupe with Some n -> n | None -> 0 in
  let dedupe = dedupe + 1 in
  let name' = if dedupe = 1 && not (StringSet.mem name reserved_words) then (
    name
  ) else (
    Printf.sprintf "%s$%i" name dedupe
  ) in
  let name' = JsAst.identifier name' in
  let scope = { scope with dedupe = StringMap.add name dedupe scope.dedupe } in
  (scope, name')

(* Sets the name to the provided identifier in this scope. Using this function
 * is unsafe! It is on you to make sure that you aren’t overriding any other
 * variables in scope. *)
let set_variable scope name identifier =
  { scope with variables = StringMap.add name identifier scope.variables }

(* Adds a name to the scope. You can access the new name with `JsScope.find`. If
 * the name already exists in the scope then we add a number to the name so that
 * they don’t conflict. Returns the updated scope along with the new name. *)
let add scope name =
  let (scope, name') = new_name scope name in
  let scope = set_variable scope name name' in
  (scope, name')
