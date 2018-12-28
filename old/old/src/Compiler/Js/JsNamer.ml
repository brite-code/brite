(* And Taborlin the Great said to the stone: "BREAK!" and the stone broke... *)

let blocks = ref []

(* Introduces a new block in which names may be generated for the provided
 * function. When the function returns the names used in this block will be free
 * for reuse. *)
let block f =
  blocks := Hashtbl.create 16 :: !blocks;
  let result = try f () with e -> blocks := List.tl !blocks; raise e in
  blocks := List.tl !blocks;
  result

(* All the [reserved words][1] of the ECMAScript specification.
 *
 * [1]: https://tc39.github.io/ecma262/#sec-reserved-words *)
let reserved_words = StringSet.of_list [
  "null"; "true"; "false"; "if"; "in"; "do"; "var"; "for"; "new"; "try"; "this"; "else"; "case"; "void"; "with"; "enum";
  "while"; "break"; "catch"; "throw"; "const"; "yield"; "class"; "super"; "return"; "typeof"; "delete"; "switch"; "export";
  "import"; "default"; "finally"; "extends"; "function"; "continue"; "debugger"; "instanceof"; "implements"; "interface";
  "package"; "private"; "protected"; "public"; "static"; "let";
]

(* Generate a unique name in the current block. Might shadow a name from an
 * earlier block. *)
let unique name =
  let block = List.hd !blocks in
  let dedupe = match Hashtbl.find_opt block name with
  | Some n -> n
  | None -> let n = ref 0 in Hashtbl.add block name n; n
  in
  dedupe := !dedupe + 1;
  let name = if !dedupe = 1 && not (StringSet.mem name reserved_words) then (
    name
  ) else (
    Printf.sprintf "%s$%i" name !dedupe
  ) in
  JsAst.identifier name
