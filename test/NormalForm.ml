open TestFramework

let () = suite "NormalForm" (fun () -> (

let cases = [
  ("x", "x");
  ("boolean", "boolean");
  ("number", "number");
  ("string", "string");
  ("⊥", "⊥");
  ("number → number", "number → number");
  ("string → number → boolean", "string → number → boolean");
  ("(string → number) → boolean", "(string → number) → boolean");
  ("string → (number → boolean)", "string → number → boolean");
  ("∀x.x", "⊥");
  ("∀x.x → x", "∀x.x → x");
  ("∀(a = number).a", "number");
  ("∀(a ≥ number).a", "number");
  ("∀(a = ∀x.x → x).a", "∀x.x → x");
  ("∀(a ≥ ∀x.x → x).a", "∀x.x → x");
  ("∀(a = number).a → a", "number → number");
  ("∀(a ≥ number).a → a", "number → number");
  ("∀a.number", "number");
  ("∀(a, b).number", "number");
  ("∀(a, b = number).a → b", "∀a.a → number");
  ("∀(a, b ≥ number).a → b", "∀a.a → number");
  ("∀(b = number, a).a → b", "∀a.a → number");
  ("∀(b ≥ number, a).a → b", "∀a.a → number");
  ("∀(a, b = number, c).a → b", "∀a.a → number");
  ("∀(a, b ≥ number, c).a → b", "∀a.a → number");
  ("∀(b = number, a, c).a → b", "∀a.a → number");
  ("∀(b ≥ number, a, c).a → b", "∀a.a → number");
  ("∀(c, a, b = number).a → b", "∀a.a → number");
  ("∀(c, a, b ≥ number).a → b", "∀a.a → number");
  ("∀(c, b = number, a).a → b", "∀a.a → number");
  ("∀(c, b ≥ number, a).a → b", "∀a.a → number");
  ("∀(a, c, b = number).a → b", "∀a.a → number");
  ("∀(a, c, b ≥ number).a → b", "∀a.a → number");
  ("∀(b = number, c, a).a → b", "∀a.a → number");
  ("∀(b ≥ number, c, a).a → b", "∀a.a → number");
  ("∀(a = ⊥).a", "⊥");
  ("∀(b, a = ⊥).a", "⊥");
  ("∀(a = ⊥, b).a", "⊥");
  ("∀(a ≥ ⊥).a", "⊥");
  ("∀(b, a ≥ ⊥).a", "⊥");
  ("∀(a ≥ ⊥, b).a", "⊥");
  ("∀(a = ∀(b, c).b → c).a", "∀(b, c).b → c");
  ("∀(a ≥ ∀(b, c).b → c).a", "∀(b, c).b → c");
  ("∀(a = ∀(b, c).b → c).a → a", "∀(a = ∀(b, c).b → c).a → a");
  ("∀(a ≥ ∀(b, c).b → c).a → a", "∀(a ≥ ∀(b, c).b → c).a → a");
  ("∀(a, d = ∀(b, c).a → b → c).d", "∀(a, b, c).a → b → c");
  ("∀(a, d = ∀(b, c).a → b → c, e).d", "∀(a, b, c).a → b → c");
  ("∀(a, d ≥ ∀(b, c).a → b → c).d", "∀(a, b, c).a → b → c");
  ("∀(a, d ≥ ∀(b, c).a → b → c, e).d", "∀(a, b, c).a → b → c");
  ("∀(a, b = a → a).b → b", "∀a.(a → a) → a → a");
  ("∀(a, b = a → a, a = number).b → b", "∀a.(a → a) → a → a");
  ("∀(a, b = a → a, a = number).b → b → a", "∀(a, a2 = number).(a → a) → a → a → a2");
  (* ("∀(a, b = a → a, a = number).b → b → a2", "");
  ("∀(a, b = a → a, a = number, a2 = string).b → b → a2", ""); *)
] in

cases |> List.iter (fun (input, output) -> (
  test (Printf.sprintf "nf(%s) = %s" input output) (fun () -> (
    let tokens = Parser.tokenize (Stream.of_string input) in
    let input = Parser.parse_polytype tokens in
    Stream.empty tokens;
    let input = Type.convert_polytype_ast input in
    let input = Printer.print_polytype input in
    assert_equal input output
  ))
))

))
