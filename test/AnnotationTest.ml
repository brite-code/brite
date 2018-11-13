open TestFramework

let run () = suite "Annotation" (fun () -> (
  let cases = [
    ("x", "%error", ["Unbound variable `x`."]);
    ("⊥", "⊥", []);
    ("∀x.x", "∀x.x", []);
    ("∀(x = x).x", "∀(x = %error).x", ["Unbound variable `x`."]);
    ("∀(x ≥ x).x", "∀(x ≥ %error).x", ["Unbound variable `x`."]);
    ("∀x.x → x", "∀x.x → x", []);
    ("x → x", "%error → %error", ["Unbound variable `x`."; "Unbound variable `x`."]);
    ("(||)", "(||)", []);
    ("(||) → (||)", "%error → %error", ["Incompatible kinds row and value."; "Incompatible kinds row and value."]);
    ("(||) → number", "%error → number", ["Incompatible kinds row and value."]);
    ("number → (||)", "number → %error", ["Incompatible kinds row and value."]);
    ("(| a: number |)", "(| a: number |)", []);
    ("(| a: number | (||) |)", "(| a: number |)", []);
    ("(| a: number, b: number |)", "(| a: number, b: number |)", []);
    ("(| a: number | x |)", "(| a: number | %error |)", ["Unbound variable `x`."]);
    ("(| a: number | number |)", "(| a: number | %error |)", ["Incompatible kinds value and row."]);
    ("(| a: (||) |)", "(| a: %error |)", ["Incompatible kinds row and value."]);
    ("∀x.(| a: number | x |)", "∀x.(| a: number | x |)", []);
    ("∀x.x → number", "∀x.x → number", []);
    ("∀x.(| a: x | x |)", "∀x.(| a: x | %error |)", ["Incompatible kinds value and row."]);
  ] in

  cases |> List.iter (fun (input, output, expected_errors) -> (
    test input (fun () -> (
      let ((), actual_errors) = Diagnostics.collect (fun () -> (
        let tokens = Parser.tokenize (Stream.of_string input) in
        let type_ = Parser.parse_polytype tokens in
        Stream.empty tokens;
        let type_ = Annotation.check StringMap.empty (Kind.unknown ()) type_ in
        assert_equal (Printer.print_polytype type_) output
      )) in
      List.iter2 assert_equal (List.map Printer.print_diagnostic actual_errors) expected_errors
    ))
  ))
))
