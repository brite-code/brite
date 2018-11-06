open TestFramework

let run () = suite "Infer" (fun () -> (
  let cases = [
    ("infer((∅), (x: boolean), x)", "((∅), boolean)", []);
    ("infer((∅), (x: number), x)", "((∅), number)", []);
    ("infer((∅), (x: string), x)", "((∅), string)", []);
    ("infer((∅), (x: ∀a.a → a), x)", "((∅), ∀a.a → a)", []);
    ("infer((∅), (x: ∀a.a), x)", "((∅), ∀a.a)", []);
    ("infer((∅), (x: ∀a.number), x)", "((∅), ∀a.number)", []);
    ("infer((∅), (∅), λx.x)", "((∅), ∀t1.t1 → t1)", []);
    ("infer((∅), (add1: number → number), add1 42)", "((∅), number)", []);
    ("infer((∅), (add1: number → number), add1 true)", "((∅), number)", ["number ≢ boolean"]);
    ("infer((a), (add1: number → number, x: a), add1 x)", "((a = number), number)", []);
    ("infer((∅), (∅), let id = λx.x in λx.x)", "((∅), ∀t1.t1 → t1)", []);
    ("infer((∅), (add1: number → number), λx.add1 x)", "((∅), number → number)", []);
    ("infer((∅), (∅), λx.42)", "((∅), ∀t1.t1 → number)", []);
    ("infer((∅), (∅), λz.λx.x)", "((∅), ∀(t1, t2 ≥ ∀t2.t2 → t2).t1 → t2)", []);
    ("infer((∅), (id: ∀a.a → a), id 42)", "((∅), number)", []);
    ("infer((∅), (id: ∀a.a → a), id id)", "((∅), ∀a.a → a)", []);
    ("infer((∅), (id: ∀a.a → a), let x = id 42 in id)", "((∅), ∀a.a → a)", []);
    ("infer((a), (x: a), x x)", "((a), ⊥)", ["Infinite type since `a` occurs in `a → t1`."]);
    ("infer((a), (x: a), let x = (x: ∀a.a → a) in x x)", "((a = ∀a.a → a), ∀a.a → a)", []);
    ("infer((∅), (∅), λx.x x)", "((∅), ∀(t1, t2).t1 → t2)", ["Infinite type since `t1` occurs in `t1 → t2`."]);
    ("infer((∅), (∅), λx.let x = (x: ∀a.a → a) in x x)", "((∅), ∀(t1 = ∀a.a → a, t2 ≥ ∀a.a → a).t1 → t2)", []);
    ("infer((∅), (∅), (λx.let x = (x: ∀a.a → a) in x x: ∀(a = ∀a.a → a).a → a))", "((∅), ∀(a = ∀a.a → a).a → a)", []);
    ("infer((∅), (add1: number → number), (λx.let x = (x: ∀a.a → a) in x x) add1)", "((∅), ∀a.a → a)", ["∀a.a → a ≢ number → number"]);
    ("infer((∅), (add1: number → number), (λx.let x = (x: ∀a.a → a) in x x) 42)", "((∅), ∀a.a → a)", ["a → a ≢ number"]);
    ("infer((∅), (∅), (λx.let x = (x: ∀a.a → a) in x x) (λx.x))", "((∅), ∀a.a → a)", []);
    ("infer((∅), (∅), (λx.let x = (x: ∀a.a → a) in x x) (λx.x) 42)", "((∅), number)", []);
  ] in

  let prefix = Prefix.create () in

  cases |> List.iter (fun (input, output, expected_errors) -> (
    let name = match List.length expected_errors with
    | 0 -> Printf.sprintf "%s = %s" input output
    | 1 -> Printf.sprintf "%s = %s with 1 error" input output
    | n -> Printf.sprintf "%s = %s with %n errors" input output n
    in
    test name (fun () -> (
      let ((), actual_errors) = Diagnostic.collect (fun () -> Prefix.level prefix (fun () -> (
        let tokens = Parser.tokenize (Stream.of_string input) in
        assert (Stream.next tokens = Identifier "infer");
        assert (Stream.next tokens = Glyph ParenthesesLeft);
        let bounds = Parser.parse_prefix tokens in
        List.iter (fun (name, bound) -> assert (Prefix.add prefix name bound = None)) bounds;
        assert (Stream.next tokens = Glyph Comma);
        let entries = Parser.parse_context tokens in
        let context = List.fold_left (fun context (name, type_) ->
          StringMap.add name type_ context
        ) StringMap.empty entries in
        assert (Stream.next tokens = Glyph Comma);
        let expression = Parser.parse_expression tokens in
        assert (Stream.next tokens = Glyph ParenthesesRight);
        Stream.empty tokens;
        let type_ = Infer.infer prefix context expression in
        let actual_output = Printf.sprintf "(%s, %s)" (Printer.print_prefix prefix) (Printer.print_polytype type_) in
        assert_equal actual_output output
      ))) in
      List.iter2 assert_equal (List.map Printer.print_diagnostic actual_errors) expected_errors
    ))
  ))
))
