open TestFramework

let () = suite "Unify" (fun () -> (
  let cases = [
    ("unify((∅), boolean, boolean)", "(∅)", []);
    ("unify((∅), number, number)", "(∅)", []);
    ("unify((∅), string, string)", "(∅)", []);
    ("unify((a), number, number)", "(a)", []);
    ("unify((a, b), number, number)", "(a, b)", []);
    ("unify((a, b, c), number, number)", "(a, b, c)", []);
    ("unify((∅), number → number, number → number)", "(∅)", []);
    ("unify((∅), number → string, number → string)", "(∅)", []);
    ("unify((∅), number → string → boolean, number → string → boolean)", "(∅)", []);
    ("unify((∅), number → number, number → string)", "(∅)", ["number ≢ string"]);
    ("unify((∅), number → number, string → number)", "(∅)", ["string ≢ number"]);
    ("unify((∅), number → number, string → string)", "(∅)", ["string ≢ number"; "number ≢ string"]);
    ("unify((∅), number → number → number, string → string → string)", "(∅)", [
      "string ≢ number";
      "string ≢ number";
      "number ≢ string";
    ]);
    ("unify((∅), (number → number) → number, (string → string) → string)", "(∅)", [
      "number ≢ string";
      "string ≢ number";
      "number ≢ string";
    ]);
    ("unify((∅), number, number → number)", "(∅)", ["number ≢ number → number"]);
    ("unify((∅), number → number, number)", "(∅)", ["number → number ≢ number"]);
    ("unify((a = number), a, number)", "(a = number)", []);
    ("unify((a = number), number, a)", "(a = number)", []);
    ("unify((a = number), a, string)", "(a = number)", ["number ≢ string"]);
    ("unify((a = number), string, a)", "(a = number)", ["string ≢ number"]);
    ("unify((a ≥ number), a, number)", "(a ≥ number)", []);
    ("unify((a ≥ number), number, a)", "(a ≥ number)", []);
    ("unify((a ≥ number), a, string)", "(a ≥ number)", ["number ≢ string"]);
    ("unify((a ≥ number), string, a)", "(a ≥ number)", ["string ≢ number"]);
    ("unify((b = number, a = b), a, number)", "(b = number, a = b)", []);
    ("unify((b = number, a = b), number, a)", "(b = number, a = b)", []);
    ("unify((b = number, a = b), a, string)", "(b = number, a = b)", ["number ≢ string"]);
    ("unify((b = number, a = b), string, a)", "(b = number, a = b)", ["string ≢ number"]);
    ("unify((b = number, a ≥ b), a, number)", "(b = number, a ≥ b)", []);
    ("unify((b = number, a ≥ b), number, a)", "(b = number, a ≥ b)", []);
    ("unify((b = number, a ≥ b), a, string)", "(b = number, a ≥ b)", ["number ≢ string"]);
    ("unify((b = number, a ≥ b), string, a)", "(b = number, a ≥ b)", ["string ≢ number"]);
    ("unify((b ≥ number, a = b), a, number)", "(b ≥ number, a = b)", []);
    ("unify((b ≥ number, a = b), number, a)", "(b ≥ number, a = b)", []);
    ("unify((b ≥ number, a = b), a, string)", "(b ≥ number, a = b)", ["number ≢ string"]);
    ("unify((b ≥ number, a = b), string, a)", "(b ≥ number, a = b)", ["string ≢ number"]);
    ("unify((b ≥ number, a ≥ b), a, number)", "(b ≥ number, a ≥ b)", []);
    ("unify((b ≥ number, a ≥ b), number, a)", "(b ≥ number, a ≥ b)", []);
    ("unify((b ≥ number, a ≥ b), a, string)", "(b ≥ number, a ≥ b)", ["number ≢ string"]);
    ("unify((b ≥ number, a ≥ b), string, a)", "(b ≥ number, a ≥ b)", ["string ≢ number"]);
    ("unify((a), a, a)", "(a)", []);
    ("unify((a ≥ ⊥), a, a)", "(a)", []);
    ("unify((a = ⊥), a, a)", "(a = ⊥)", []);
    ("unify((a, b = a, c = b), b, c)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), c, b)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), a, b)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), a, c)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), b, a)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), c, a)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), b, a)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), b, b)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), c, c)", "(a, b = a, c = b)", []);
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), b, c)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), c, b)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), a, b)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), a, c)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), b, a)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), c, a)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), b, a)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), b, b)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), c, c)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    ("unify((a), a, number)", "(a = number)", []);
    ("unify((a), number, a)", "(a = number)", []);
    ("unify((a ≥ ⊥), a, number)", "(a = number)", []);
    ("unify((a ≥ ⊥), number, a)", "(a = number)", []);
    ("unify((a = ⊥), a, number)", "(a = ⊥)", ["number ≢ ⊥"]);
    ("unify((a = ⊥), number, a)", "(a = ⊥)", ["number ≢ ⊥"]);
    ("unify((b ≥ ⊥, a ≥ b), a, number)", "(b = number, a ≥ b)", []);
    ("unify((b ≥ ⊥, a ≥ b), number, a)", "(b = number, a ≥ b)", []);
    ("unify((b ≥ ⊥, a = b), a, number)", "(b = number, a = b)", []);
    ("unify((b ≥ ⊥, a = b), number, a)", "(b = number, a = b)", []);
    ("unify((b = ⊥, a ≥ b), a, number)", "(b = ⊥, a ≥ b)", ["number ≢ ⊥"]);
    ("unify((b = ⊥, a ≥ b), number, a)", "(b = ⊥, a ≥ b)", ["number ≢ ⊥"]);
    ("unify((b = ⊥, a = b), a, number)", "(b = ⊥, a = b)", ["number ≢ ⊥"]);
    ("unify((b = ⊥, a = b), number, a)", "(b = ⊥, a = b)", ["number ≢ ⊥"]);
    ("unify((b, a = b → b), a, b)", "(b, a = b → b)", ["Infinite type since `b` occurs in `b → b`."]);
    ("unify((b, a = b → b), b, a)", "(b, a = b → b)", ["Infinite type since `b` occurs in `b → b`."]);
    ("unify((b, c = b → b, a = c), a, b)", "(b, c = b → b, a = c)", ["Infinite type since `b` occurs in `b → b`."]);
    ("unify((b, c = b → b, a = c), b, a)", "(b, c = b → b, a = c)", ["Infinite type since `b` occurs in `b → b`."]);
    ("unify((b, c = b → b, a = c → c), a, b)", "(b, c = b → b, a = c → c)", ["Infinite type since `b` occurs in `c → c`."]);
    ("unify((b, c = b → b, a = c → c), b, a)", "(b, c = b → b, a = c → c)", ["Infinite type since `b` occurs in `c → c`."]);
    ("unify((a, b), a, b)", "(a, b = a)", []);
    ("unify((a, b), b, a)", "(b, a = b)", []);
    ("unify((a, b), a, b → b)", "(b, a = b → b)", []);
    ("unify((a, b), b → b, a)", "(b, a = b → b)", []);
    ("unify((a ≥ ∀x.x → x), a, number → number)", "(a = number → number)", []);
    ("unify((a ≥ ∀x.x → x), a, number → string)", "(a ≥ ∀x.x → x)", ["number ≢ string"]);
    ("unify((a = ∀x.x → x), a, number → number)", "(a = ∀x.x → x)", ["number → number ≢ ∀x.x → x"]);
    ("unify((a = ∀x.x → x), a, number → string)", "(a = ∀x.x → x)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀y.y → y), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀y.y → y), b, a)", "(b ≥ ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀(y, z).y → z), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀(y, z).y → z), b, a)", "(b ≥ ∀(z, x = z, y = x).y → z, a = b)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀(y, z).y → z), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀(y, z).y → z), b, a)", "(b ≥ ∀(z, x = z, y = x).y → z, a = b)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀(y, z).y → z), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀(y, z).y → z), b, a)", "(b = ∀(z, x = z, y = x).y → z, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀(y, z).y → z), a, b)", "(a ≥ ∀x.x → x, b = ∀(y, z).y → z)", [
      "∀(y, x = y).x → x ≢ ∀(y, z).y → z";
    ]);
    ("unify((a ≥ ∀x.x → x, b = ∀(y, z).y → z), b, a)", "(a ≥ ∀x.x → x, b = ∀(y, z).y → z)", [
      "∀(z, x = z, y = x).y → z ≢ ∀(y, z).y → z";
    ]);
    ("unify((a = ∀x.x → x, b = ∀(y, z).y → z), a, b)", "(a = ∀x.x → x, b = ∀(y, z).y → z)", [
      "∀(y, x = y).x → x ≢ ∀(y, z).y → z";
    ]);
    ("unify((a = ∀x.x → x, b = ∀(y, z).y → z), b, a)", "(a = ∀x.x → x, b = ∀(y, z).y → z)", [
      "∀(z, x = z, y = x).y → z ≢ ∀(y, z).y → z";
    ]);
    ("unify((a ≥ ∀x.x → x, b = number → number), a, b)", "(a = number → number, b = number → number)", []);
    ("unify((a ≥ ∀x.x → x, b = number → number), b, a)", "(a = number → number, b = number → number)", []);
    ("unify((a, b = a → a), a, b)", "(a, b = a → a)", ["Infinite type since `a` occurs in `a → a`."]);
    ("unify((a, b = a → a), b, a)", "(a, b = a → a)", ["Infinite type since `a` occurs in `a → a`."]);
    ("unify((a, b = ∀c.c → a), a, b)", "(a, b = ∀c.c → a)", ["Infinite type since `a` occurs in `∀c.c → a`."]);
    ("unify((a, b = ∀c.c → a), b, a)", "(a, b = ∀c.c → a)", ["Infinite type since `a` occurs in `∀c.c → a`."]);
    ("unify((a = ⊥, b = ⊥), a, b)", "(a = ⊥, b = a)", []);
    ("unify((a = ⊥, b = ⊥), b, a)", "(b = ⊥, a = b)", []);
    ("unify((a = ⊥, b = ∀c.c → c), a, b)", "(a = ⊥, b = ∀c.c → c)", ["∀c.c → c ≢ ⊥"]);
    ("unify((a = ⊥, b = ∀c.c → c), b, a)", "(a = ⊥, b = ∀c.c → c)", ["∀c.c → c ≢ ⊥"]);
    ("unify((a, b), a, b)", "(a, b = a)", []);
    ("unify((a, b), b, a)", "(b, a = b)", []);
    ("unify((a ≥ ⊥, b ≥ ⊥), a, b)", "(a, b = a)", []);
    ("unify((a ≥ ⊥, b ≥ ⊥), b, a)", "(b, a = b)", []);
    ("unify((a = ⊥, b ≥ ⊥), a, b)", "(a = ⊥, b = a)", []);
    ("unify((a = ⊥, b ≥ ⊥), b, a)", "(b = ⊥, a = b)", []);
    ("unify((a ≥ ⊥, b = ⊥), a, b)", "(a = ⊥, b = a)", []);
    ("unify((a ≥ ⊥, b = ⊥), b, a)", "(b = ⊥, a = b)", []);
    ("unify((a = ⊥, b = ⊥), a, b)", "(a = ⊥, b = a)", []);
    ("unify((a = ⊥, b = ⊥), b, a)", "(b = ⊥, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀y.y → y), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀y.y → y), b, a)", "(b ≥ ∀(x, y = x).y → y, a = b)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    (* TODO: ("unify((a ≥ ∀x.x → x, b ≥ ∀x.x → x), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []); *)
    (* TODO: ("unify((a ≥ ∀x.x → x, b ≥ ∀x.x → x), b, a)", "(b ≥ ∀(x, y = x).y → y, a = b)", []); *)
    (* TODO: ("unify((a = ∀x.x → x, b ≥ ∀x.x → x), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []); *)
    (* TODO: ("unify((a = ∀x.x → x, b ≥ ∀x.x → x), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []); *)
    (* TODO: ("unify((a ≥ ∀x.x → x, b = ∀x.x → x), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []); *)
    (* TODO: ("unify((a ≥ ∀x.x → x, b = ∀x.x → x), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []); *)
    (* TODO: ("unify((a = ∀x.x → x, b = ∀x.x → x), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []); *)
    (* TODO: ("unify((a = ∀x.x → x, b = ∀x.x → x), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []); *)
  ] in

  let prefix = Prefix.create () in

  cases |> List.iter (fun (input, output, expected_errors) -> (
    let name = match List.length expected_errors with
    | 0 -> Printf.sprintf "%s = %s" input output
    | 1 -> Printf.sprintf "%s = %s with 1 error" input output
    | n -> Printf.sprintf "%s = %s with %n errors" input output n
    in
    test name (fun () -> (
      let (result, actual_errors) = Diagnostic.collect (fun () -> Prefix.level prefix (fun () -> (
        let tokens = Parser.tokenize (Stream.of_string input) in
        assert (Stream.next tokens = Identifier "unify");
        assert (Stream.next tokens = Glyph ParenthesesLeft);
        let _ = Parser.parse_prefix tokens (fun (name, bound) -> assert (Prefix.add prefix name bound = None)) in
        assert (Stream.next tokens = Glyph Comma);
        let actual = Parser.parse_monotype tokens in
        assert (Stream.next tokens = Glyph Comma);
        let expected = Parser.parse_monotype tokens in
        assert (Stream.next tokens = Glyph ParenthesesRight);
        Stream.empty tokens;
        let result = Unify.unify prefix actual expected in
        assert_equal (Printer.print_prefix prefix) output;
        result
      ))) in
      assert (match result with Ok () -> actual_errors = [] | Error _ -> actual_errors <> []);
      List.iter2 assert_equal (List.map Printer.print_diagnostic actual_errors) expected_errors
    ))
  ))
))
