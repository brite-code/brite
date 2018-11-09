open TestFramework

let run () = suite "Infer" (fun () -> (
  let cases = [
    ("infer((∅), (x: boolean), x)", "((∅), boolean)", []);
    ("infer((∅), (x: number), x)", "((∅), number)", []);
    ("infer((∅), (x: ∀a.a → a), x)", "((∅), ∀a.a → a)", []);
    ("infer((∅), (x: ∀a.a), x)", "((∅), ∀a.a)", []);
    ("infer((∅), (x: ∀a.number), x)", "((∅), ∀a.number)", []);
    ("infer((∅), (∅), λx.x)", "((∅), ∀t1.t1 → t1)", []);
    ("infer((∅), (add1: number → number), add1 42)", "((∅), number)", []);
    ("infer((∅), (add1: number → number), add1 true)", "((∅), number)", ["number ≢ boolean"]);
    ("infer((a), (add1: number → number, x: a), add1 x)", "((a = number), number)", []);
    ("infer((∅), (add1: number → number, x: ⊥), add1 x)", "((∅), number)", []);
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
    ("infer((∅), (∅), nope)", "((∅), ⊥)", ["Unbound variable `nope`."]);
    ("infer((∅), (∅), true)", "((∅), boolean)", []);
    ("infer((∅), (∅), let x = true in x)", "((∅), boolean)", []);
    ("infer((∅), (∅), let x = true in let y = x in y)", "((∅), boolean)", []);
    ("infer((∅), (∅), let x = true in let y = x in x)", "((∅), boolean)", []);
    ("infer((∅), (∅), λx.true)", "((∅), ∀t1.t1 → boolean)", []);
    ("infer((∅), (∅), λx.let y = x in y)", "((∅), ∀t1.t1 → t1)", []);
    ("infer((∅), (∅), true 42)", "((∅), ⊥)", ["boolean ≢ number → t1"]);
    ("infer((b), (choose: ∀a.a → a → a, x: b), choose x 42)", "((b = number), number)", []);
    ("infer((b), (choose: ∀a.a → a → a, x: b), choose 42 x)", "((b = number), number)", []);
    ("infer((∅), (choose: ∀a.a → a → a), λx.choose x 42)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a), λx.choose 42 x)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a), λx.let y = choose x 42 in x)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a), λx.let y = choose 42 x in x)", "((∅), number → number)", []);
    ("infer((b), (choose: ∀a.a → a → a, id: ∀a.a → a, x: b), choose x id)", "((b ≥ ∀a.a → a), b)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a), λx.choose x id)", "((∅), ∀(t1 ≥ ∀a.a → a).t1 → t1)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a), λx.choose id x)", "((∅), ∀(t1 ≥ ∀a.a → a).t1 → t1)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a), λx.let y = choose x id in x)", "((∅), ∀(t1 ≥ ∀a.a → a).t1 → t1)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a), λx.let y = choose id x in x)", "((∅), ∀(t1 ≥ ∀a.a → a).t1 → t1)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), choose id add1)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), choose add1 id)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), (λx.choose x id) add1)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), (λx.choose id x) add1)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a), (λx.choose x true) 42)", "((∅), boolean)", ["boolean ≢ number"]);
    ("infer((∅), (choose: ∀a.a → a → a), (λx.choose true x) 42)", "((∅), boolean)", ["boolean ≢ number"]);
    ("infer((∅), (choose: ∀a.a → a → a), λf.λx.choose (f x) x)", "((∅), ∀t3.(t3 → t3) → t3 → t3)", []);
    ("infer((∅), (choose: ∀a.a → a → a), (λf.λx.choose (f x) x) (λx.x))", "((∅), ∀t4.t4 → t4)", []);
    ("infer((∅), (choose: ∀a.a → a → a, undefined: ⊥), (λf.λx.choose (f x) x) (λx.undefined))", "((∅), ∀t4.t4 → t4)", []);
    ("infer((∅), (choose: ∀a.a → a → a, undefined: ⊥), λf.λx.choose (f x) undefined)", "((∅), ∀(t2, t3).(t2 → t3) → t2 → t3)", []);
    ("infer((∅), (∅), (42: boolean))", "((∅), boolean)", ["number ≢ boolean"]);
    ("infer((∅), (∅), let f = (λx.42: boolean → number) in (f: number → boolean))", "((∅), number → boolean)", ["boolean ≢ number"; "number ≢ boolean"]);
    ("infer((∅), (∅), let f = (λx.42: boolean → number) in (f: boolean → number))", "((∅), boolean → number)", []);
    ("infer((∅), (∅), let f = (λx.42: boolean → number) in (f: number → number))", "((∅), number → number)", ["boolean ≢ number"]);
    ("infer((∅), (∅), let f = (λx.42: boolean → number) in (f: boolean → boolean))", "((∅), boolean → boolean)", ["number ≢ boolean"]);
    ("infer((∅), (∅), (λx.42: boolean → number))", "((∅), boolean → number)", []);
    ("infer((∅), (∅), (λx.x: boolean → number))", "((∅), boolean → number)", ["boolean ≢ number"]);
    ("infer((∅), (∅), (λx.x: boolean → boolean))", "((∅), boolean → boolean)", []);
    ("infer((∅), (∅), ((λx.x: boolean → boolean): ∀a.a → a))", "((∅), ∀a.a → a)", ["∀a.a → a ≢ boolean → boolean"]);
    ("infer((a, b), (f: a, x: b), f x)", "((b, t1, a = b → t1), t1)", []);
    ("infer((∅), (∅), λf.λx.f x)", "((∅), ∀(t2, t3).(t2 → t3) → t2 → t3)", []);
    ("infer((∅), (app: ∀(a, b).(a → b) → a → b, add1: number → number), app add1)", "((∅), number → number)", []);
    ("infer((∅), (app: ∀(a, b).(a → b) → a → b, add1: number → number), app add1 0)", "((∅), number)", []);
    ("infer((∅), (app: ∀(a, b).(a → b) → a → b), app (λx.x))", "((∅), ∀b.b → b)", []);
    ("infer((∅), (app: ∀(a, b).(a → b) → a → b), app (λx.x) 42)", "((∅), number)", []);
    ("infer((∅), (choose: ∀a.a → a → a), choose (λx.x))", "((∅), ∀(t2 ≥ ∀t1.t1 → t1).t2 → t2)", []);
    ("infer((∅), (choose: ∀a.a → a → a), choose (λx.x) 42)", "((∅), ∀t1.t1 → t1)", ["t4 → t4 ≢ number"]);
    ("infer((∅), (choose: ∀a.a → a → a), choose 42 (λx.x))", "((∅), number)", ["number ≢ t3 → t3"]);
    ("infer((∅), (∅), λx.x x)", "((∅), ∀(t1, t2).t1 → t2)", ["Infinite type since `t1` occurs in `t1 → t2`."]);
    ("infer((∅), (choose: ∀a.a → a → a), λx.λy.let z = choose x y in x y)", "((∅), ∀(t1, t2 ≥ ∀t3.t1 → t3).t1 → t2)", ["Infinite type since `t1` occurs in `t2 → t3`."]);
    ("infer((∅), (choose: ∀a.a → a → a), λx.λy.let z = choose y x in x y)", "((∅), ∀(t1, t2 ≥ ∀t3.t1 → t3).t1 → t2)", ["Infinite type since `t1` occurs in `t2 → t3`."]);
    ("infer((∅), (choose: ∀a.a → a → a), λx.λy.let z = choose x y in y x)", "((∅), ∀(t1, t2 ≥ ∀t3.t1 → t3).t1 → t2)", ["Infinite type since `t1` occurs in `t1 → t3`."]);
    ("infer((∅), (choose: ∀a.a → a → a), λx.λy.let z = choose y x in y x)", "((∅), ∀(t1, t2 ≥ ∀t3.t1 → t3).t1 → t2)", ["Infinite type since `t1` occurs in `t1 → t3`."]);
    ("infer((∅), (∅), λx.let x = (x: ∀a.a → a) in x x)", "((∅), ∀(t1 = ∀a.a → a, t2 ≥ ∀a.a → a).t1 → t2)", []);
    ("infer((∅), (add1: number → number), (λx.let x = (x: ∀a.a → a) in x x) add1)", "((∅), ∀a.a → a)", ["∀a.a → a ≢ number → number"]);
    ("infer((∅), (add1: number → number), (λx.let x = (x: ∀a.a → a) in x x: ∀(a = ∀a.a → a).a → a) add1)", "((∅), ∀a.a → a)", ["∀a.a → a ≢ number → number"]);
    ("infer((∅), (auto: ∀(a = ∀a.a → a).a → a, add1: number → number), auto add1)", "((∅), ∀a.a → a)", ["∀a.a → a ≢ number → number"]);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), choose id id)", "((∅), ∀a.a → a)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), choose add1 add1)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), choose id)", "((∅), ∀(t2 ≥ ∀a.a → a).t2 → t2)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), choose add1)", "((∅), (number → number) → number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), choose id add1)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), choose add1 id)", "((∅), number → number)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, auto: ∀(a = ∀a.a → a).a → a), choose id auto)", "((∅), ∀(a = ∀a.a → a).a → a)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, auto: ∀(a = ∀a.a → a).a → a), choose auto id)", "((∅), ∀(a = ∀a.a → a).a → a)", []);
    ("infer((∅), (id: ∀a.a → a, auto: ∀(a = ∀a.a → a).a → a), id auto)", "((∅), ∀(a = ∀a.a → a).a → a)", []);
    ("infer((∅), (id: ∀a.a → a, auto: ∀(a = ∀a.a → a).a → a), auto id)", "((∅), ∀a2.a2 → a2)", []);
    ("infer((∅), (id: ∀a.a → a, auto: ∀(a = ∀a.a → a).a → a), (λx.x id))", "((∅), ∀(t2 ≥ ∀a.a → a, t3).(t2 → t3) → t3)", []);
    ("infer((∅), (id: ∀a.a → a, auto: ∀(a = ∀a.a → a).a → a), (λx.x id) auto)", "((∅), ∀a2.a2 → a2)", []);
    ("infer((∅), (∅), (λx.x (λx.x)) (λx.let x = (x: ∀a.a → a) in x x))", "((∅), ∀a.a → a)", []);
    ("infer((∅), (app: ∀(a, b).(a → b) → a → b, auto: ∀(a = ∀a.a → a).a → a, id: ∀a.a → a), app auto id)", "((∅), ∀a.a → a)", []);
    ("infer((∅), (∅), (λf.λx.f x) (λx.let x = (x: ∀a.a → a) in x x) (λx.x))", "((∅), ∀a.a → a)", []);
    ("infer((∅), (undefined: ⊥), λx.undefined)", "((∅), ∀(t1, t2).t1 → t2)", []);
    ("infer((∅), (∅), let id = λx.x in (id: ∀x.x → number))", "((∅), ∀x.x → number)", ["∀x.x → number ≢ number → number"]);
    ("infer((∅), (∅), let id = λx.x in let id = (id: ∀x.x → number) in (id: ∀x.x → x))", "((∅), ∀x.x → x)", ["∀x.x → number ≢ number → number"; "∀x.x → x ≢ number → number"]);
    ("infer((∅), (∅), let f = λx.42 in (f: ∀x.x → boolean))", "((∅), ∀x.x → boolean)", ["number ≢ boolean"]);
    ("infer((∅), (auto: ∀(a = ∀a.a → a).a → a, add1: number → number), auto add1)", "((∅), ∀a.a → a)", ["∀a.a → a ≢ number → number"]);
    ("infer((∅), (add1: number → number), let id = (λx.x: ∀x.x → number) in add1 (id true))", "((∅), number)", ["∀x.x → number ≢ number → number"]);
    ("infer((∅), (add1: number → number), let id = (λx.true: ∀x.x → x) in add1 (id 42))", "((∅), number)", ["∀x.x → x ≢ boolean → boolean"]);
    ("infer((∅), (add1: number → number), let id = (λx.true: ∀x.x → x) in add1 (id 42))", "((∅), number)", ["∀x.x → x ≢ boolean → boolean"]);
    ("infer((∅), (add1: number → number), let id = λx.x in add1 (id true))", "((∅), number)", ["number ≢ boolean"]);
    ("infer((∅), (add1: number → number), let id = λx.x in let add1 = λx.add1 (id x) in add1 true)", "((∅), number)", ["number ≢ boolean"]);
    ("infer((∅), (undefined: ⊥, choose: ∀a.a → a → a), choose (undefined: ∀(x ≥ ∀(a ≥ ⊥, b = ⊥).a → b).x → x) (undefined: ∀(x ≥ ∀(a = ⊥, b ≥ ⊥).a → b).x → x))", "((∅), ∀(x ≥ ∀(a = ⊥, b = ⊥).a → b).x → x)", []);
    ("infer((∅), (auto: ∀(a = ∀a.a → a).a → a, add1: number → number), (auto: ∀(x ≥ ∀a.a → a).x → x))", "((∅), ∀(x ≥ ∀a.a → a).x → x)", ["∀(x ≥ ∀a.a → a).x → x ≢ ∀(a = ∀a2.a2 → a2).a → a"]);
    ("infer((∅), (auto: ∀(a = ∀a.a → a).a → a, add1: number → number), (auto: ∀(x ≥ ∀a.a → a).x → x) add1)", "((∅), number → number)", ["∀(x ≥ ∀a.a → a).x → x ≢ ∀(a = ∀a2.a2 → a2).a → a"]);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), (choose id: ∀(x = ∀a.a → a).x → x))", "((∅), ∀(x = ∀a.a → a).x → x)", []);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), (choose id: ∀(x = ∀a.a → a).x → x) add1)", "((∅), ∀a.a → a)", ["∀a.a → a ≢ number → number"]);
    ("infer((∅), (choose: ∀a.a → a → a, id: ∀a.a → a, add1: number → number), (choose id: ∀(x ≥ ∀a.a → a).x → x) add1)", "((∅), number → number)", []);
    ("infer((∅), (add1: number → number), (add1: ∀a.a → a))", "((∅), ∀a.a → a)", ["∀a.a → a ≢ number → number"]);
    ("infer((∅), (∅), (λx.x: number → number))", "((∅), number → number)", []);
    ("infer((∅), (∅), (λx.x: ∀a.a → a) 42)", "((∅), number)", []);
    ("infer((a, b, c), (a: a, b: b, c: c), if a then b else c)", "((a = boolean, b, c = b), b)", []);
    ("infer((∅), (∅), λx.λy.if true then x else y)", "((∅), ∀t1.t1 → t1 → t1)", []);
    ("infer((∅), (∅), if 42 then true else false)", "((∅), boolean)", ["number ≢ boolean"]);
    ("infer((∅), (∅), if true then true else false)", "((∅), boolean)", []);
    ("infer((∅), (∅), if true then 1 else 0)", "((∅), number)", []);
    ("infer((∅), (∅), if true then 1 else false)", "((∅), ⊥)", ["number ≢ boolean"]);
    ("infer((∅), (∅), if true then true else 0)", "((∅), ⊥)", ["boolean ≢ number"]);
    ("infer((∅), (id: ∀a.a → a, add1: number → number), if true then id else add1)", "((∅), number → number)", []);
    ("infer((∅), (id: ∀a.a → a, add1: number → number), if true then add1 else id)", "((∅), number → number)", []);
  ] in

  let prefix = Prefix.create () in

  cases |> List.iter (fun (input, output, expected_errors) -> (
    let name = match List.length expected_errors with
    | 0 -> Printf.sprintf "%s = %s" input output
    | 1 -> Printf.sprintf "%s = %s with 1 error" input output
    | n -> Printf.sprintf "%s = %s with %n errors" input output n
    in
    test name (fun () -> (
      let ((), actual_errors) = Diagnostics.collect (fun () -> Prefix.level prefix (fun () -> (
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
