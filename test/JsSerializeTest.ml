open TestFramework

let run () = suite "JsSerialize" (fun () -> (
  let cases = [
    (
"λx.x",
"\
x => x;
"
    );
    (
"λx.λy.x y",
"\
x => y => x(y);
"
    );
    (
"λf.(λx.λv.f (x x) v) (λx.λv.f (x x) v)",
"\
f => (x => v => f(x(x))(v))(x => v => f(x(x))(v));
"
    );
    (
"λf.let x = λx.λv.f (x x) v in λv.f (x x) v",
"\
f => {
  let x = x => v => f(x(x))(v);
  return v => f(x(x))(v);
};
"
    );
  ] in

  cases |> List.iter (fun (input, output) -> (
    test input (fun () -> (
      let tokens = Parser.tokenize (Stream.of_string input) in
      let expression = Parser.parse_expression tokens in
      Stream.empty tokens;
      let program = JsSerialize.serialize expression in
      let program = JsAst.print_program program in
      assert_equal (Printf.sprintf "%S" program) (Printf.sprintf "%S" output)
    ))
  ))
))
