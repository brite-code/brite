open TestFramework

let () = suite "NormalForm" (fun () -> (

let cases = [
  ("x", "x");
] in

cases |> List.iter (fun (input, output) -> (
  test (Printf.sprintf "nf(%s) = %s" input output) (fun () -> (
    let tokens = Parser.tokenize (Stream.of_string input) in
    let input = Parser.parse_polytype tokens in
    Stream.empty tokens;
    let (input, _) = Diagnostic.collect (fun () -> Infer.convert_polytype input) in
    let input = Printer.print_polytype input in
    assert_equal input output
  ))
))

))
