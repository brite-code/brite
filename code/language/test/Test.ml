open TestFramework

let () =
  NormalFormTest.run ();
  UnifyTest.run ();
  InferTest.run ();
  JsSerializeTest.run ();
  exit_tests ()
