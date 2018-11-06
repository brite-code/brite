open TestFramework

let () =
  NormalFormTest.run ();
  UnifyTest.run ();
  InferTest.run ();
  exit_tests ()
