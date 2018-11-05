open TestFramework

let () =
  UnifyTest.run ();
  InferTest.run ();
  exit_tests ()
