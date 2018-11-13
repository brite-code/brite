open TestFramework

let () =
  KindTest.run ();
  AnnotationTest.run ();
  NormalFormTest.run ();
  UnifyTest.run ();
  InferTest.run ();
  JsSerializeTest.run ();
  exit_tests ()
