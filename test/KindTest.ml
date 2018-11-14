open TestFramework

let run () = suite "Kind" (fun () -> (
  test "value kinds unify" (fun () -> (
    let (result, diagnostics) = Diagnostics.collect (fun () -> (
      Kind.unify Kind.value Kind.value
    )) in
    assert (List.map Printer.print_diagnostic diagnostics = []);
    assert (result = Ok ())
  ));

  test "row kinds unify" (fun () -> (
    let (result, diagnostics) = Diagnostics.collect (fun () -> (
      Kind.unify Kind.row Kind.row
    )) in
    assert (List.map Printer.print_diagnostic diagnostics = []);
    assert (result = Ok ())
  ));

  test "value does not unify with row" (fun () -> (
    let (result, diagnostics) = Diagnostics.collect (fun () -> (
      Kind.unify Kind.value Kind.row
    )) in
    assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
    assert (result = Error (List.nth diagnostics 0))
  ));

  test "row does not unify with value" (fun () -> (
    let (result, diagnostics) = Diagnostics.collect (fun () -> (
      Kind.unify Kind.row Kind.value
    )) in
    assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds row and value."]);
    assert (result = Error (List.nth diagnostics 0))
  ));

  test "unknown kind unifies with value" (fun () -> (
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        Kind.unify Kind.value (Kind.unknown ())
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        Kind.unify (Kind.unknown ()) Kind.value
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    )
  ));

  test "unknown kind unifies with row" (fun () -> (
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        Kind.unify Kind.row (Kind.unknown ())
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        Kind.unify (Kind.unknown ()) Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    )
  ));

  test "unknown kind unifies with value twice" (fun () -> (
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify Kind.value unknown in
        assert (result = Ok ());
        Kind.unify Kind.value unknown
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify unknown Kind.value in
        assert (result = Ok ());
        Kind.unify Kind.value unknown
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify Kind.value unknown in
        assert (result = Ok ());
        Kind.unify unknown Kind.value
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify unknown Kind.value in
        assert (result = Ok ());
        Kind.unify unknown Kind.value
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    )
  ));

  test "unknown kind unifies with row twice" (fun () -> (
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify Kind.row unknown in
        assert (result = Ok ());
        Kind.unify Kind.row unknown
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify unknown Kind.row in
        assert (result = Ok ());
        Kind.unify Kind.row unknown
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify Kind.row unknown in
        assert (result = Ok ());
        Kind.unify unknown Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify unknown Kind.row in
        assert (result = Ok ());
        Kind.unify unknown Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    )
  ));

  test "unknown kind unified with value does not unify with row" (fun () -> (
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify Kind.value unknown in
        assert (result = Ok ());
        Kind.unify Kind.row unknown
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds row and value."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify Kind.value unknown in
        assert (result = Ok ());
        Kind.unify unknown Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify unknown Kind.value in
        assert (result = Ok ());
        Kind.unify Kind.row unknown
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds row and value."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify unknown Kind.value in
        assert (result = Ok ());
        Kind.unify unknown Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
      assert (result = Error (List.nth diagnostics 0))
    )
  ));

  test "unknown kind unified with row does not unify with value" (fun () -> (
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify Kind.row unknown in
        assert (result = Ok ());
        Kind.unify Kind.value unknown
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify Kind.row unknown in
        assert (result = Ok ());
        Kind.unify unknown Kind.value
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds row and value."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify unknown Kind.row in
        assert (result = Ok ());
        Kind.unify Kind.value unknown
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown = Kind.unknown () in
        let result = Kind.unify unknown Kind.row in
        assert (result = Ok ());
        Kind.unify unknown Kind.value
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds row and value."]);
      assert (result = Error (List.nth diagnostics 0))
    )
  ));

  test "unknown kind unifies with self" (fun () -> (
    let (result, diagnostics) = Diagnostics.collect (fun () -> (
      let unknown = Kind.unknown () in
      Kind.unify unknown unknown
    )) in
    assert (List.map Printer.print_diagnostic diagnostics = []);
    assert (result = Ok ())
  ));

  test "unknown kind unifies with self through other unknown" (fun () -> (
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        Kind.unify unknown1 unknown2
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown2 unknown1 in
        assert (result = Ok ());
        Kind.unify unknown1 unknown2
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        Kind.unify unknown2 unknown1
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown2 unknown1 in
        assert (result = Ok ());
        Kind.unify unknown2 unknown1
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = []);
      assert (result = Ok ())
    )
  ));

  test "unknowns unified with each other will not create infinite kinds 1" (fun () -> (
    (
      let ((), diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown2 unknown1 in
        assert (result = Ok ());
        let result = Kind.unify unknown1 Kind.value in
        assert (result = Ok ());
        let result = Kind.unify unknown1 Kind.value in
        assert (result = Ok ())
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = [])
    );
    (
      let ((), diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown1 Kind.value in
        assert (result = Ok ());
        let result = Kind.unify unknown1 Kind.value in
        assert (result = Ok ())
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = [])
    );
    (
      let ((), diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown2 unknown1 in
        assert (result = Ok ());
        let result = Kind.unify unknown2 Kind.value in
        assert (result = Ok ());
        let result = Kind.unify unknown2 Kind.value in
        assert (result = Ok ())
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = [])
    );
    (
      let ((), diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown2 Kind.value in
        assert (result = Ok ());
        let result = Kind.unify unknown2 Kind.value in
        assert (result = Ok ())
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = [])
    )
  ));

  test "unknowns unified with each other will not create infinite kinds 2" (fun () -> (
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown2 unknown1 in
        assert (result = Ok ());
        let result = Kind.unify unknown1 Kind.value in
        assert (result = Ok ());
        Kind.unify unknown1 Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown1 Kind.value in
        assert (result = Ok ());
        Kind.unify unknown1 Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown2 unknown1 in
        assert (result = Ok ());
        let result = Kind.unify unknown2 Kind.value in
        assert (result = Ok ());
        Kind.unify unknown2 Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
      assert (result = Error (List.nth diagnostics 0))
    );
    (
      let (result, diagnostics) = Diagnostics.collect (fun () -> (
        let unknown1 = Kind.unknown () in
        let unknown2 = Kind.unknown () in
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown1 unknown2 in
        assert (result = Ok ());
        let result = Kind.unify unknown2 Kind.value in
        assert (result = Ok ());
        Kind.unify unknown2 Kind.row
      )) in
      assert (List.map Printer.print_diagnostic diagnostics = ["Incompatible kinds value and row."]);
      assert (result = Error (List.nth diagnostics 0))
    )
  ));

  ()
))
