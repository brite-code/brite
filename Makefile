SRC=src/Ast src/Checker src/Diagnostics src/Parser src/Utils

.PHONY: clean test

clean:
	ocamlbuild -clean

test: test/Test.native
	./Test.native

src/%.native:
	ocamlbuild -Is "$(SRC)" $@

test/%.native:
	ocamlbuild -Is "$(SRC)" $@
