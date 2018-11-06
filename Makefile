SRC=src/Ast src/Checker src/Compiler/Js src/Diagnostics src/Parser src/Utils

.PHONY: all clean test

all: src/Main.native

clean:
	ocamlbuild -clean

test: test/Test.native
	./Test.native

src/%.native:
	ocamlbuild -Is "$(SRC)" $@

test/%.native:
	ocamlbuild -Is "$(SRC)" $@
