SRC=src/Checker src/Compiler/Js src/Diagnostics src/Language src/Parser src/Utils
OCB=ocamlbuild -Is "$(SRC)"

.PHONY: all test clean

all: src/Main.native

test: test/Test.native
	./Test.native

clean:
	$(OCB) -clean

src/%.native:
	$(OCB) $@

test/%.native:
	$(OCB) $@
