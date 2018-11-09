SRC=code/language/src/{Ast,Checker,Compiler/Js,Diagnostics,Parser,Utils}
OCB=ocamlbuild -Is "$(shell echo $(SRC))"

.PHONY: all test clean

all: code/language/src/Main.native

test: code/language/test/Test.native
	./Test.native

clean:
	$(OCB) -clean

code/language/src/%.native:
	$(OCB) $@

code/language/test/%.native:
	$(OCB) $@
