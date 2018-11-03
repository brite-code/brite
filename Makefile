.PHONY: clean test

clean:
	ocamlbuild -clean

test: test/Test.native
	./Test.native

src/%.native:
	ocamlbuild $@

test/%.native:
	ocamlbuild -I src $@
