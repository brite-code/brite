.PHONY: clean

clean:
	ocamlbuild -clean

%.native:
	ocamlbuild $@
