all: Driver.native

%.cmo %.mli %.byte %.native: force
	@ocamlbuild $@

clean:
	rm -fr *.native *.byte _build

force: ;

.PHONY: all clean force
