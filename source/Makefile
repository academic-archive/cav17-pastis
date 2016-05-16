all: Driver.native

%.cmo %.mli %.byte %.native: force
	@SANDBOX="`pwd`/../packages/sandbox" ocamlbuild $@

clean:
	rm -fr *.native *.byte _build

force: ;

.PHONY: all clean force
