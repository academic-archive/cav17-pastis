include ../config.mk

all: Driver.native

%.cmo %.mli %.byte %.native: force
	@CXX=$(CXX) ocamlbuild $@

clean:
	rm -fr *.native *.byte _build

force: ;

.PHONY: all clean force
