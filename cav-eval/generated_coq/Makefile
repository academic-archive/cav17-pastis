COQ = $(wildcard *.v)
OBJ = $(COQ:.v=.vo)

help:
	@echo '+ Rebuild 10 ".vo" files at random             make random'
	@echo '+ Attempt the compilation of all ".v" files    make all (LONG!)'
	@echo '+ Compile a single file "foo.v"                make foo.vo'
	@echo '+ Remove all the compiled ".vo" files          make clean'

all: $(OBJ)

%.vo: %.v
	coqc -Q ../../source/coq pasta $^ || true

RND := $(shell find . -name \*.vo | shuf -n 10)
random:
	rm $(RND)
	make $(RND)

clean:
	rm *.vo *.glob .*.vo.aux .lia.cache

.PHONY: help all random clean
