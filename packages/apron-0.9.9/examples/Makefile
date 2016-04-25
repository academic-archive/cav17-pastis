include ../Makefile.config

ICFLAGS = \
-I$(GMP_PREFIX)/include \
-I$(MPFR_PREFIX)/include \
-I$(APRON_PREFIX)/include \
-Wall -Wstrict-prototypes -Wimplicit-function-declaration \
-std=c99
# -Winline -Wconversion

LCFLAGS = \
-L$(GMP_PREFIX)/lib \
-L$(MPFR_PREFIX)/lib \
-L$(APRON_PREFIX)/lib \
-L$(PPL_PREFIX)/lib \
-L../mlapronidl \
-L../mlgmpidl \
-L../newpolka \
-L../box \
-L../itv \
-L../apron \
-L../octagons \
-L$(CAMLIDL_PREFIX)/lib/ocaml

OCAMLINC = \
-I $(MLGMPIDL_PREFIX)/lib \
-I $(APRON_PREFIX)/lib \
-I ../mlapronidl \
-I ../mlgmpidl \
-I ../newpolka \
-I ../box \
-I ../octagons 

OCAMLLDFLAGS = \
-noautolink -ccopt "$(LCFLAGS)" \
bigarray.cma gmp.cma apron.cma \
# box.cma oct.cma polka.cma ppl.cma \
box.cma oct.cma polka.cma \
-cclib "-lpolka_caml -lpolka_debug -loct_caml -loctMPQ -lbox_caml -lbox_debug -litv_debug -lapron_caml_debug -lapron_debug -lgmp_caml -lmpfr -lgmpxx -lgmp -lbigarray -lcamlidl"

OCAMLOPTLDFLAGS = \
-noautolink -verbose -ccopt "$(LCFLAGS)" \
bigarray.cmxa gmp.cmxa apron.cmxa box.cmxa polka.cmxa \
-cclib "-lpolka_caml -lpolka_debug -loct_caml -loctMPQ -lbox_caml -lbox_debug -litv_debug -lapron_caml_debug -lapron_debug -lgmp_caml -lmpfr -lgmpxx -lgmp -lbigarray -lcamlidl"

all: C ml

# C examples

C: example1g

%g: %g.o
	$(CXX) $(CXXFLAGS) $(ICFLAGS) $(LCFLAGS) -o $@  $< -lapron_ppl_debug -lppl -lgmpxx -lpolkag_debug -loctQg_debug -lboxMPQ_debug -lapron_debug -litvMPQ_debug -litvdbl_debug -lmpfr -lgmp

%g.o: %.c
	$(CC) $(CFLAGS_DEBUG) $(ICFLAGS) -c -o $@ $<

# OCaml examples

ml: mlexample1.opt

mlexample%.opt: mlexample%.cmx
	$(OCAMLOPT) -cc "g++" $(OCAMLOPTFLAGS) $(OCAMLINC) $(OCAMLOPTLDFLAGS) -o $@ $<
test.opt: test.cmx
	$(OCAMLOPT) -cc "g++" $(OCAMLOPTFLAGS) $(OCAMLINC) $(OCAMLOPTLDFLAGS) -o $@ $<

%: %.cmo $(APRON_PREFIX)/bin/apronrun
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) $(OCAMLLDFLAGS) -g -use-runtime $(APRON_PREFIX)/bin/apronrun -o $@ $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC)  -c -o $@ $<

#

clean:
	rm -f example1g *.cm[ioxa] *.o mlexample1 mlexample2 mlexample3 mlexample4 mlexample5 *.opt

distclean: clean

dist: example1.c mlexample1.ml mlexample2.ml mlexample3.ml Makefile README
	(cd ..; tar zcvf examples.tgz $(^:%=examples/%))
