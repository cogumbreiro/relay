
CIL := /home/jan/research/cil_1_3_5
CIL_OBJS := $(CIL)/obj/x86_LINUX
CIL_INCLUDE := -I $(CIL)/src -I $(CIL)/src/frontc \
               -I $(CIL)/src/ext -I $(CIL)/ocamlutil \
               -I $(CIL_OBJS)

OCAMLC   := ocamlc
OCAMLOPT := ocamlopt
OCAMLDEP := ocamldep
OCAMLDOC := ocamldoc

INCLUDES := $(CIL_INCLUDE) 
OCAMLFLAGS := -thread $(INCLUDES) -g -ccopt -L$(CIL_OBJS) \
        unix.cma str.cma threads.cma 
OCAMLOPTFLAGS := -thread $(INCLUDES) -dtypes -ccopt -L$(CIL_OBJS) \
        unix.cmxa str.cmxa threads.cmxa 

TARGETS = test1_exe test1_byte

test1_exe: test1.cmx
	$(OCAMLOPT) -o test1 $(OCAMLOPTFLAGS) $(CIL_OBJS)/cil.cmxa \
        test1.cmx 

test1_byte: test1.cmo
	$(OCAMLC) -o test1_byte $(OCAMLFLAGS) $(CIL_OBJS)/cil.cma test1.cmo

#--------------------------------------------------
# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.c.o:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<


clean:
	rm -f *.cm[iox]
	rm -f *.o
	rm -f *.annot
	rm -f $(TARGETS)
	rm .depend

.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

all: $(TARGETS)
