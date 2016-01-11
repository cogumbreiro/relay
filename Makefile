

#--------------------------------------------------
# User-defined constants

CIL := cil
CIL_OBJS := $(CIL)/obj/x86_LINUX
CIL_INCLUDE := -I $(CIL)/src -I $(CIL)/src/frontc -I $(CIL_OBJS)
DS_DIR := datastructs
PTA_DIR := pta
VPATH = $(DS_DIR) $(PTA_DIR)

#--------------------------------------------------
# System-dependent settings



#--------------------------------------------------
# Selection of system-dependent settings

OS_VER_STR = $(shell uname -rs)

ifneq (,$(findstring SunOS,$(OS_VER_STR)))
	include Makefile.SunOS
else
ifneq (,$(findstring Linux, $(OS_VER_STR)))
	include Makefile.Linux
else
endif
endif


#--------------------------------------------------
# Compiler consts

#OCAMLDIR := /home/jan/local/bin

#OCAMLC := $(OCAMLDIR)/ocamlc
#OCAMLOPT := $(OCAMLDIR)/ocamlopt
#OCAMLDEP := $(OCAMLDIR)/ocamldep
#OCAMLDOC := $(OCAMLDIR)/ocamldoc

#INCLUDES := $(CIL_INCLUDE) -I $(DS_DIR) -I $(PTA_DIR) -I +zip

#-I +zip
#zip.cma
#zip.cmxa

OCAMLC := ocamlc
OCAMLOPT := ocamlopt
OCAMLDEP := ocamldep
OCAMLDOC := ocamldoc

INCLUDES := $(CIL_INCLUDE) -I $(DS_DIR) -I $(PTA_DIR)
OCAMLFLAGS := -thread $(INCLUDES) -g -ccopt -L$(CIL_OBJS) \
	unix.cma str.cma threads.cma statfs_c.o
OCAMLOPTFLAGS := -thread $(INCLUDES) -dtypes -ccopt -L$(CIL_OBJS) \
	unix.cmxa str.cmxa threads.cmxa statfs_c.o 


#--------------------------------------------------
# Objs/targets

UTILS := gen_num strutil logging mystats gc_stats stdutil statfs \
	config gz_marshal scp inspect timeout

DATA_STRUCTS := queueset stackset mapset iset uf intrange hashcons hset hmap \
	simplehc

PTA := pta_types pta_compile pta_fi_eq pta_fb_eq

BACKING_STORE := backed_summary

REQUEST := messages distributed file_serv request

FCACHE := cache filecache default_cache

ID_FIX_CG = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer $(FCACHE) \
	cilinfos $(PTA) alias_types alias lvals scope \
	callg dumpcalls readcalls \
	fix_id_cg 

TEST_PTA = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer $(FCACHE) \
	cilinfos $(PTA) alias_types \
	alias lvals scope callg dumpcalls readcalls test_pta 


RACE = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer offset $(FCACHE) $(BACKING_STORE) cilinfos \
	$(PTA) alias_types alias lvals scope callg dumpcalls readcalls \
	scc entry_points threads shared \
	checkpoint relative_set lockset guarded_access warn_reports race_reports \
	$(REQUEST) safer_sum modsummary racesummary interDataflow \
	sym_types symsummary manage_sums \
	symstate2 intraDataflow racestate \
	roots race_warnings race_anal 


SERVER_SOCKET = $(UTILS) $(DATA_STRUCTS) fstructs callg cil_lvals \
	cilfiles id_fixer $(FCACHE) $(BACKING_STORE) \
	cilinfos $(PTA) alias_types \
	alias lvals scope dumpcalls readcalls scc relative_set lockset \
	guarded_access warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum modsummary racesummary \
	entry_points threads server_socket

CG_DOT = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer $(FCACHE) \
	cilinfos $(PTA) alias_types alias lvals scope \
	callg dumpcalls readcalls scc entry_points threads cg_to_dot 

SCC_STATS = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer $(FCACHE) distributed \
	cilinfos $(PTA) alias_types alias lvals scope \
	callg dumpcalls readcalls scc scc_stats

INSTR_STATS = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer $(FCACHE) \
	cilinfos $(PTA) alias_types alias lvals scope \
	callg dumpcalls readcalls scc instr_stats

MERGE = $(UTILS) merge_sources

TEST_MEMUSAGE = $(UTILS) test_memusage

PRINTSUMM = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer $(FCACHE) $(BACKING_STORE) cilinfos \
	$(PTA) alias_types alias lvals scope relative_set lockset \
	offset guarded_access warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum modsummary racesummary \
	sym_types symsummary print_summary	

PRINTWARN = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer offset $(FCACHE) cilinfos $(PTA) \
	alias_types alias lvals scope callg dumpcalls readcalls \
	checkpoint relative_set lockset \
	$(BACKING_STORE) guarded_access warn_reports race_reports \
	$(REQUEST) safer_sum modsummary racesummary scc entry_points threads \
	interDataflow shared sym_types symsummary manage_sums symstate2 racestate \
	roots race_warnings print_warnings

WARNSTATS = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer offset $(FCACHE) cilinfos $(PTA) \
	alias_types alias lvals scope $(BACKING_STORE) \
	relative_set lockset guarded_access warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum modsummary racesummary warn_stats 


SYMSTATE = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer offset $(FCACHE) $(BACKING_STORE) \
	cilinfos $(PTA) alias_types alias lvals scope callg \
	dumpcalls readcalls scc checkpoint relative_set lockset guarded_access \
	warn_reports race_reports $(REQUEST) \
	safer_sum modsummary racesummary entry_points threads \
	shared interDataflow sym_types symsummary symstate2 test_symstate

TEST_DS = $(UTILS) $(DATA_STRUCTS) test_datastructs


TEST_INSPECT = $(UTILS) $(DATA_STRUCTS) fstructs cil_lvals \
	cilfiles id_fixer offset $(FCACHE) $(BACKING_STORE) cilinfos \
	$(PTA) alias_types alias lvals scope callg dumpcalls readcalls \
	scc entry_points threads shared \
	relative_set lockset guarded_access warn_reports race_reports \
	checkpoint $(REQUEST) safer_sum modsummary racesummary  \
	sym_types symsummary manage_sums \
	symstate2 interDataflow racestate test_inspect 


BIN_TARGETS = fix_id_cg.exe race_anal.exe printwarn.exe printsumm.exe \
	scc_stats.exe merge_src.exe test_memusage.exe test_ds.exe \
	test_symstate.exe cg_to_dot.exe \
	test_pta.exe server.exe warn_stats.exe instr_stats.exe \
	inspect.exe

BYTE_TARGETS = fix_id_cg_byte race_byte printwarn_byte printsumm_byte \
	runtest_byte symstate_byte cg_to_dot_byte \
	test_pta_byte server_byte warn_stats_byte \
	inspect_byte 

TARGETS = .depend $(BIN_TARGETS) $(BYTE_TARGETS)

.PHONY: all clean htmldoc dot

all: .depend $(TARGETS)



#--------------------------------------------------------
# ML <-> C interface files (requires the C obj file also)

statfs.cmo: statfs.ml statfs_c.o
	$(OCAMLC) $(OCAMLFLAGS) -custom -c $<

statfs.cmx: statfs.ml statfs_c.o
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<


#--------------------------------------------------
# Fix ids + dump the call graph


ID_FIX_CG_OBJS = $(addsuffix .cmx, $(ID_FIX_CG))

fix_id_cg.exe: $(ID_FIX_CG_OBJS)
	$(OCAMLOPT) -o fix_id_cg.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(ID_FIX_CG_OBJS)


ID_FIX_CG_BYTE = $(addsuffix .cmo, $(ID_FIX_CG))

fix_id_cg_byte: $(ID_FIX_CG_BYTE)
	$(OCAMLC) -o fix_id_cg_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(ID_FIX_CG_BYTE)


#--------------------------------------------------
# Fix ids + dump the call graph

TEST_PTA_OBJS = $(addsuffix .cmx, $(TEST_PTA))

test_pta.exe: $(TEST_PTA_OBJS)
	$(OCAMLOPT) -o test_pta.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(TEST_PTA_OBJS)


TEST_PTA_BYTE = $(addsuffix .cmo, $(TEST_PTA))

test_pta_byte: $(TEST_PTA_BYTE)
	$(OCAMLC) -o test_pta_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(TEST_PTA_BYTE)


#--------------------------------------------------
# Main analysis

# The list of object files for the byte code version
RACE_BYTE_OBJS = $(addsuffix .cmo, $(RACE))

race_byte: $(RACE_BYTE_OBJS)
	$(OCAMLC) -o race_byte $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $(RACE_BYTE_OBJS)

# The list of object files for the native version
RACE_NATIVE_OBJS = $(addsuffix .cmx, $(RACE))

race_anal.exe: $(RACE_NATIVE_OBJS)
	$(OCAMLOPT) -o race_anal.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(RACE_NATIVE_OBJS)



#--------------------------------------------------
# Server for distributed mode (using sockets)

# The list of object files for the byte code version
SERVER_SOCKET_BYTE_OBJS = $(addsuffix .cmo, $(SERVER_SOCKET))

server_byte: $(SERVER_SOCKET_BYTE_OBJS)
	$(OCAMLC) -o server_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(SERVER_SOCKET_BYTE_OBJS)

# The list of object files for the native version
SERVER_SOCKET_NATIVE_OBJS = $(addsuffix .cmx, $(SERVER_SOCKET))

server.exe: $(SERVER_SOCKET_NATIVE_OBJS)
	$(OCAMLOPT) -o server.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(SERVER_SOCKET_NATIVE_OBJS)


#--------------------------------------------------
# Convert cg to dot graphs

CG_DOT_OBJS = $(addsuffix .cmx, $(CG_DOT))

cg_to_dot.exe: $(CG_DOT_OBJS)
	$(OCAMLOPT) -o cg_to_dot.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(CG_DOT_OBJS)

CG_DOT_BYTE_OBJS = $(addsuffix .cmo, $(CG_DOT))

cg_to_dot_byte: $(CG_DOT_BYTE_OBJS)
	$(OCAMLC) -o cg_to_dot_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(CG_DOT_BYTE_OBJS)


#--------------------------------------------------
# Scc stat printer

SCC_STATS_OBJS = $(addsuffix .cmx, $(SCC_STATS))

scc_stats.exe: $(SCC_STATS_OBJS)
	$(OCAMLOPT) -o scc_stats.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(SCC_STATS_OBJS)

#--------------------------------------------------
# Instr stat printer

INSTR_STATS_OBJS = $(addsuffix .cmx, $(INSTR_STATS))

instr_stats.exe: $(INSTR_STATS_OBJS)
	$(OCAMLOPT) -o instr_stats.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(INSTR_STATS_OBJS)

#--------------------------------------------------
# Source merger

MERGE_OBJS = $(addsuffix .cmx, $(MERGE))

merge_src.exe: $(MERGE_OBJS)
	$(OCAMLOPT) -o merge_src.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(MERGE_OBJS)


#--------------------------------------------------
# Load / Run some analyses test

TEST_MEMUSAGE_OBJS = $(addsuffix .cmx, $(TEST_MEMUSAGE))

test_memusage.exe: $(TEST_MEMUSAGE_OBJS)
	$(OCAMLOPT) -o test_memusage.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(TEST_MEMUSAGE_OBJS)

TEST_MEMUSAGE_BYTE_OBJS = $(addsuffix .cmo, $(TEST_MEMUSAGE))

runtest_byte: $(TEST_MEMUSAGE_BYTE_OBJS)
	$(OCAMLC) -o runtest_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(TEST_MEMUSAGE_BYTE_OBJS)

#--------------------------------------------------
# Load / Echo a summary

PRINTSUMM_OBJS = $(addsuffix .cmx, $(PRINTSUMM))

printsumm.exe: $(PRINTSUMM_OBJS)
	$(OCAMLOPT) -o printsumm.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(PRINTSUMM_OBJS)

PRINTSUMM_BYTE_OBJS = $(addsuffix .cmo, $(PRINTSUMM))

printsumm_byte: $(PRINTSUMM_BYTE_OBJS)
	$(OCAMLC) -o printsumm_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(PRINTSUMM_BYTE_OBJS)

#--------------------------------------------------
# Load summaries, print race_warnings

PRINTWARN_OBJS = $(addsuffix .cmx, $(PRINTWARN))

printwarn.exe: $(PRINTWARN_OBJS)
	$(OCAMLOPT) -o printwarn.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(PRINTWARN_OBJS)

PRINTWARN_BYTE_OBJS = $(addsuffix .cmo, $(PRINTWARN))

printwarn_byte: $(PRINTWARN_BYTE_OBJS)
	$(OCAMLC) -o printwarn_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(PRINTWARN_BYTE_OBJS)

#--------------------------------------------------
# Load warning data, print statistics

WARNSTATS_OBJS = $(addsuffix .cmx, $(WARNSTATS))

warn_stats.exe: $(WARNSTATS_OBJS)
	$(OCAMLOPT) -o warn_stats.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(WARNSTATS_OBJS)

WARNSTATS_BYTE_OBJS = $(addsuffix .cmo, $(WARNSTATS))

warn_stats_byte: $(WARNSTATS_BYTE_OBJS)
	$(OCAMLC) -o warn_stats_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(WARNSTATS_BYTE_OBJS)


#--------------------------------------------------
# Test the symstate analysis

SYMSTATE_OBJS = $(addsuffix .cmx, $(SYMSTATE))

test_symstate.exe: $(SYMSTATE_OBJS)
	$(OCAMLOPT) -o test_symstate.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(SYMSTATE_OBJS)


SYMSTATE_BYTE_OBJS = $(addsuffix .cmo, $(SYMSTATE))

symstate_byte: $(SYMSTATE_BYTE_OBJS)
	$(OCAMLC) -o symstate_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(SYMSTATE_BYTE_OBJS)

#--------------------------------------------------
# Simple post-analysis, re-analysis + inspection

INSPECT_OBJS = $(addsuffix .cmx, $(TEST_INSPECT))

inspect.exe: $(INSPECT_OBJS)
	$(OCAMLOPT) -o inspect.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(INSPECT_OBJS)


INSPECT_BYTE_OBJS = $(addsuffix .cmo, $(TEST_INSPECT))

inspect_byte: $(INSPECT_BYTE_OBJS)
	$(OCAMLC) -o inspect_byte $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(INSPECT_BYTE_OBJS)



#--------------------------------------------------
# Test memory usage for data structures

TEST_DS_OBJS = $(addsuffix .cmx, $(TEST_DS))

test_ds.exe: $(TEST_DS_OBJS)
	$(OCAMLOPT) -o test_ds.exe $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(TEST_DS_OBJS)

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


#--------------------------------------------------
# Clean up

	
clean:
	rm -f $(TARGETS)
	rm -f *.cm[iox]
	rm -f *.annot
	rm -f *.o
	rm -f *.*~
	rm -f $(DS_DIR)/*.cm[iox]
	rm -f $(DS_DIR)/*.annot
	rm -f $(DS_DIR)/*.o
	rm -f $(DS_DIR)/*.*~
	rm -f $(PTA_DIR)/*.cm[iox]
	rm -f $(PTA_DIR)/*.annot
	rm -f $(PTA_DIR)/*.o
	rm -f $(PTA_DIR)/*.*~


#--------------------------------------------------
# Dependencies
.depend:
	$(OCAMLDEP) $(INCLUDES) $(DS_DIR)/*.mli $(DS_DIR)/*.ml \
	$(PTA_DIR)/*.mli $(PTA_DIR)/*.ml *.mli *.ml > .depend

include .depend


#-------------------------------------------------------
# TODO FIX THIS DOC generation
#

htmldoc:
	$(OCAMLDOC) $(INCLUDES) -d /tmp/ocamldocs -html cilexts/*.ml \
		datastructs/*.ml pta/*.ml *.ml

dot:
	$(OCAMLDOC) $(INCLUDES) -sort -dot cilexts/*.ml datastructs/*.ml *.ml 
