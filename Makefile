

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

OCAMLC := $(shell if ocamlc.opt > /dev/null 2>&1; then echo 'ocamlc.opt'; else echo 'ocamlc'; fi)
OCAMLOPT := $(shell if ocamlopt.opt > /dev/null 2>&1; then echo 'ocamlopt.opt'; else echo 'ocamlopt'; fi)
OCAMLDEP := $(shell if ocamldep.opt > /dev/null 2>&1; then echo 'ocamldep.opt'; else echo 'ocamldep'; fi)
OCAMLDOC := $(shell if ocamldoc.opt > /dev/null 2>&1; then echo 'ocamldoc.opt'; else echo 'ocamldoc'; fi)



INCLUDES := $(CIL_INCLUDE) -I $(DS_DIR) -I $(PTA_DIR)
TO_LINK := -ccopt -L$(CIL_OBJS) 
OCAMLFLAGS := -thread $(INCLUDES) -g $(TO_LINK) \
	unix.cma str.cma threads.cma statfs_c.o
OCAMLOPTFLAGS := -thread $(INCLUDES) -dtypes $(TO_LINK) \
	unix.cmxa str.cmxa threads.cmxa statfs_c.o 


#--------------------------------------------------
# Objs/targets

UTILS := gen_num strutil logging mystats gc_stats stdutil statfs \
	config gz_marshal scp inspect timeout size osize

CIL_EXTRAS := pp_visitor cil_lvals cilinfos offset cast_hierarchy

DATA_STRUCTS := queueset stackset mapset iset uf intrange hashcons hset hmap \
	simplehc distributions graph

PTA := pta_types pta_compile pta_fp_test pta_fi_eq pta_fb_eq pta_fs_dir 

BACKING_STORE := backed_summary

REQUEST := messages distributed file_serv request

FCACHE := cache cilfiles filecache default_cache

GUARDED_ACCS := access_info guarded_access_base guarded_access \
	guarded_access_sep guarded_access_clust

MODSUMS := modsummaryi

ORIG_SYMEX := sym_types symsummary symstate2

NEW_SYMEX := symex_types symex_sum symex

SYMEX := symex_base $(ORIG_SYMEX) $(NEW_SYMEX)

ID_FIX_CG = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) \
	alias_types alias scope lvals \
	callg dumpcalls readcalls fix_id_cg 

TEST_PTA = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) pta_link_test alias_types \
	alias scope lvals callg dumpcalls readcalls test_pta 

RACE = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(BACKING_STORE) \
	$(PTA) alias_types alias scope lvals callg dumpcalls readcalls \
	scc scc_cg threads entry_points shared \
	checkpoint relative_set lockset $(GUARDED_ACCS) \
	warn_reports race_reports \
	$(REQUEST) safer_sum $(MODSUMS) racesummary analysis_dep interDataflow \
	manage_sums intraDataflow knowledge_pass $(SYMEX) modsummary racestate \
	roots race_warnings race_anal 


PSEUDO_FILTER = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(BACKING_STORE) \
	$(PTA) alias_types alias scope lvals callg dumpcalls readcalls \
	scc scc_cg threads entry_points shared \
	relative_set lockset $(GUARDED_ACCS) warn_reports \
	race_reports \
	$(REQUEST) checkpoint safer_sum $(MODSUMS) racesummary \
	analysis_dep interDataflow manage_sums intraDataflow \
	$(SYMEX) relative_df racestate roots \
	all_unlocks lockset_partitioner pseudo_access race_warnings2 \
	knowledge_pass rns null_warnings nullstate2 pseudo_filter


SERVER_SOCKET = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	callg id_fixer $(CIL_EXTRAS) \
	$(BACKING_STORE) $(PTA) alias_types \
	alias scope lvals dumpcalls readcalls scc scc_cg \
	threads entry_points shared relative_set lockset \
	$(GUARDED_ACCS) warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum $(MODSUMS) racesummary \
	server_socket

CG_DOT = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) alias_types alias scope lvals \
	callg dumpcalls readcalls scc scc_cg threads entry_points cg_to_dot 

CAST_GRAPH_DOT = $(UTILS) $(DATA_STRUCTS) $(FCACHE) id_fixer $(CIL_EXTRAS) \
	test_cast_graph 

SCC_STATS = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer distributed $(CIL_EXTRAS) $(PTA) alias_types alias scope lvals \
	callg dumpcalls readcalls scc scc_cg threads entry_points scc_stats

INSTR_STATS = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) alias_types alias scope lvals \
	callg dumpcalls readcalls \
	scc scc_cg \
	threads entry_points shared relative_set lockset \
	instr_stats

MERGE = $(UTILS) merge_sources

TEST_PP_UNIQUENESS = $(UTILS) test_pp_uniqueness

PRINTSUMM = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(BACKING_STORE) $(CIL_EXTRAS) \
	$(PTA) alias_types alias scope lvals relative_set lockset \
	scc callg dumpcalls readcalls scc_cg threads entry_points \
	shared $(GUARDED_ACCS) warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum $(MODSUMS) racesummary \
	analysis_dep interDataflow manage_sums intraDataflow \
	$(SYMEX) relative_df racestate \
	lockset_partitioner pseudo_access \
	all_unlocks print_summary

PRINTCIL = print_cil

PRINTWARN = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) \
	alias_types alias scope lvals callg dumpcalls readcalls \
	scc scc_cg threads entry_points \
	checkpoint relative_set lockset \
	$(BACKING_STORE) shared $(GUARDED_ACCS) warn_reports race_reports \
	$(REQUEST) safer_sum $(MODSUMS) racesummary \
	analysis_dep interDataflow manage_sums intraDataflow $(SYMEX) racestate \
	roots race_warnings print_warnings

WARNSTATS = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) alias_types alias \
	scope lvals $(BACKING_STORE) threads shared \
	relative_set lockset $(GUARDED_ACCS) warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum $(MODSUMS) racesummary warn_stats 


TEST_INSPECT = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(BACKING_STORE) \
	$(PTA) alias_types alias scope lvals callg dumpcalls readcalls \
	scc scc_cg threads entry_points shared \
	relative_set lockset $(GUARDED_ACCS) warn_reports race_reports \
	checkpoint $(REQUEST) safer_sum $(MODSUMS) racesummary  \
	manage_sums analysis_dep interDataflow intraDataflow \
	$(SYMEX) racestate test_inspect 


BIN_TARGETS = fix_id_cg.exe race_anal.exe printwarn.exe \
	printsumm.exe scc_stats.exe merge_src.exe \
	test_symstate.exe cg_to_dot.exe print_cil.exe \
	test_pta.exe server.exe warn_stats.exe instr_stats.exe \
	inspect.exe cast_graph.exe

BYTE_TARGETS = fix_id_cg_byte race_byte printwarn_byte printsumm_byte \
	merge_byte symstate_byte cg_to_dot_byte \
	test_pta_byte server_byte warn_stats_byte \
	inspect_byte print_cil_byte cast_graph_byte


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
	$(OCAMLC) -o fix_id_cg_byte $(OCAMLFLAGS) \
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
	$(OCAMLC) -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $(RACE_BYTE_OBJS)

# The list of object files for the native version
RACE_NATIVE_OBJS = $(addsuffix .cmx, $(RACE))

race_anal.exe: $(RACE_NATIVE_OBJS)
	$(OCAMLOPT) -ccopt -static -o $@ $(OCAMLOPTFLAGS) \
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
	$(OCAMLOPT) -ccopt -static -o server.exe $(OCAMLOPTFLAGS) \
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
# Graph out type casting occurrences

CAST_OBJS = $(addsuffix .cmx, $(CAST_GRAPH_DOT))

cast_graph.exe: $(CAST_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

CAST_BYTE_OBJS = $(addsuffix .cmo, $(CAST_GRAPH_DOT))

cast_graph_byte: $(CAST_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $^

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
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(MERGE_OBJS)


MERGE_BYTE_OBJS = $(addsuffix .cmo, $(MERGE))

merge_byte: $(MERGE_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $^


#--------------------------------------------------
# Load / Echo a summary

PRINTSUMM_OBJS = $(addsuffix .cmx, $(PRINTSUMM))

printsumm.exe: $(PRINTSUMM_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(PRINTSUMM_OBJS)

PRINTSUMM_BYTE_OBJS = $(addsuffix .cmo, $(PRINTSUMM))

printsumm_byte: $(PRINTSUMM_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(PRINTSUMM_BYTE_OBJS)

#--------------------------------------------------
# Load and print a CIL ast

PRINT_CIL_OBJS = $(addsuffix .cmx, $(PRINTCIL))

print_cil.exe: $(PRINT_CIL_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(PRINT_CIL_OBJS)

PRINT_CIL_BYTE_OBJS = $(addsuffix .cmo, $(PRINTCIL))

print_cil_byte: $(PRINT_CIL_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $(PRINT_CIL_BYTE_OBJS)

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
