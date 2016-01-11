

#--------------------------------------------------
# User-defined constants

CIL := cil
CIL_OBJS := $(CIL)/obj/x86_LINUX
CIL_INCLUDE := -I $(CIL_OBJS)
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

OCAMLC := $(shell if ocamlc.opt > /dev/null 2>&1; then echo 'ocamlc.opt'; else echo 'ocamlc'; fi)
OCAMLOPT := $(shell if ocamlopt.opt > /dev/null 2>&1; then echo 'ocamlopt.opt'; else echo 'ocamlopt'; fi)
OCAMLDEP := $(shell if ocamldep.opt > /dev/null 2>&1; then echo 'ocamldep.opt'; else echo 'ocamldep'; fi)
OCAMLDOC := $(shell if ocamldoc.opt > /dev/null 2>&1; then echo 'ocamldoc.opt'; else echo 'ocamldoc'; fi)



INCLUDES := $(CIL_INCLUDE) -I $(DS_DIR) -I $(PTA_DIR)
TO_LINK := -ccopt -L$(CIL_OBJS) 
OCAMLFLAGS := -thread $(INCLUDES) -g $(TO_LINK) \
	unix.cma str.cma threads.cma statfs_c.o
OCAMLOPTFLAGS := -thread $(INCLUDES) -dtypes $(TO_LINK) \
	unix.cmxa str.cmxa threads.cmxa statfs_c.o -g


#--------------------------------------------------
# Objs/targets

UTILS := gen_num strutil logging mystats gc_stats stdutil statfs \
	config gz_marshal scp inspect timeout size osize

CIL_EXTRAS := exit_funcs pp_visitor scope cil_lvals cilinfos offset \
	type_reach type_utils cast_hierarchy

DATA_STRUCTS := queueset stackset mapset prioqueuemap iset uf \
	intrange hashcons \
	simplehc distributions list_utils graph sparsebitv scc 

PTA := pta_types pta_compile pta_shared pta_cycle pta_offline_cycle \
	pta_fp_test pta_fi_eq pta_fb_eq pta_fs_dir 

BACKING_STORE := backed_summary inout_summary cache_sum

REQUEST := messages distributed file_serv request

FCACHE := cache cilfiles filecache default_cache

GUARDED_ACCS := access_info guarded_access_base guarded_access \
	guarded_access_sep guarded_access_clust

MODSUMS := modsummaryi

ORIG_SYMEX := sym_types symsummary symstate2

NEW_SYMEX := symex_types symex_sum symex

SYMEX := symex_base $(ORIG_SYMEX) $(NEW_SYMEX)

OO_PARTITION := addr_taken global_addr_taken faddr_taken oo_partition

FIELD_PARTITION := field_partition

CALLG := summary_keys callg scc_cg

DIFF_CG = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) alias_types alias \
	$(CALLG) lvals backed_summary diff_fp_cg

DYNAMIC_CG = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	$(CALLG) id_fixer $(CIL_EXTRAS) dynamic_cg

LLVM_CG = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	$(CALLG) id_fixer $(CIL_EXTRAS) llvm_cg

ID_FIX_CG = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) \
	alias_types alias $(CALLG) lvals dot_lib \
	$(OO_PARTITION) $(FIELD_PARTITION) dumpcalls \
	filter_dumpcalls fix_id_cg 

TEST_PTA = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) pta_link_test alias_types \
	alias $(CALLG) lvals dumpcalls test_pta 


RACE = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(CALLG) $(BACKING_STORE) \
	$(PTA) alias_types alias lvals dumpcalls \
	threads entry_points shared \
	checkpoint relative_set lockset $(GUARDED_ACCS) \
	warn_reports race_reports \
	$(REQUEST) safer_sum $(MODSUMS) racesummary analysis_dep interDataflow \
	manage_sums intraDataflow $(SYMEX) racestate \
	roots race_warnings thread_needed_funcs race_anal 

DIFF_RACES = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) alias_types alias \
	$(CALLG) lvals threads shared relative_set lockset $(GUARDED_ACCS) \
	warn_reports race_reports diff_races

SERVER_SOCKET = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CALLG) $(CIL_EXTRAS) $(BACKING_STORE) $(PTA) alias_types \
	alias dumpcalls lvals threads entry_points shared relative_set lockset \
	$(GUARDED_ACCS) warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum $(MODSUMS) racesummary \
	server_socket

CG_DOT = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CALLG) $(CIL_EXTRAS) $(BACKING_STORE) \
	$(PTA) alias_types alias lvals \
	threads entry_points dot_lib cg_to_dot 

CAST_GRAPH_DOT = $(UTILS) $(DATA_STRUCTS) $(FCACHE) id_fixer $(CIL_EXTRAS) \
	test_cast_graph 

DEBUG_BLOCKED = $(UTILS) $(DATA_STRUCTS) $(FCACHE) id_fixer $(CIL_EXTRAS) \
	fstructs $(CALLG) debug_blocked

SCC_STATS = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CALLG) $(BACKING_STORE) \
	distributed $(CIL_EXTRAS) $(PTA) \
	alias_types alias lvals threads entry_points scc_plot scc_stats

SCC_COMPARE = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CALLG) $(BACKING_STORE) \
	distributed $(CIL_EXTRAS) $(PTA) \
	alias_types alias lvals \
	threads entry_points scc_plot scc_compare

INSTR_STATS = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) alias_types alias $(CALLG) lvals dumpcalls \
	threads entry_points shared relative_set lockset malloc_stats \
	dot_lib obj_stats basic_line_count \
	$(OO_PARTITION) $(FIELD_PARTITION) instr_stats

CFIELD_BUG = $(UTILS) $(DATA_STRUCTS) $(FCACHE) id_fixer $(CIL_EXTRAS) \
	cfields_bug

LOCK_STATS = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) alias_types alias $(CALLG) lvals \
	dumpcalls scc scc_cg \
	threads entry_points shared relative_set lockset \
	test_lock_kernel

TEST_MEMUSAGE = $(UTILS) test_memusage

TEST_PP_UNIQUENESS = $(UTILS) test_pp_uniqueness

PRINTSUMM = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CALLG) $(CIL_EXTRAS) \
	$(PTA) alias_types alias  $(BACKING_STORE) dumpcalls \
	lvals relative_set lockset threads entry_points \
	shared $(GUARDED_ACCS) warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum $(MODSUMS) racesummary \
	analysis_dep interDataflow manage_sums intraDataflow \
	$(SYMEX) relative_df racestate \
	lockset_partitioner pseudo_access \
	all_unlocks knowledge_pass rns null_warnings \
	print_summary

PRINTCIL = print_cil

PRINTWARN = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) \
	alias_types alias $(CALLG) lvals dumpcalls \
	threads entry_points \
	checkpoint relative_set lockset \
	$(BACKING_STORE) shared $(GUARDED_ACCS) warn_reports race_reports \
	$(REQUEST) safer_sum $(MODSUMS) racesummary \
	analysis_dep interDataflow manage_sums intraDataflow $(SYMEX) racestate \
	roots race_warnings print_warnings

WARNSTATS = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(PTA) alias_types alias \
	$(CALLG) lvals $(BACKING_STORE) threads shared \
	relative_set lockset $(GUARDED_ACCS) warn_reports race_reports $(REQUEST) \
	checkpoint safer_sum $(MODSUMS) racesummary warn_stats 

# figure out which ones are actually needed!
DEREFSTATS = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(CALLG) $(BACKING_STORE) \
	$(PTA) alias_types alias lvals dumpcalls threads entry_points shared \
	relative_set lockset $(GUARDED_ACCS) warn_reports race_reports \
	$(REQUEST) checkpoint safer_sum $(MODSUMS) racesummary \
	analysis_dep interDataflow manage_sums intraDataflow \
	$(SYMEX) relative_df all_unlocks lockset_partitioner pseudo_access \
	knowledge_pass rns null_warnings dereference_stats

# figure out which ones are actually needed!
PRINTDELTA = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(CALLG) $(BACKING_STORE) \
	$(PTA) alias_types alias lvals dumpcalls threads entry_points shared \
	relative_set lockset $(GUARDED_ACCS) \
	warn_reports race_reports \
	$(REQUEST) checkpoint safer_sum $(MODSUMS) racesummary \
	analysis_dep interDataflow manage_sums intraDataflow \
	$(SYMEX) relative_df all_unlocks lockset_partitioner pseudo_access \
	knowledge_pass rns null_warnings print_delta


SYMSTATE = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CALLG) $(CIL_EXTRAS) $(BACKING_STORE) \
	$(PTA) alias_types alias lvals dumpcalls \
	threads entry_points checkpoint shared \
	relative_set lockset $(GUARDED_ACCS) \
	warn_reports race_reports $(REQUEST) \
	manage_sums safer_sum $(MODSUMS) racesummary \
	analysis_dep interDataflow intraDataflow \
	$(SYMEX) test_symstate

TEST_DS = $(UTILS) $(DATA_STRUCTS) test_datastructs

TEST_INSPECT = $(UTILS) $(DATA_STRUCTS) $(FCACHE) fstructs \
	id_fixer $(CIL_EXTRAS) $(CALLG) $(BACKING_STORE) \
	$(PTA) alias_types alias lvals dumpcalls threads entry_points shared \
	relative_set lockset $(GUARDED_ACCS) warn_reports race_reports \
	checkpoint $(REQUEST) safer_sum $(MODSUMS) racesummary  \
	manage_sums analysis_dep interDataflow intraDataflow \
	$(SYMEX) racestate test_inspect 


BIN_TARGETS = fix_id_cg.exe race_anal.exe printwarn.exe printsumm.exe \
	scc_stats.exe scc_compare.exe test_memusage.exe test_ds.exe \
	test_symstate.exe cg_to_dot.exe print_cil.exe \
	test_pta.exe server.exe warn_stats.exe instr_stats.exe deref_stats.exe \
	print_delta.exe inspect.exe cast_graph.exe lock_stats.exe	cfields_bug.exe \
	diff_fp_cg.exe dynamic_cg.exe llvm_cg.exe diff_races.exe

BYTE_TARGETS = fix_id_cg_byte race_byte printwarn_byte printsumm_byte \
	merge_byte runtest_byte symstate_byte cg_to_dot_byte \
	test_pta_byte server_byte warn_stats_byte instr_stats_byte \
	inspect_byte print_cil_byte \
	race_temp_byte \
	cast_graph_byte cfields_bug_byte

ALL_TARGETS = .depend $(BIN_TARGETS) $(BYTE_TARGETS)

.PHONY: all clean htmldoc dot

all: .depend $(BIN_TARGETS)



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
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^


ID_FIX_CG_BYTE = $(addsuffix .cmo, $(ID_FIX_CG))

fix_id_cg_byte: $(ID_FIX_CG_BYTE)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $^


#--------------------------------------------------
# Fix ids + dump the call graph

TEST_PTA_OBJS = $(addsuffix .cmx, $(TEST_PTA))

test_pta.exe: $(TEST_PTA_OBJS)
	$(OCAMLOPT) -ccopt -static -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^


TEST_PTA_BYTE = $(addsuffix .cmo, $(TEST_PTA))

test_pta_byte: $(TEST_PTA_BYTE)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^


#--------------------------------------------------
# Main analysis

# The list of object files for the byte code version
RACE_BYTE_OBJS = $(addsuffix .cmo, $(RACE))

race_byte: $(RACE_BYTE_OBJS)
	$(OCAMLC) -ccopt -static -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $(RACE_BYTE_OBJS)

# The list of object files for the native version
RACE_NATIVE_OBJS = $(addsuffix .cmx, $(RACE))

race_anal.exe: $(RACE_NATIVE_OBJS)
	$(OCAMLOPT) -ccopt -static -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $(RACE_NATIVE_OBJS)


#--------------------------------------------------
# Diff races

DIFF_RACES_NATIVE_OBJS = $(addsuffix .cmx, $(DIFF_RACES))

diff_races.exe: $(DIFF_RACES_NATIVE_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

DIFF_RACES_BYTE_OBJS = $(addsuffix .cmo, $(DIFF_RACES))

diff_races_byte: $(DIFF_RACES_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $^

#--------------------------------------------------
# Server for distributed mode (using sockets)

# The list of object files for the byte code version
SERVER_SOCKET_BYTE_OBJS = $(addsuffix .cmo, $(SERVER_SOCKET))

server_byte: $(SERVER_SOCKET_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^

# The list of object files for the native version
SERVER_SOCKET_NATIVE_OBJS = $(addsuffix .cmx, $(SERVER_SOCKET))

server.exe: $(SERVER_SOCKET_NATIVE_OBJS)
	$(OCAMLOPT) -ccopt -static -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^


#--------------------------------------------------
# Convert cg to dot graphs

CG_DOT_OBJS = $(addsuffix .cmx, $(CG_DOT))

cg_to_dot.exe: $(CG_DOT_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

CG_DOT_BYTE_OBJS = $(addsuffix .cmo, $(CG_DOT))

cg_to_dot_byte: $(CG_DOT_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^


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
# Debug blocked functions in top-down analysis

DEBUG_BLOCKED_OBJS = $(addsuffix .cmx, $(DEBUG_BLOCKED))

debug_blocked.exe: $(DEBUG_BLOCKED_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

DEBUG_BLOCKED_BYTE_OBJS = $(addsuffix .cmo, $(DEBUG_BLOCKED))

debug_blocked: $(DEBUG_BLOCKED_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $^


#--------------------------------------------------
# Scc stat printer

SCC_STATS_OBJS = $(addsuffix .cmx, $(SCC_STATS))

scc_stats.exe: $(SCC_STATS_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

#--------------------------------------------------
# Scc comparison printer

SCC_COMPARE_OBJS = $(addsuffix .cmx, $(SCC_COMPARE))

scc_compare.exe: $(SCC_COMPARE_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

#--------------------------------------------------
# Instr stat printer

INSTR_STATS_OBJS = $(addsuffix .cmx, $(INSTR_STATS))

instr_stats.exe: $(INSTR_STATS_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

INSTR_STATS_BYTE_OBJS = $(addsuffix .cmo, $(INSTR_STATS))

instr_stats_byte: $(INSTR_STATS_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $^

#--------------------------------------------------
# Cfields bug checker

CFIELD_BUG_OBJS = $(addsuffix .cmx, $(CFIELD_BUG))

cfields_bug.exe: $(CFIELD_BUG_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

CFIELD_BUG_BYTE_OBJS = $(addsuffix .cmo, $(CFIELD_BUG))

cfields_bug_byte: $(CFIELD_BUG_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) \
	$(CIL_OBJS)/cil.cma $^


#--------------------------------------------------
# Lock_kernel stats

LOCK_STATS_OBJS = $(addsuffix .cmx, $(LOCK_STATS))

lock_stats.exe: $(LOCK_STATS_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

#--------------------------------------------------
# Load / Run some analyses test

TEST_MEMUSAGE_OBJS = $(addsuffix .cmx, $(TEST_MEMUSAGE))

test_memusage.exe: $(TEST_MEMUSAGE_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

TEST_MEMUSAGE_BYTE_OBJS = $(addsuffix .cmo, $(TEST_MEMUSAGE))

runtest_byte: $(TEST_MEMUSAGE_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^

#--------------------------------------------------
# Load / Echo a summary

PRINTSUMM_OBJS = $(addsuffix .cmx, $(PRINTSUMM))

printsumm.exe: $(PRINTSUMM_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

PRINTSUMM_BYTE_OBJS = $(addsuffix .cmo, $(PRINTSUMM))

printsumm_byte: $(PRINTSUMM_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^

#--------------------------------------------------
# Load and print a CIL ast

PRINT_CIL_OBJS = $(addsuffix .cmx, $(PRINTCIL))

print_cil.exe: $(PRINT_CIL_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

PRINT_CIL_BYTE_OBJS = $(addsuffix .cmo, $(PRINTCIL))

print_cil_byte: $(PRINT_CIL_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^

#--------------------------------------------------
# Load summaries, print race_warnings

PRINTWARN_OBJS = $(addsuffix .cmx, $(PRINTWARN))

printwarn.exe: $(PRINTWARN_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

PRINTWARN_BYTE_OBJS = $(addsuffix .cmo, $(PRINTWARN))

printwarn_byte: $(PRINTWARN_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^

#--------------------------------------------------
# Load warning data, print statistics

WARNSTATS_OBJS = $(addsuffix .cmx, $(WARNSTATS))

warn_stats.exe: $(WARNSTATS_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

WARNSTATS_BYTE_OBJS = $(addsuffix .cmo, $(WARNSTATS))

warn_stats_byte: $(WARNSTATS_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^

#
DEREFSTATS_OBJS = $(addsuffix .cmx, $(DEREFSTATS))

deref_stats.exe: $(DEREFSTATS_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

#
PRINTDELTA_OBJS = $(addsuffix .cmx, $(PRINTDELTA))

print_delta.exe: $(PRINTDELTA_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^


#--------------------------------------------------
# Test the symstate analysis

SYMSTATE_OBJS = $(addsuffix .cmx, $(SYMSTATE))

test_symstate.exe: $(SYMSTATE_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

SYMSTATE_BYTE_OBJS = $(addsuffix .cmo, $(SYMSTATE))

symstate_byte: $(SYMSTATE_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^

#--------------------------------------------------

DIFF_CG_OBJS = $(addsuffix .cmx, $(DIFF_CG))

diff_fp_cg.exe: $(DIFF_CG_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

#--------------------------------------------------

DYNAMIC_CG_OBJS = $(addsuffix .cmx, $(DYNAMIC_CG))

dynamic_cg.exe: $(DYNAMIC_CG_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

#--------------------------------------------------

LLVM_CG_OBJS = $(addsuffix .cmx, $(LLVM_CG))

llvm_cg.exe: $(LLVM_CG_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^

LLVM_CG_BYTE_OBJS = $(addsuffix .cmo, $(LLVM_CG))

llvm_cg_byte: $(LLVM_CG_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^

#--------------------------------------------------
# Simple post-analysis, re-analysis + inspection

INSPECT_OBJS = $(addsuffix .cmx, $(TEST_INSPECT))

inspect.exe: $(INSPECT_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^


INSPECT_BYTE_OBJS = $(addsuffix .cmo, $(TEST_INSPECT))

inspect_byte: $(INSPECT_BYTE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS)  \
	$(CIL_OBJS)/cil.cma $^



#--------------------------------------------------
# Test memory usage for data structures

TEST_DS_OBJS = $(addsuffix .cmx, $(TEST_DS))

test_ds.exe: $(TEST_DS_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) \
	$(CIL_OBJS)/cil.cmxa $^



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
	rm -f $(ALL_TARGETS)
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

# TODO: have Makefiles in subdirs that just do this...


#--------------------------------------------------
# Dependencies
.depend:
	$(OCAMLDEP) $(INCLUDES) $(DS_DIR)/*.mli $(DS_DIR)/*.ml \
	$(PTA_DIR)/*.mli $(PTA_DIR)/*.ml \
	*.mli *.ml > .depend

include .depend


#-------------------------------------------------------
# TODO FIX THIS DOC generation
#

htmldoc:
	$(OCAMLDOC) $(INCLUDES) -d /tmp/ocamldocs -html cilexts/*.ml \
		datastructs/*.ml pta/*.ml *.ml

dot:
	$(OCAMLDOC) $(INCLUDES) -sort -dot cilexts/*.ml datastructs/*.ml *.ml 
