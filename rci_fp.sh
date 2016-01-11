#!/bin/bash

EXE=fp_rci.exe
#EXE=fp_rci_byte
CALLS_FILE=calls.oic
SCRIPTS_DIR=/home/jan/research/relay-race/scripts

date > log
echo "incrementing generation number"
$SCRIPTS_DIR/next_num.py gen_num.txt
echo "running funptr analysis w/ option $@"
./$EXE -r -u jan -cg $@ -o $CALLS_FILE 2>&1 | tee --append log
./scc_stats.exe -cg $1/$CALLS_FILE 2>&1 | tee --append log

echo "generating scc plot"
./scc_compare.exe -cg $1/ -o $1/ 2>&1 | tee --append log
cd $1; $SCRIPTS_DIR/plot.gp

date >> log
