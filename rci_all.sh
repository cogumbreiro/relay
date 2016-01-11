#!/bin/bash

EXE=fp_rci.exe
#EXE=fp_rci_byte
CALLS_FILE=calls.fp_rci
CALLS_NOMERGE=calls.oic
SCRIPTS_DIR=/home/jan/research/relay-race/scripts
CG_DIR=$1
LOG=$CG_DIR/log
LOGNOMERGE=$CG_DIR/lognomerge
PWD=`pwd`

#date > $LOG
#echo "incrementing generation number"
#$SCRIPTS_DIR/next_num.py gen_num.txt
#echo "running funptr analysis w/ option $@ AND MERGE" | tee --append $LOG
#nice -n 15 ./$EXE -r -u jan -cg $@ -o $CALLS_FILE 2>&1 | tee --append $LOG
#nice -n 15 ./scc_stats.exe -cg $CG_DIR/$CALLS_FILE 2>&1 | tee --append $LOG

#date >> $LOG
date > $LOGNOMERGE

echo "incrementing generation number"
$SCRIPTS_DIR/next_num.py gen_num.txt
echo "running funptr analysis w/ option $@ AND NO MERGE" | tee --append $LOGNOMERGE
nice -n 15 ./$EXE -r -u jan -cg $@ -o $CALLS_NOMERGE -recm 1 2>&1 | tee --append $LOGNOMERGE
nice -n 15 ./scc_stats.exe -cg $CG_DIR/$CALLS_NOMERGE 2>&1 | tee --append $LOGNOMERGE

date >> $LOGNOMERGE

echo "generating scc plot" | tee --append $LOG
./scc_compare.exe -cg $CG_DIR/ -o $1/ 2>&1 | tee --append $LOG
(cd $1; $SCRIPTS_DIR/plot.gp)

echo `pwd`
date >> $LOG
$SCRIPTS_DIR/contextstats.sh $LOG > $CG_DIR/log.cstats
$SCRIPTS_DIR/contextstats.sh $LOGNOMERGE > $CG_DIR/logno.cstats

tar -czvf $CG_DIR/results.tar.gz $CG_DIR/calls.* $CG_DIR/*.dat $CG_DIR/log* $CG_DIR/*.ps

