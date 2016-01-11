#!/bin/bash
if [ $# -ne "1" ]; then
   echo 1>&2 "Usage: $0 srcfile"
   exit 127
fi

FILE=$1
DIR_OF_FILE=`dirname $FILE.c`
START=`cd $DIR_OF_FILE; pwd`
echo "START: $START"
LLVM_DUMP=/home/jan/research/llvm26/print_indir_calls.sh
SUFFIX=llvm_dsa
RELAY=/home/jan/research/relay-race
LLVM_CG=$RELAY/llvm_cg.exe
STARTTIME=$(date +%s)

function check_death {
  if [ $1 -ne 0 ]; then
    echo "Previous pass died"
    exit $1
  fi
}


echo "LLVM-GCC'ing $FILE.c"
llvm-gcc -emit-llvm $FILE.c -c -g -o $FILE.bc
check_death $?

$LLVM_DUMP $FILE.bc 2>&1 | tee $START/log.llvm
check_death $PIPESTATUS

ENDTIME=$(date +%s)
DIFF=$(( $ENDTIME - $STARTTIME ))
echo "TOTAL TIME: $DIFF (s)" >> $START/log.llvm

# now generate the callgraph
(cd $RELAY; $LLVM_CG -cg $START/ciltrees -i $START/log.llvm -o calls.$SUFFIX 2>&1 | tee $START/ciltrees/log.txt.$SUFFIX)
check_death $PIPESTATUS

(cd $RELAY; $RELAY/scc_stats.exe -cg $START/ciltrees/calls.$SUFFIX >> $START/ciltrees/log.txt.$SUFFIX)
check_death $?
