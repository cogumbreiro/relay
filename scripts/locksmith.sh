#!/bin/sh

# Runs locksmith on a merged file
# remember to change path variables

LSROOT=/home/jan/research/locksmith-0.4
PWD=`pwd`
OUTFILE=$PWD/ls-log.txt

$LSROOT/cil/bin/cilly --list-shared --list-guardedby --debug-locksmith $* > $OUTFILE 2>&1
grep "Warning: Possible" $OUTFILE | wc -l 
