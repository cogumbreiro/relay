#!/bin/sh

# Merge the source files according to info in "stripped-log.txt"

CURROOT=$PWD
RELAYROOT=/home/jan/research/relay-race
CILLYROOT=$RELAYROOT/cil/bin
DUPPYROOT=$RELAYROOT/scripts
LOG=dumplog

#gcc-log has "cd" and "duppy" commands on each line
#will interpret "duppy" as "mergy" later...
CMDS=$PWD/stripped-log.txt

export CURROOT
export CILLYROOT
SKIP_AFTERCIL=1
export SKIP_AFTERCIL
NODEF=$PWD/nodef.h
export NODEF

MYCC ()
{
  echo mergeCC $*
  $CILLYROOT/cilly --noPrintLn --merge --keepmerged $*
#  $CILLYROOT/cilly --noPrintLn --merge --keepmerged --keepunused $*
}

MYLD ()
{
  echo mergeLD $*
  $CILLYROOT/cilly --noPrintLn --merge --keepmerged $*
#  $CILLYROOT/cilly --noPrintLn --merge --keepmerged --keepunused $*
}

MYAR ()
{
  echo mergeAR $*
  $CILLYROOT/cilly --mode="AR" --noPrintLn --merge --keepmerged $*
#  $CILLYROOT/cilly --mode="AR" --noPrintLn --merge --keepmerged --keepunused $*
}


STARTTIME=$(date +%s)

(. $CMDS) | tee $LOG 2>&1

ENDTIME=$(date +%s)
DIFF=$(( $ENDTIME - $STARTTIME ))
echo "merged in $DIFF seconds"

