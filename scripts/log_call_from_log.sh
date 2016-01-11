#!/bin/sh

# Replay commands from "stripped-log.txt" but run through "cilly --dologcalls"

CURROOT=$PWD
RELAYROOT=/home/jan/research/relay-race
CILLYROOT=$RELAYROOT/cil/bin

#gcc-log has "cd" and "duppy" commands on each line
#will interpret "duppy" as "mergy" later...
CMDS=$PWD/stripped-log.txt

export CURROOT
export CILLYROOT
SKIP_AFTERCIL=1
export SKIP_AFTERCIL
NODEF=$PWD/nodef.h
export NODEF

LOGCOMMAND="--dologcalls2"

MYCC ()
{
  echo mergeCC $*
  $CILLYROOT/cilly $LOGCOMMAND $*
}

MYLD ()
{
  echo mergeLD $*
  $CILLYROOT/cilly $LOGCOMMAND $*
}

MYAR ()
{
  echo mergeAR $*
  $CILLYROOT/cilly --mode="AR" $LOGCOMMAND $*
}

(. $CMDS) | tee $LOG 2>&1

