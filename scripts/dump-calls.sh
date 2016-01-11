#!/bin/sh

# Dumps the call graph, and cil ASTs to the DUMPROOT specified below
# Reads the information from "gcc-log"
# Requires the "duppy" script
# Remember to change CILLYROOT, DUPPYROOT, etc.


DUMPROOT=$PWD/ciltrees
RELAYROOT=/home/jan/research/relay-race
DUPPYROOT=$RELAYROOT/scripts
CILLYROOT=$RELAYROOT/cil/bin
LOG=$DUMPROOT/log.txt

#gcc-log has "cd" and "duppy" commands on each line
CMDS=$PWD/gcc-log.txt

/bin/rm -rf $DUMPROOT
mkdir -p $DUMPROOT
/bin/rm -f $LOG

export DUMPROOT
export CILLYROOT
SKIP_AFTERCIL=1
export SKIP_AFTERCIL

NODEF=$PWD/nodef.h
export NODEF

# nodef?

# first dump the files

duppy ()
{
    echo duppy $*
    $DUPPYROOT/duppy $*
}

(. $CMDS) > /dev/null #| tee $LOG 2>&1

# fix variable / struct ids + dump the call graph


cd $RELAYROOT; ./fix_id_cg.exe -cg $DUMPROOT
