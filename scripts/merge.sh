#!/bin/sh

# Merge the source files according to info in "gcc-log"
# Requires the "mergy" script
# Remember to change CILLYROOT, MERGYROOT, etc.

DUMPROOT=$PWD/cilmerged
CILLYROOT=/home/jan/research/cil_1_3_5/bin
MERGYROOT=/home/jan/research/relay-race/scripts


#gcc-log has "cd" and "duppy" commands on each line
#will interpret "duppy" as "mergy" later...
CMDS=$PWD/gcc-log.txt

LOG=$DUMPROOT/log.txt

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

duppy ()
{
    echo mergy $*
    $MERGYROOT/mergy $*
}

(. $CMDS) #> /dev/null #| tee $LOG 2>&1
