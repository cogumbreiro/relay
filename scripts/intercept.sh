#!/bin/sh

# Intercepts GCC calls from a makefile -- remember to change LOCKROOT
# Example usage: "intercept.sh make -e"

LOCKROOT=/home/jan/research/relay-race

CC=$LOCKROOT/scripts/mycc.pl
GCC=$LOCKROOT/scripts/mycc.pl
LOGFILE=$PWD/gcc-log.txt


/bin/rm -f $LOGFILE
export CC
export GCC
export LOGFILE

$*
