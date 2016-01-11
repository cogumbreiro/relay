#!/bin/sh

# Intercepts GCC calls from a makefile -- remember to change LOCKROOT
# Example usage: "intercept.sh make -e"

LOCKROOT=/home/jan/research/relay-race

CC=$LOCKROOT/scripts/mycc.pl
GCC=$LOCKROOT/scripts/mycc.pl
LD=$LOCKROOT/scripts/myld.pl
AR=$LOCKROOT/scripts/myar.pl

LOGFILE=$PWD/gcc-log.txt


/bin/rm -f $LOGFILE
export CC
export GCC
export AR
export LD
export LOGFILE

$*

# make a file that my merge script understands too 
sed "s/duppy/MYCC/" $LOGFILE > $PWD/stripped-log.txt
