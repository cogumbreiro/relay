#!/bin/sh

# Runs locksmith on a merged file
# remember to change path variables


LSROOT=/home/jan/research/locksmith

OUT=$PWD/ls-log.txt

$LSROOT/cil/bin/cilly --list-shared --list-guardedby $* 2> $OUT