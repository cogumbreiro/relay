#!/bin/bash

# Prune duppy calls that would have compiled files w/ other "main" functions

other_mains="htcacheclean.c htdbm.c checkgid.c ab.c logresolve.c rotatelogs.c htdigest.c htpasswd.c gen_test_char.c dftables.c"

LOG="gcc-log.txt"
OUTLOG="gcc-log2.txt"
TO_DELETE="httxt2dbm.c" # explicitly list the first guy

for filename in $other_mains; do
    TO_DELETE=$TO_DELETE"\|"$filename
done

echo "Will remove matches to: $TO_DELETE"

sed "/$TO_DELETE/d" $LOG > $OUTLOG