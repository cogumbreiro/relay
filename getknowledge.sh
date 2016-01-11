#!/bin/sh

echo "\n === RACE INFO\n"

grep "Total Warnings" $1
grep "Gamma Report" $1
grep -A2 "Delta Report" $1 #| head -n 3

echo "\n === KNOWLEDGE COUNTS\n"

./getknowledge_counts.sh $1

#echo "\n === UNCLUSTERED COUNTS OF SAFE/UNSAFE DEREF CHECKS\n"
#
#grep -A5 "Deref Report" $1

echo "\n === CLUSTERED COUNTS OF SAFE/UNSAFE DEREF CHECKS\n"

./deref_stats.exe -caption SEQ \
                  -safe $2/safe_derefs_seq.dat \
                  -unsafe $2/unsafe_derefs_seq.dat 

./deref_stats.exe -caption RADAR \
                  -safe $2/safe_derefs_adj_l.dat \
                  -unsafe $2/unsafe_derefs_adj_l.dat

./deref_stats.exe -caption RADAR-NL \
                  -safe $2/safe_derefs_adj_nl.dat \
                  -unsafe $2/unsafe_derefs_adj_nl.dat

./deref_stats.exe -caption STEENS \
                  -safe $2/safe_derefs_pess.dat \
                  -unsafe $2/unsafe_derefs_pess.dat

echo ""

