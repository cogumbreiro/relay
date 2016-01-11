#!/bin/bash

# "intercept" calls to GNU libtool
# log its dry-run commands (which should reference gcc) 
# and run/convert its calls to gcc into my own tool (duppy)

# 1
# log what make does
make clean
make > make-log

# 2
# strip comments: "Making XYZ in ABC", "...Leaving directory...", "creating "
sed "/Making .* in .*/d" make-log > temp-log
sed "/.*Leaving directory.*/d" temp-log > temp-log2
sed "/creating .*/d" temp-log2 > make-log2

# 3
# convert "XYZ Entering directory `DIR'" to "cd DIR"
sed "s/.*Entering directory \`\(.*\)'/echo 'cd \1'\ncd \1/" make-log2 > make-log3

# 4
# convert ".../apr/libtool --silent ..." into ".../apr/libtool --dry-run"
sed "s/\/libtool --silent/\/libtool --dry-run/" make-log3 > temp-log
sed "s/\/libtool --mode/\/libtool --dry-run --mode/" temp-log > make-log4


# 5 
# "run" the commands in that log and redirect dry-run output to gcc-log
(. make-log4) > make-log5

# 6
# substitute all calls to gcc into calls to duppy... (and that is the gcc-log)
# (don't need to run them... they were already run in pass 1)
sed "/creating .*/d" make-log5 > temp-log
sed "s/gcc/duppy/" temp-log > gcc-log.txt
