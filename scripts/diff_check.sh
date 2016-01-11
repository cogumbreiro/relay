#!/bin/sh

if [ $# -le 2 ]; then
    echo 1>&2 Usage: $0 outpath1 outpath2 test1 [test2, ...]
    exit 127
fi

path1=$1
shift
path2=$1
echo diffing output from $path1 and $path2
shift

tocheck=$@
#tocheck="locktest ptr_tests ptr_tests2 thread_tests plip knot esp ide-disk synclink" 

rm diff_out
for f in $tocheck
do
    echo ">>> " $f >> diff_out
    diff $path1$f.out $path2$f.out | grep "SUMS (\|Total Warnings" >> diff_out
done

echo results are in the file diff_out
