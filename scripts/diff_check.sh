#!/bin/bash

if [ $# -le 2 ]; then
    echo 1>&2 Usage: $0 patterns outpath1 outpath2 test1 [test2, ...]
    exit 127
fi

patterns=$1
shift
path1=$1
shift
path2=$1
echo diffing output from $path1 and $path2
shift

tocheck=$@ 

echo "Diffing for pattern: $patterns"
echo " in: $tocheck"

rm diff_out

function checkExists {
    if test -f $1 ; then
        echo "Wanted to use $1 as scratch, but it already exists"
        exit 1
    fi
}

scratch1=temp_diff1
scratch2=temp_diff2
checkExists $scratch1
checkExists $scratch2

function grepFor {
    grep "$patterns" $1$2.out
}

function doChecks {
    for f in $@
    do
        echo ">>> " $f >> diff_out
        grepFor $path1 $f | sort > $scratch1
        grepFor $path2 $f | sort > $scratch2
        diff $scratch1 $scratch2 >> diff_out
        rm $scratch1
        rm $scratch2
    done
}

doChecks $tocheck

echo results are in the file diff_out
