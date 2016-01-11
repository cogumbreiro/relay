#!/bin/sh

#---
# Tags all source files in a directory w/ the LICENSE. Checks if it may
# have already been tagged before doing so. 
# TODO: have a user-supplied extension (don't hardcode *.ml)
# also, having some sort of ignore list would be nice...
#---

if [ $# -ne "2" ]; then
    echo 1>&2 "Tag *.ml source files with LICENSE
	Usage: $0 license src_dir"
    exit 127
fi

LICENSE=$1
SRCPATH=$2

# set up the check for a previous LICENSE tag
LICENSE_LEN=`wc -l LICENSE | awk '{print($1)}'`
MAX_DIFF=`expr $LICENSE_LEN / 2`

echo "License is $LICENSE_LEN lines long"
echo "Allowing diff of: $MAX_DIFF"
echo "\n"

for srcfile in `ls $SRCPATH/*.ml`; do
    echo "Checking if $srcfile is tagged with license"
    DIFFLEN=`head $srcfile -n $LICENSE_LEN | diff LICENSE - | wc -l`
    echo "diff of head is $DIFFLEN lines long"

    if [ $DIFFLEN -gt $LICENSE_LEN ]; then
        cat $LICENSE $srcfile > $srcfile.tag
        mv $srcfile.tag $srcfile
    else
        echo "Already tagged with $LICENSE, skipping"
    fi
    echo "\n"
done

echo "==============="
echo "DONE!"
