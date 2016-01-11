#!/bin/sh

# recover parts of relay's _fun_done_dir from directory listings 

if [ $# -ne "3" ]; then
    echo 1>&2 "Searches directory for completed functions
	Usage: $0 src_abs_dirname src_ip dest_dir"
    exit 127
fi



for fname in `ls $1`
do
  echo "found $fname"
  stripped=`echo "$fname" | sed "s/\..*//"`
  echo "stripped $stripped"
  echo "$2\$$1" > $3/f_$stripped
done