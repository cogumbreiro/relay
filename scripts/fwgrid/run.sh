#!/bin/sh

if [ $# -lt "3" ]; then
   echo 1>&2 "Run qsub on scripts that require callgraph file
        Usage: $0 script num_cores cg_file args"
   exit 127
fi

script=$1
shift
num_procs=$1
shift
for (( i = 1; i <= $num_procs; i++ ))
do
   qsub $script $@
   if [ $? -ne "0" ]; then
      echo "only ran $i processes"
      exit 0
   fi 
done
