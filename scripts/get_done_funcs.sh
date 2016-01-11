#!/bin/sh
if [ $# -ne "1" ]; then
   echo 1>&2 "Searches log for completed funcs.
	Usage: $0 logfile"
   exit 127
fi

# find a file that doesn't exist, to direct the output
i=0
while [ -f done_funcs$i ]
  do
    i=`expr $i + 1`
  done

# Search through the log (assuming all summaries are in a fixed directory..)
sed -n '
s/Summary for function: \([_a-zA-Z]*\) : \(.*\)/\1$\2$\\home\\jan\\research\\relay-race\\scc_sum/p' < $1 > done_funcs$i
 
echo output is in file done_funcs$i
