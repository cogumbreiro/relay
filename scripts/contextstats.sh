#!/bin/bash
LOG=$1
./scripts/getfpdonefuncs.sh $LOG | awk '{
if (NR == 1) {
  sum=min=max=$1;
} else {
  sum += $1;
  min = (min < $1) ? min : $1;
  max = (max > $1) ? max : $1;
}
}
END {
   print "Contexts AVG : " sum/NR " MIN: " min " MAX: " max;
} '


