#!/bin/bash

TO_SPLIT=`ls`
INDICES="10 20 30 40 50 60 70 80 90"
COUNTER="0"
TARGET="../calllogs2/_log"

for LOG in $TO_SPLIT; do
  for IND in $INDICES; do
    let COUNTER=$[COUNTER + 1]
    grep "$IND :" $LOG | cut -d ":" -f 2,3,4 | sed 's/^ //' > $TARGET$COUNTER
  done
done

