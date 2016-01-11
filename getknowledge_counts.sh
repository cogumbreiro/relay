#!/bin/bash

### think about overflow?
grep "SEQ knowledge counter" $1 \
  | awk -F ":" ' { sum = sum + $2 } END { print "      SEQ " sum } '
grep "RADAR knowledge counter" $1 \
  | awk -F ":" ' { sum = sum + $2 } END { print "    RADAR " sum } '
grep "RADAR-NL knowledge counter" $1 \
  | awk -F ":" ' { sum = sum + $2 } END { print " RADAR-NL " sum } '
grep "STEENS knowledge counter" $1 \
  | awk -F ":" ' { sum = sum + $2 } END { print "   STEENS " sum } '
