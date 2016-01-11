#!/bin/bash

SUMDIR=$1
TEMP_DF=`tempfile`
echo "DF: $TEMP_DF"
ls $SUMDIR*.fp_df | sed "s/fp_df//" | awk -F '/' ' { print $5 } ' | sort | uniq > $TEMP_DF
TEMP_SUMS=`tempfile`
echo "SUMS: $TEMP_SUMS"
ls $SUMDIR*.fpsum | sed "s/fpsum//" | awk -F '/' ' { print $5 } ' | sort | uniq > $TEMP_SUMS

TEMP_DIFF=`tempfile`
echo "DIFF-Left $TEMP_DIFF"
diff $TEMP_DF $TEMP_SUMS --left-column | grep "<" | sed "s/< //" | sed "s/\([0-9]*\).*/\1/" > $TEMP_DIFF 

