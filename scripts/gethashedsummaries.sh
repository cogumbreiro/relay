#!/bin/bash
LOG=$1
FUNC=$2
STARTPATTERN="New hashed summary $FUNC"
ENDPATTERN="^------"
DIVPATTERN=$STARTPATTERN
OUTDIR="temphashsums"
echo "Getting summaries for $FUNC from $LOG into $OUTDIR"
scripts/grepbetween.py "$STARTPATTERN" "$ENDPATTERN" -n 2 $LOG | 
  sed "s/$STARTPATTERN.*/$STARTPATTERN/" > $OUTDIR/temp
scripts/split_on.py "$DIVPATTERN" $OUTDIR/temp $OUTDIR/$FUNC"_"
