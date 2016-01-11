#!/bin/bash
LOG=$1
FUNC=$2
STARTPATTERN="New summary $FUNC"
ENDPATTERN="Global mods:"
DIVPATTERN=$STARTPATTERN
OUTDIR="tempsummaries"
echo "Getting summaries for $FUNC from $LOG into $OUTDIR"
scripts/grepbetween.py "$STARTPATTERN" "$ENDPATTERN" $LOG > $OUTDIR/temp
scripts/split_on.py "$DIVPATTERN" $OUTDIR/temp $OUTDIR/$FUNC"_"
