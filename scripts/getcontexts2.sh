#!/bin/bash
LOG=$1
FUNC=$2
STARTPATTERN="SuggestContext for $FUNC"
DIVPATTERN="========"
OUTDIR="tempcontexts"
echo "Getting contexts for $FUNC from $LOG into $OUTDIR"
scripts/grepbetween.py "$STARTPATTERN" "$DIVPATTERN" $LOG > $OUTDIR/temp
scripts/split_on.py "$DIVPATTERN" $OUTDIR/temp $OUTDIR/$FUNC
