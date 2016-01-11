#!/bin/bash
LOG=$1
LOC=$2
STARTPATTERN="New global @ $LOC"
ENDPATTERN="  }"
OUTDIR="tempglobals"
echo "Getting global vals @ $LOC from $LOG into $OUTDIR"
scripts/grepbetween.py "$STARTPATTERN" "$ENDPATTERN" $LOG > $OUTDIR/temp
scripts/split_on.py "$STARTPATTERN" $OUTDIR/temp $OUTDIR/$FUNC
