#!/bin/bash
LOG=$1
NAME=$2
STARTPATTERN="New global.*$NAME"
ENDPATTERN="  }"
OUTDIR="tempglobals"
echo "Getting global vals for $NAME from $LOG into $OUTDIR"
scripts/grepbetween.py "$STARTPATTERN" "$ENDPATTERN" $LOG > $OUTDIR/temp
scripts/split_on.py "$STARTPATTERN" $OUTDIR/temp $OUTDIR/$FUNC
