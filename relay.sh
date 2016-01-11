#!/bin/bash
date

#ulimit -v 15000
#ulimit -a

usage="Usage: relay.sh cg_file [opts]"

if [ $# -lt 1 ]; then
    echo $usage
    exit 127
fi

cgdir=$1
shift
tempdir=/tmp/

#reserve a file to store checkpoint / status information
function reserveCheck {
    statusFile=`tempfile -d $tempdir -p temp.status`
    echo "reserved status file: $statusFile"
}

reserveCheck

function clearCheck {
    rm $statusFile
    echo "cleared status file: $statusFile"
}

function runAnalysis {
    echo "running analysis on $cgdir"
    ./race_anal.exe -cg $cgdir -r -u jan -st $statusFile $@
    return $?
}

MAXTRIES=1
TRIES=0
retval=127
until [ $retval -eq "0" ]; do
    runAnalysis $@
    retval=$?
    TRIES=$[$TRIES + 1]
    #grr... can't get boolean || to work in the until's conditional so...
    if [ $TRIES -ge $MAXTRIES ]; then
        echo "Exhausted tries: $TRIES"
        retval=0
    fi
    echo "return value is $retval"
    echo "ran $TRIES times"
done
clearCheck
date
