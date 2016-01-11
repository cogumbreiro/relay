#!/bin/bash
date

tempdir=/tmp/

#reserve a file to store checkpoint / status information
function reserveCheck {
    statusFile=`tempfile -d $tempdir -p temp.status`
    echo "reserved status file: $statusFile"
}

function clearCheck {
    rm $statusFile
    echo "cleared status file: $statusFile"
}

function runAnalysis {
    echo "running analysis on $1"
    ./null_anal.exe -cg $1 -r -u jan -st $statusFile
    return $?
}

MAXTRIES=100
TRIES=0
reserveCheck
retval=127
until [ $retval -eq "0" ]; do
    runAnalysis $1
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
