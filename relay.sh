#!/bin/bash
date

#ulimit -v 15000
#ulimit -a

usage="Usage: relay.sh cg_file [status_file]"

if [ $# -lt 1 ]; then
    echo $usage
    exit 127
fi

#grr... can't get boolean OR to work
if [ $# -gt 2 ]; then
    echo $usage
    exit 127
fi

tempdir=/tmp/

#reserve a file to store checkpoint / status information
function reserveCheck {
    statusFile=`tempfile -d $tempdir -p temp.status`
    echo "reserved status file: $statusFile"
}


if [ $# -eq 2 ]; then
    statusFile=$2
    if [ -e $statusFile ]; then
        echo "using status file: $statusFile"
    else
        echo "asked to use status file $statusFile and it doesn't exist"
        exit 127
    fi
else
    reserveCheck
fi

function clearCheck {
    rm $statusFile
    echo "cleared status file: $statusFile"
}

function runAnalysis {
    echo "running analysis on $1"
    ./race_anal.exe -cg $1 -r -u jan -st $statusFile
    return $?
}

MAXTRIES=100
TRIES=0
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
