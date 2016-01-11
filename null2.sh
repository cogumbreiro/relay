#!/bin/bash
date

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
    # running null_byte for now to get stack trace printed
    # always directing stdout and stderr to "log"
    #./null_anal.exe -cg $1 -u ravi -r -st $statusFile > log 2>&1
    echo -e "\n\n4TH ANALYSIS\n\n" >> log
     ./null2_anal.exe -cg $1 -u ravi -st $statusFile >> log 2>&1
     #./null2_byte -cg $1 -u ravi -st $statusFile >> log 2>&1
    return $?
}

MAXTRIES=1
TRIES=0
#reserveCheck
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
