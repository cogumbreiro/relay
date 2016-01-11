#!/bin/bash
date

usage="Usage: $0 cg_file [status_file]"

if [ $# -lt 1 ]; then
    echo $usage
    exit 127
fi

CILTREES=$1
shift
tempdir=/tmp/

#reserve a file to store checkpoint / status information
function reserveCheck {
    statusFile=`tempfile -d $tempdir -p temp.status`
    echo "reserved status file: $statusFile"
}

if [ $# -ge 1 ]; then
  logFILE=$1
  shift
  echo "using log file: $logFILE"
else
  logFILE="log"
fi

# if [ $# -ge 1 ]; then
#     statusFile=$1
#     shift
#     if [ -e $statusFile ]; then
#         echo "using status file: $statusFile"
#     else
#         echo "asked to use status file $statusFile and it doesn't exist"
#         exit 127 
#     fi  
# else
#     reserveCheck
# fi

function clearCheck {
    rm $statusFile
    echo "cleared status file: $statusFile"
}

function runAnalysis {
    echo "running NULL analysis on $CILTREES w/ $@"
    echo -e "\n\n2ND ANALYSIS\n\n" >> $logFILE
    ./null_anal.exe -cg $CILTREES -u ravi $@ >> $logFILE 2>&1
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
#clearCheck
date
