#!/bin/bash

ulimit -m 2300000
#--- test one callgraph w/ null analysis

#TODO remove the need to the Lockset partitioning phase

CILTREES=$1
CGTYPE=$2
TIMEOUT_SECS=50000

function limit_time()
{
  limit=$1
  proc_id=$2
  i=0
  while [ "$i" -lt "$limit" ]
  do 
    sleep 1
    kill -0 $proc_id > /dev/null 2>&1
    if [ $? -ne "0" ]; then # already terminated
      return 0
    fi
    i=$((i+1))
  done
  kill -9 $proc_id > /dev/null 2>&1
  return 1
}


function doAnalysis () {
    CGTYPE=$1 
    CONFIG=client.cfg.$CGTYPE
    LOG=$CILTREES/log.$CGTYPE

    if [ ! -e $CILTREES/calls.$CGTYPE ]; then
        echo "Need to create calls.$CGTYPE"
    else  
        echo "TESTING $CGTYPE"
        ./server.sh $CILTREES -cc $CONFIG > /dev/null 2>&1 &
        ./race_temp.sh $CILTREES $LOG -su $CONFIG > /dev/null 2>&1 &
        limit_time $TIMEOUT_SECS $!
        killall server.exe

        sleep 2

        ./server.sh $CILTREES -cc $CONFIG > /dev/null 2>&1 &
        ./null.sh $CILTREES $LOG -su $CONFIG > /dev/null 2>&1 &
        limit_time $TIMEOUT_SECS $!
        killall server.exe

        echo "\n === CLUSTERED COUNTS OF SAFE/UNSAFE DEREF CHECKS\n" >> $LOG
        
        ./deref_stats.exe -caption SEQ \
            -safe $CILTREES/safe_derefs_seq.dat \
            -unsafe $CILTREES/unsafe_derefs_seq.dat >> $LOG
    fi
}

doAnalysis $CGTYPE

