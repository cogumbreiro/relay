#!/bin/bash

function checkFinish {
  PREV_EXIT=$1
  if [ $PREV_EXIT -ne "0" ]; then
    echo "Previous pass died, stopping $0"
    exit $PREV_EXIT
  fi
}

ANALYSIS=$1
shift
CILTREES=$1
shift


LOGFILE="${CILTREES}/log_${ANALYSIS}_rad"
RADAR_PASSES="${ANALYSIS}_radar.exe"
RACE_PASSES="${ANALYSIS}_race.exe"

STARTTIME=$(date +%s)

./server.sh $CILTREES &
./race_temp.sh $CILTREES $LOGFILE $@
PREV_EXIT=$?
killall server.exe

echo "waiting for server to end"
wait
checkFinish $PREV_EXIT

echo "==============" >> $LOGFILE
echo "PASS SEQ" >> $LOGFILE
echo "==============" >> $LOGFILE
date >> $LOGFILE

./server.sh $CILTREES &
./$RADAR_PASSES -cg $CILTREES -u jan -mode seq $@ >> $LOGFILE 2>&1
PREV_EXIT=$?
killall server.exe

echo "waiting for server to end"
wait
checkFinish $PREV_EXIT

echo "==============" >> $LOGFILE
echo "PASS RACE" >> $LOGFILE
echo "==============" >> $LOGFILE
date >> $LOGFILE

./server.sh $CILTREES &
./$RACE_PASSES -cg $CILTREES -u jan $@ >> $LOGFILE 2>&1
 PREV_EXIT=$?
killall server.exe

echo "waiting for server to end"
wait
checkFinish $PREV_EXIT

echo "==============" >> $LOGFILE
echo "PASS ADJ" >> $LOGFILE
echo "==============" >> $LOGFILE
date >> $LOGFILE

./server.sh $CILTREES &
./$RADAR_PASSES -cg $CILTREES -u jan -mode adj $@ >> $LOGFILE 2>&1
PREV_EXIT=$?
killall server.exe

echo "waiting for server to end"
wait
checkFinish $PREV_EXIT

echo "==============" >> $LOGFILE
echo "PASS ADJ-NL" >> $LOGFILE
echo "==============" >> $LOGFILE
date >> $LOGFILE

./server.sh $CILTREES &
./$RADAR_PASSES -cg $CILTREES -u jan -mode adjnl $@ >> $LOGFILE 2>&1
PREV_EXIT=$?
killall server.exe

echo "waiting for server to end"
wait
checkFinish $PREV_EXIT

echo "==============" >> $LOGFILE
echo "PASS PESS" >> $LOGFILE
echo "==============" >> $LOGFILE
date >> $LOGFILE

./server.sh $CILTREES &
./$RADAR_PASSES -cg $CILTREES -u jan -mode pess $@ >> $LOGFILE 2>&1
PREV_EXIT=$?
killall server.exe

echo "waiting for server to end"
wait
checkFinish $PREV_EXIT

echo "==============" >> $LOGFILE
echo "DONE" >> $LOGFILE
date >> $LOGFILE

#----

KNOWFILE=$LOGFILE.know_data

./getknowledge_counts.sh $LOGFILE > $KNOWFILE
date >> $KNOWFILE

ENDTIME=$(date +%s)
DIFF=$(( $ENDTIME - $STARTTIME ))
echo "RADAR'ed in $DIFF seconds" | tee -a $LOGFILE

