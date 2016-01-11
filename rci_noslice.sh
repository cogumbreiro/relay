#!/bin/bash

EXE=fp_rci.exe
#EXE=fp_rci_byte
CALLS_FILE=calls.oic
CALLS_NOSLICE=calls.oic.noslice
SCRIPTS_DIR=/home/jan/research/relay-race/scripts
CG_DIR=$1
LOG=$CG_DIR/log
LOGNOSLICE=$CG_DIR/lognoslice
PWD=`pwd`
TIMEOUT=21600 # 6 hours timeout
POLLTIME=5    # seconds between check for timeout

# copy-paste of timeout checker...
limit_time()
{
  limit=$1
  proc_id=$2
  poll_time=$3
  i=0
  while [ "$i" -lt "$limit" ]
  do 
    sleep $poll_time
    kill -0 $proc_id > /dev/null 2>&1
    if [ $? -ne "0" ]; then # already terminated
      return 0
    fi
    i=$((i+$poll_time))
  done
  kill -9 $proc_id > /dev/null 2>&1
  return 1
}

check_time_for()
{
  pid=$1
  log=$2
  limit_time $TIMEOUT $pid $POLLTIME
  if [ $? -ne "0" ]; then
    echo "TIMED OUT!!!" >> $log
    return 0
  fi
}


date > $LOG
echo "incrementing generation number"
$SCRIPTS_DIR/next_num.py gen_num.txt
echo "running funptr analysis w/ option $@ AND SLICE (CI, FI also)" | tee --append $LOG
nice -n 15 ./$EXE -r -u jan -cg $@ -o $CALLS_FILE -testCIFI -offw 2>&1 | tee --append $LOG &
check_time_for $! $LOG
nice -n 15 ./scc_stats.exe -cg $CG_DIR/$CALLS_FILE 2>&1 | tee --append $LOG

date >> $LOG

date > $LOGNOSLICE

echo "incrementing generation number"
$SCRIPTS_DIR/next_num.py gen_num.txt
echo "running funptr analysis w/ option $@ AND NO SLICE" | tee --append $LOGNOSLICE
nice -n 15 ./$EXE -r -u jan -cg $@ -o $CALLS_NOSLICE -noNFP 2>&1 | tee --append $LOGNOSLICE &
check_time_for $! $LOGNOSLICE
nice -n 15 ./scc_stats.exe -cg $CG_DIR/$CALLS_NOSLICE 2>&1 | tee --append $LOGNOSLICE

date >> $LOGNOSLICE

echo "generating scc plot" | tee --append $LOG
./scc_compare.exe -cg $CG_DIR/ -o $1/ 2>&1 | tee --append $LOG
(cd $1; $SCRIPTS_DIR/plot.gp)

echo `pwd`
date >> $LOG
$SCRIPTS_DIR/contextstats.sh $LOG > $CG_DIR/log.cstats
$SCRIPTS_DIR/contextstats.sh $LOGNOSLICE > $CG_DIR/logno.cstats

tar -cjvf $CG_DIR/results.tar.bz2 $CG_DIR/calls.* $CG_DIR/*.dat $CG_DIR/log* $CG_DIR/*.ps


