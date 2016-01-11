#!/bin/bash

FILE=test.c
GCC=/usr/bin/gcc
CG_STUFF=/home/jan/r
DUMP=$CG_STUFF/scripts/dump-calls.sh
CALLS_FILE=calls.oic

limit_time()
{
  limit=$1
  proc_id=$2
  i=0
  echo "Watching for $proc_id"
  while [ "$i" -lt "$limit" ]
  do 
    sleep 1
    echo "$i"
    kill -0 $proc_id > /dev/null 2>&1
    if [ $? -ne "0" ]; then # already terminated
      return 0
    fi
    i=$((i+1))
  done
  kill -9 $proc_id > /dev/null 2>&1
  return 1
}

TIME_LIMIT=35

cg_cons () {
  CURDIR=`pwd`
  cd $CG_STUFF; \
      ./fp_rci.exe -r -u jan -o $CALLS_FILE -cg $CURDIR/ciltrees \
      > templog 2>&1 &
  limit_time $TIME_LIMIT $!
  if [ $? -ne "0" ]; then
    echo "TIMED OUT"
    return 0
  fi
  return 1
}

$GCC -Wfatal-errors -c $FILE && \
  ($DUMP && cg_cons)

