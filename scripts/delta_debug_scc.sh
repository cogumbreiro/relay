#!/bin/bash

# delta-debug a file down while steens and andersens still gets SCCs > N
# (anders may timeout... if that is the case we'll just assume it still
#  had a large enough SCC)

FILE=test.c
GCC=/usr/bin/gcc
CG_STUFF=/home/jan/r
DUMP=$CG_STUFF/scripts/dump-calls.sh

TIME_LIMIT=150
SCC_LIMIT=$1

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

scc_test () {
    CURDIR=`pwd`
    echo "Attempting to dump anders cg"
    cd $CG_STUFF; \
        ./fix_id_cg.exe -cg $CURDIR/ciltrees -ni -su client.cfg.anders > \
        log.txt.anders 2>&1 &
    limit_time $TIME_LIMIT $!
    if [ $? -ne "0" ]; then
        echo "TIMED OUT!!!"
        echo "CHECKING steens size"
        # check steens size as safety net?
        (cd $CG_STUFF; \
            ./scc_stats.exe -su client.cfg.steens -cg $CURDIR/ciltrees/calls.steens | \
            $CG_STUFF/scripts/maxscc_at_least.py $SCC_LIMIT)
        MAXSCCSTEENS=$?
        return $MAXSCCSTEENS
    fi

    echo "CHECKING anders size"
    (cd $CG_STUFF; \
        ./scc_stats.exe -su client.cfg.anders -cg $CURDIR/ciltrees/calls.anders | \
        $CG_STUFF/scripts/maxscc_at_least.py $SCC_LIMIT)
    MAXSCCANDERS=$?
    return $MAXSCCANDERS
}

$GCC -Wfatal-errors -c $FILE && \
    ($DUMP && scc_test)

