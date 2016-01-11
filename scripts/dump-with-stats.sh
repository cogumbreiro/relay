#!/bin/sh

# Dumps the call graph, and cil ASTs to the DUMPROOT specified below
# Reads the information from "gcc-log"
# Requires the "duppy" script
# Remember to change RELAYROOT

CURROOT=$PWD
DUMPROOT=$PWD/ciltrees
RELAYROOT=/home/jan/research/relay-race
DUPPYROOT=$RELAYROOT/scripts
CILLYROOT=$RELAYROOT/cil/bin
LOG=$DUMPROOT/log.txt

#gcc-log has "cd" and "duppy" commands on each line
CMDS=$PWD/gcc-log.txt


/bin/rm -rf $DUMPROOT
mkdir -p $DUMPROOT
/bin/rm -f $LOG

export CURROOT
export DUMPROOT
export CILLYROOT
export RELAYROOT
SKIP_AFTERCIL=1
export SKIP_AFTERCIL

NODEF=$PWD/nodef.h
export NODEF

# nodef?

# first dump the files

duppy ()
{
    echo duppy $*
    $DUPPYROOT/duppy $*
}

check_death ()
{
  cmdname=$1
  retval=$?
  if [ $retval -ne "0" ]; then
    echo "$cmdname died with $retval"
    exit 127
  fi
}

limit_time()
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

STARTTIME=$(date +%s)

(. $CMDS) 2>&1 | tee $LOG

check_death "dumpcmds"

#fix variable / struct ids...
cd $RELAYROOT; ./fix_id_cg.exe -cg $DUMPROOT -ng -su client.cfg.steens >> $LOG 2>&1

check_death "fixid"

#now gen scc, etc. stats for other comparison points
COMPARE_TO="steens cil anders noalias"
TIMEOUT_SECS=15000


do_compare() {
  COMP=$1
  # -ni to not fix ids, but at least dump the callgraph
  date > $LOG.$COMP
  cd $RELAYROOT; time ./fix_id_cg.exe -cg $DUMPROOT -ni -su client.cfg.$COMP >> $LOG.$COMP 2>&1 &
  limit_time $TIMEOUT_SECS $!
  if [ $? -ne "0" ]; then
    echo "TIMED OUT!!!" >> $LOG.$COMP
    return 0
  fi
  cd $RELAYROOT; ./scc_stats.exe -cg $DUMPROOT/calls.$COMP -su client.cfg.$COMP >> $LOG.$COMP 2>&1
  date >> $LOG.$COMP
  #TODO: add fan-out stats
}


for COMP in $COMPARE_TO; do
  echo "Generating stats for $COMP"
  do_compare $COMP
  echo "DONE!"
done

ENDTIME=$(date +%s)
DIFF=$(( $ENDTIME - $STARTTIME ))
echo "Dumped in $DIFF seconds"
