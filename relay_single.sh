#!/bin/bash

#--- test one callgraph w/ null analysis

#TODO remove the need to the Lockset partitioning phase

CILTREES=$1
shift 

function doAnalysis () {
    CONFIG=client.cfg
    LOG=$CILTREES/log.relay

    ./server.sh $CILTREES -cc $CONFIG > /dev/null 2>&1 &
    ./relay.sh $CILTREES -su $CONFIG $@ | tee $LOG 2>&1
    killall server.exe
}

doAnalysis $@
