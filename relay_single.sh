#!/bin/bash

#--- run one instance of relay (along with the server)

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
