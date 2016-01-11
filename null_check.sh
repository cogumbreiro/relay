#!/bin/bash

CILTREES=$1
ANALYSIS="nullset"
LOGFILE="${CILTREES}/log_${ANALYSIS}_rad"
./radar_passes.sh $ANALYSIS $@

KNOWFILE=$LOGFILE.know_data
./getknowledge.sh $LOGFILE $CILTREES > $KNOWFILE 2>&1
