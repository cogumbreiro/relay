#!/bin/bash
LOG=$1
grep "Starting call" $LOG | awk ' { print $4 } ' | sort | uniq -c 
