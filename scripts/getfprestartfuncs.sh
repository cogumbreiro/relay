#!/bin/bash
LOG=$1
grep "Restarting " $LOG | cut -d ":" -f 1 | sort | uniq -c 
