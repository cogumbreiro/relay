#!/bin/bash
grep " / calls / " $1 | awk -F '/' ' { print $3 } ' | sort | uniq
