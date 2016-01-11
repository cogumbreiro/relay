#!/bin/bash
date
echo "incrementing generation number"
scripts/next_num.py gen_num.txt
echo "running server on $1"
./server.exe -cg $1 -r
date
