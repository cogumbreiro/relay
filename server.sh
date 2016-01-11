#!/bin/bash
date
echo "incrementing generation number"
scripts/next_num.py gen_num.txt
echo "running server on $@"
#./server.exe -r -u -cg $@
./server.exe -r -cg $@
date
