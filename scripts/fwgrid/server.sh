#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -e logs
#$ -o logs

date
echo "incrementing generation number"
./next_num.py gen_num.txt
echo "running server on $1"
./server.exe -cg $1 -r
date
