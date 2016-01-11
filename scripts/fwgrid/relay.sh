#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -e logs
#$ -o logs

date
echo "running analysis worker on $@"
./race_anal.exe -u jvoung -r -cg $@
date
