#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -e logs
#$ -o logs

date
echo "running on $1"
./printwarn.exe -cg $1
date
