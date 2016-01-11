#!/bin/bash
EXE=analyze_fp.exe
#EXE=analyze_fp_byte
date
echo "incrementing generation number"
scripts/next_num.py gen_num.txt
echo "running funptr analysis w/ option $@"
./$EXE -r -u jan -cg $@ -o tempcg.txt 2>&1 | tee log
date
