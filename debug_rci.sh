#!/bin/bash
EXE=fp_rci_byte
echo "incrementing generation number"
scripts/next_num.py gen_num.txt
echo "running funptr analysis w/ option $@"
./debug.sh $EXE -r -u jan -cg $@ -o tempcg.txt

