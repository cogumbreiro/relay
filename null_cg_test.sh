#!/bin/bash

#--- test different callgraphs w/ the null analysis as client

CILTREES=$1
TO_TEST="noalias oic llvm_anders anders llvm_dsa steens"

for CGTYPE in $TO_TEST; do
    ./null_cg_one.sh $CILTREES $CGTYPE
    echo "FINISHED $CGTYPE"
    sleep 1
done
echo "DONE with $TO_TEST"

