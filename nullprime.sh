#!/bin/bash

function checkFinish {
  if [ $? -ne "0" ]; then
    echo "Previous pass died, stopping $0"
    exit $?
  fi
}

./server.sh $1 &
sleep 0.5
./race_temp.sh $1
checkFinish
killall server.exe
wait $!

./server.sh $1 &
sleep 0.5
./null.sh $1
checkFinish
killall server.exe
wait $!

./server.sh $1 &
sleep 0.5
./race_temp2.sh $1
checkFinish
wait $!


./server.sh $1 &
sleep 0.5
./null2.sh $1
checkFinish
killall server.exe
wait $!

./server.sh $1 &
sleep 0.5
./null3.sh $1
checkFinish
killall server.exe
wait $!

echo -e "\n\nPRINTING DELTA: SEQ vs PAR\n\n"
./print_delta.exe -cg $1 -seq $1/null_warn_seq.dat -adj $1/null_warn_par.dat >> log 2>&1

echo -e "\n\nPRINTING DELTA: SEQ vs PESS\n\n"
./print_delta.exe -cg $1 -seq $1/null_warn_seq.dat -adj $1/null_warn_pess.dat >> log 2>&1

./getknowledge.sh log $1/ >> log 
