#!/bin/bash

function checkFinish {
  if [ $? -ne "0" ]; then
    echo "Previous pass died, stopping $0"
    exit $?
  fi
}

#rm log, then do the loopTryAnalysis w/ append only?

./server.sh $1 &
sleep 0.5
./race_temp.sh $1
checkFinish
killall server.exe
wait $!

./server.sh $1 &
sleep 0.5
echo -e "\n\n2ND ANALYSIS\n\n" >> log
python run_client.py "./sym_seq.exe" $1
checkFinish
killall server.exe
wait $!

./server.sh $1 &
sleep 0.5
echo -e "\n\n3RD ANALYSIS\n\n" >> log
python run_client.py "./sym_race.exe" $1
checkFinish
wait $!

./server.sh $1 &
sleep 0.5
echo -e "\n\n4TH ANALYSIS\n\n" >> log
python run_client.py "./sym_adj_l.exe" $1
checkFinish
killall server.exe
wait $!


./server.sh $1 &
sleep 0.5
echo -e "\n\n4.5TH ANALYSIS\n\n" >> log
python run_client.py "./sym_adj_nl.exe" $1
checkFinish
killall server.exe
wait $!

./server.sh $1 &
sleep 0.5
echo -e "\n\n5TH ANALYSIS\n\n" >> log
python run_client.py "./sym_pess.exe" $1
checkFinish
killall server.exe
wait $!


#echo -e "\n\nPRINTING DELTA: SEQ vs PAR\n\n"
#./print_delta.exe -cg $1 -seq $1/null_warn_seq.dat -adj $1/null_warn_par.dat >> log 2>&1

#echo -e "\n\nPRINTING DELTA: SEQ vs PESS\n\n"
#./print_delta.exe -cg $1 -seq $1/null_warn_seq.dat -adj $1/null_warn_pess.dat >> log 2>&1

./getknowledge.sh log $1 >> log 
