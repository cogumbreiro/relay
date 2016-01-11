#!/bin/sh
if [ $# -eq "1" ]; then
  remote=$1
else
  remote="jvoung@fwg-cs0.ucsd.edu:~/relay/."
fi
echo "synchronizing with $remote"
scp race_anal.exe server.exe printwarn.exe printsumm.exe inspect.exe race_temp_anal.exe race_temp2_anal.exe null_anal.exe null2_anal.exe null3_anal.exe print_delta.exe $remote

