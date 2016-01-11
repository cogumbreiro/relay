#!/usr/bin/gnuplot

set terminal postscript color enhanced
#set terminal png transparent nocrop enhanced font arial 8 size 420,320 

set autoscale
set xtic auto
set ytic auto
set key left box


#------------------------------------------------------------

set output "scc.ps"

#set title "Fun-ID vs SCC size"
set xlabel "Normalized Fun-ID (0-1)"
set ylabel "SCC Size"

plot "steens.scc.dat" using 1:2 title "Steens" , \
     "fp_rci_ci.scc.dat" using 1:2 title "Us - CI" , \
     "fp_rci.scc.dat" using 1:2 title "Us" , \
     "noalias.scc.dat" using 1:2 title "No-FPs"
# with linespoints


#------------------------------------------------------------

set output "fanout.ps"

set xlabel "Normalized Callsite-ID (0-1)"
set ylabel "Fanout"

plot "steens.fanout.dat" using 1:2 title "Steens" , \
     "fp_rci_ci.fanout.dat" using 1:2 title "Us - CI" with points pointtype 5 , \
     "fp_rci.fanout.dat" using 1:2 title "Us" with points pointtype 6
