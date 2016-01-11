RUNSCRIPT=`pwd`/$1
DUMPER=`pwd`/scripts/dump-with-stats.sh
echo "dumper is $DUMPER"
echo "runscript is $RUNSCRIPT"

CGTEST_DIR=cgtests
#BENCHMARKS="bzip2-1.0.5_comb unzip60_comb"
BENCHMARKS="bzip2-1.0.5_comb unzip60_comb icecast-2.3.2_comb openssh-5.2p1_comb mutt-1.5.19_comb postfix262_smtpd_comb git-1.6.3.3_comb vim72_comb emacs-22.3_comb nethack-3.4.3_comb"

for BENCH in $BENCHMARKS; do
  echo "Starting $BENCH"
  TESTDIR=$CGTEST_DIR/$BENCH/
  if [ ! -d $TESTDIR/ciltrees ]; then
    echo "Need to create ciltrees for $BENCH"
    ( cd $TESTDIR ; $DUMPER > /dev/null 2>&1 )
  fi
  $RUNSCRIPT $CGTEST_DIR/$BENCH/ciltrees -time -noFunkPtrA > /dev/null 2>&1
  echo "Finished $BENCH"
done
echo "DONE!!!"

