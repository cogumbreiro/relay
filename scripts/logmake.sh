#!/bin/bash

make clean
make > make-log

TMP1=`tempfile`
sed "s/^.\?gcc /MYCC /" make-log | \
  sed "s/^.\?ld /MYLD /" | \
  sed "s/^.\?ar /MYAR /" | \
  # strip some crap 
  sed "/^checking /d" | \
  sed "/^creating /d" | \
  sed "/^configure: /d" | \
  sed "/^running /d" | \
  sed "/^building /d" | \
  sed "/^INFO: /d" | \
  # convert "XYZ Entering directory `DIR'" to "cd DIR"
  sed "s/.*Entering directory \`\(.*\)'/echo 'cd \1'\ncd \1/" | \
  # strip the rest of the crap
  sed "/^make/d" | \
  sed "/^Making/d" | \
  sed "/libtool/d" | \
  sed "/^config.status /d" > stripped-log.txt

