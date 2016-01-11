#!/bin/bash

CIL=cil
INCLUDES="-I $CIL/src -I $CIL/src/ext -I $CIL/src/ext/pta -I $CIL/src/frontc -I $CIL/ocamlutil -I $CIL/obj/x86_LINUX -I datastructs -I pta -I cilexts -I fp_analysis -I fp_rci_analysis -I radar "
TARGET=$1
shift
if [ `which rlwrap` != "" ]; then
  RLWRAP="rlwrap"
  echo "using rlwrap!"
else
  RLWRAP=""
  echo "not using rlwrap =("
fi
 
$RLWRAP ocamldebug $INCLUDES $TARGET $*
