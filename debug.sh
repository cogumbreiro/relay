#!/bin/sh

CIL=cil
INCLUDES="-I $CIL/src -I $CIL/src/ext -I $CIL/src/ext/pta -I $CIL/src/frontc -I $CIL/ocamlutil -I $CIL/obj/x86_LINUX -I datastructs -I pta"
TARGET=$1
shift
ocamldebug $INCLUDES $TARGET $*
