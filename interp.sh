#!/bin/sh

#-- Run the ocaml interpreter w/ the necessary libraries linked

CIL=cil
INCLUDES="-I $CIL/src -I $CIL/src/ext -I $CIL/src/ext/pta -I $CIL/src/frontc -I $CIL/ocamlutil -I $CIL/obj/x86_LINUX -I datastructs -I pta"
ocaml $INCLUDES 
