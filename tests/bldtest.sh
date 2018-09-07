#!/bin/bash
export HB_INS=/home/alkresin/apps/harbour
export SYSTEM_LIBS="-lm -lrt -lglib-2.0"
export HARBOUR_LIBS="-lhbdebug -lhbvm -lhbrtl -lgttrm -lhblang -lhbrdd -lhbmacro -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbsix -lhbcommon -lhbcpage -lhbsqlit3 -lhbct"

$HB_INS/bin/linux/gcc/harbour $1.prg ../extgui.prg ../extgui_classes.prg -n  -q -i$HB_INS/include -w2 $1 2>bldh.log

gcc $1.c extgui.c extgui_classes.c ../hbip.c ../listen.c ../misc.c -o$1  -I $HB_INS/include -DHWG_USE_POINTER_ITEM -L $HB_INS/lib/linux/gcc $SYSTEM_LIBS -Wl,--start-group $HARBOUR_LIBS -Wl,--end-group  >bld.log 2>bld.log

rm $1.c
rm extgui.c
rm extgui_classes.c
rm *.o
