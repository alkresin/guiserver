#!/bin/bash
export HB_INS=/home/apps/harbour
export HWGUI_DIR=$HOME/apps/hwgui-code
export HWGUI_INC=$HWGUI_DIR/include
export HWGUI_LIB=$HWGUI_DIR/lib
export SYSTEM_LIBS="-lm -lrt"
export HARBOUR_LIBS="-lhbdebug -lhbvm -lhbrtl -lgtcgi -lhblang -lhbrdd -lhbmacro -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbsix -lhbcommon -lhbcpage -lhbsqlit3 -lhbct"
export HWGUI_LIBS="-lhwgui -lprocmisc -lhbxml -lhwgdebug"

$HB_INS/bin/linux/gcc/harbour guiserver -n  -q -i$HB_INS/include -i$HWGUI_INC -w2 $1 2>bldh.log

gcc guiserver.c hbip.c listen.c misc.c -c -Wall -O3 -I $HB_INS/include -I $HWGUI_INC -I ../../../source/gtk -DHWG_USE_POINTER_ITEM -L $HB_INS/lib/linux/gcc `pkg-config --cflags gtk+-2.0`  >bld.log 2>bld.log
ar rc guisrv.a guiserver.o hbip.o listen.o misc.o

rm guiserver.c
rm *.o
