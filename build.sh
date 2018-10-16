#!/bin/bash
export HB_INS=/home/alkresin/apps/harbour
export HWGUI_DIR=/home/alkresin/apps/hwgui-code
export HWGUI_INC=$HWGUI_DIR/include
export HWGUI_LIB=$HWGUI_DIR/lib
export SYSTEM_LIBS="-lm -lrt"
export HARBOUR_LIBS="-lhbdebug -lhbvm -lhbrtl -lgtcgi -lhblang -lhbrdd -lhbmacro -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbsix -lhbcommon -lhbcpage -lhbsqlit3 -lhbct"
export HWGUI_LIBS="-lhwgui -lprocmisc -lhbxml -lhwgdebug"

$HB_INS/bin/linux/gcc/harbour guiserver -n  -q -i$HB_INS/include -i$HWGUI_INC -w2 $1 2>bldh.log

gcc guiserver.c hbip.c listen.c -oguiserver  -I $HB_INS/include -I $HWGUI_INC -I ../../../source/gtk -DHWG_USE_POINTER_ITEM -L $HB_INS/lib/linux/gcc -L $HWGUI_LIB -Wl,--start-group $HWGUI_LIBS $HARBOUR_LIBS $SYSTEM_LIBS -Wl,--end-group `pkg-config --cflags gtk+-2.0` `pkg-config gtk+-2.0 --libs`  >bld.log 2>bld.log

rm guiserver.c
rm *.o
