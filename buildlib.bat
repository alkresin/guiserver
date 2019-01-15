@echo off

set HB_INSTALL=\harbour
set HWGUI_INSTALL=\papps\hwgui_uni

%HB_INSTALL%\bin\harbour guiserver.prg -n -DGUIS_LIB -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include

bcc32  -c -O2 -tW -M -I%HB_INSTALL%\include;%HWGUI_INSTALL%\include guiserver.c hbip.c listen.c misc.c >a1.out

del guisrv.lib
tlib guisrv.lib +guiserver +hbip +listen +misc

del guiserver.c
del *.obj
