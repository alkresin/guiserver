@echo off

set HB_INSTALL=\harbour
set HWGUI_INSTALL=\papps\hwgui_uni
set SRC_DIR=source

%HB_INSTALL%\bin\harbour %SRC_DIR%\guiserver.prg %SRC_DIR%\fconnect.prg -n -q -w2 -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include 2>ah.out

bcc32  -c -O2 -tW -M -I%HB_INSTALL%\include;%HWGUI_INSTALL%\include guiserver.c fconnect.c %SRC_DIR%\hbip.c %SRC_DIR%\listen.c >a1.out

echo 1 24 "\papps\hwgui_218\image\WindowsXP.Manifest" > hwgui_xp.rc
rem echo 1 24 "Windows7.Manifest" > hwgui_xp.rc
brc32 -r hwgui_xp -fohwgui_xp

echo c0w32.obj + > b32.bc
echo guiserver.obj + >> b32.bc
echo fconnect.obj + >> b32.bc
echo listen.obj + >> b32.bc
echo hbip.obj, + >> b32.bc
echo guiserver.exe, + >> b32.bc
echo guiserver.map, + >> b32.bc
echo %HWGUI_INSTALL%\lib\hwgui.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\procmisc.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\hbxml.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\hwgdebug.lib + >> b32.bc

echo %HB_INSTALL%\lib\win\bcc\hbrtl.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbvm.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbdebug.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\gtgui.lib + >> b32.bc
rem echo %HB_INSTALL%\lib\win\bcc\gtwin.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hblang.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbcpage.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbmacro.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbrdd.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\rddntx.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\rddfpt.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\rddcdx.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbsix.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbcommon.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbpp.lib + >> b32.bc
echo %HB_INSTALL%\lib\win\bcc\hbpcre.lib + >> b32.bc

echo cw32mt.lib + >> b32.bc
echo ws2_32.lib + >> b32.bc
echo iphlpapi.lib + >> b32.bc
echo import32.lib, >> b32.bc
echo hwgui_xp.res >> b32.bc
ilink32 -Gn -Tpe -aa @b32.bc

@del *.tds
@del guiserver.c
@del fconnect.c
@del *.map
@del *.obj
@del *.rc
@del *.res
@del b32.bc
