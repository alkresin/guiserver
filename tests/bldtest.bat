@echo off
set HB_INSTALL=c:\harbour

   %HB_INSTALL%\bin\harbour %1.prg ..\source\extgui.prg ..\source\extgui_classes.prg ..\source\fconnect.prg -n -w -i%HB_INSTALL%\include %2 %3
   bcc32 -O2 -d -I%HB_INSTALL%\include -L%HB_INSTALL%\lib\win\bcc %1.c extgui.c extgui_classes.c fconnect.c ..\source\hbip.c ..\source\misc.c ..\source\listen.c hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbcpage.lib hbct.lib hbpcre.lib hbcplr.lib ws2_32.lib iphlpapi.lib

   del %1.c
   del extgui.c
   del extgui_classes.c
   del fconnect.c
   del *.obj
   del *.tds