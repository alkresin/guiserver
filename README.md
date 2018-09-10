# GuiServer
A server application, providing GUI service via tcp/ip connection, providing an opportunity to create GUI frameworks for different programming languages.
It is written on Harbour (см. http://www.kresin.ru/en/harbour.html) and C, GUI layer is provided by HwGUI library (http://www.kresin.ru/en/hwgui.html).
The official web page is http://www.kresin.ru/guisrv.html

### Preface

### How to build
   Project source files:
      guiserver.prg    - main file, written on Harbour.
      hbip.c
      listen.c
      misc.c

   To compile a GuiServer from sources you will need Harbour, HwGUI and a C compiler.

#### Windows:
Build.bat is provided to build guiserver.exe with Borland C compiler.
Probably, you will need to change HB_INSTALL and HWGUI_INSTALL variables in this batch file - they should point to your Harbour and HwGUI directories, respectively.
Unicode version of HwGUI is necessary for GuiServer.

#### Linux:
Use the build.sh to compile guiserver.
Probably, you will need to change HRB_INS and HWGUI_DIR variables in this shell file - they should point to your Harbour and HwGUI directories, respectively.

### Download
   You may download binaries from http://www.kresin.ru/guisrv.html

### Installation
   Just copy an executable file to a folder of your choice. For to not write a full path to it in your applications, add this folder to a PATH environment variable.
