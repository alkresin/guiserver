# GuiServer

<b> Attention! Since October 6, 2023 we are forced to use two-factor authentication to be able to
   update the repository. Because it's not suitable for me, I will probably use another place for projects.
   Maybe, https://gitflic.ru/, maybe, Sourceforge... Follow the news on my website, http://www.kresin.ru/ </b>

A server application, providing GUI service, providing an opportunity to create GUI frameworks for different programming languages.
A connection with your program is established via tcp/ip connection or via regular files.
It is written on Harbour (look at http://www.kresin.ru/en/harbour.html) and C, GUI layer is provided by HwGUI library (http://www.kresin.ru/en/hwgui.html).
The official web page is http://www.kresin.ru/en/guisrv.html
Join the multilanguage group https://groups.google.com/d/forum/guiserver to discuss the GuiServer, External and related issues.

### How to build
   Project source files:
      source/guiserver.prg    - main file, written on Harbour.
      source/fconnect.prg
      source/hbip.c
      source/listen.c
      source/misc.c

   To compile a GuiServer from sources you will need Harbour, HwGUI and a C compiler.

#### Windows:
Build.bat is provided to build guiserver.exe with Borland C compiler.
Probably, you will need to change HB_INSTALL and HWGUI_INSTALL variables in this batch file - they should point to your Harbour and HwGUI directories, respectively.
Unicode version of HwGUI is necessary for GuiServer.

#### Linux:
Use the build.sh to compile guiserver.
Probably, you will need to change HRB_INS and HWGUI_DIR variables in this shell file - they should point to your Harbour and HwGUI directories, respectively.

### Download
   You may download some ready binaries from http://www.kresin.ru/en/guisrv.html

### Installation
   Just copy an executable file to a folder of your choice. For to not write a full path to it in your applications, add this folder to a PATH environment variable.

### Notes
   extgui.prg and extgui_classes.prg together with hbip.c, listen.c are an implementation of a GUI framework, based on connection with GuiServer, for Harbour.
