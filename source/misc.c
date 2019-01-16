/*
 * extGUI - GUI framework for Harbour
 * Functions, which provides launching of a GuiServer process.
 *
 * Copyright 2018 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */
#include "hbapiitm.h"

#if defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
//#include "gtk/gtk.h"
HB_FUNC( EXTGUI_RUNAPP )
{
   hb_retl( g_spawn_command_line_async( hb_parc(1), NULL ) );
}

#else
#include "windows.h"
HB_FUNC( EXTGUI_RUNAPP )
{
      STARTUPINFO si;
      PROCESS_INFORMATION pi;
      void * hStr;

      ZeroMemory( &si, sizeof(si) );
      si.cb = sizeof(si);
      si.wShowWindow = SW_SHOW;
      si.dwFlags = STARTF_USESHOWWINDOW;
      ZeroMemory( &pi, sizeof(pi) );

      CreateProcess( NULL,   // No module name (use command line)
          (LPTSTR)hb_parc(1),  // Command line
          NULL,           // Process handle not inheritable
          NULL,           // Thread handle not inheritable
          FALSE,          // Set handle inheritance to FALSE
          CREATE_NEW_CONSOLE,   // No creation flags
          NULL,           // Use parent's environment block
          NULL,           // Use parent's starting directory 
          &si,            // Pointer to STARTUPINFO structure
          &pi );          // Pointer to PROCESS_INFORMATION structure
}
#endif
