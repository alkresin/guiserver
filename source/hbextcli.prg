/*
 * Calls external program as a dll
 */

#define GUIS_VERSION   "1.4"

STATIC nConnType := 2
STATIC cn := e"\n"
STATIC nLogOn := 0, cLogFile := "extclient.log"
STATIC cFileRoot := "gs", cDirRoot
STATIC nInterval := 20

FUNCTION gs_Run( cExe, nLog, nType, cDir )

   IF Valtype( nLog ) == "N"
      nLogOn := nLog
   ENDIF

   IF nType != Nil .AND. nType == 1
      nConnType := 1
#ifdef __IP_SUPPORT
      gs_ipInit()
      IF nLogOn > 1
         gs_SetLogFile( "ac.log" )
      ENDIF
      gs_SetVersion( GUIS_VERSION )
      gs_SetHandler( "MAINHANDLER" )
      gs_CreateSocket( nPort )
#endif
   ELSE //IF nType == 2
      nConnType := 2
      IF Empty( cDir )
         cDirRoot := Iif( Empty( cDir ), hb_DirTemp(), cDir )
      ENDIF
      srv_conn_Create( cDirRoot + cFileRoot, .F. )
   ENDIF

#ifdef __HWGUI__
   //SET TIMER oMTimer OF HWindow():GetMain() VALUE nInterval ACTION {||TimerFunc()}
#endif
#ifdef __PLATFORM__UNIX
   hbext_RunApp( cExe + Iif( nConnType==2, " type=2 &", " &" ) )
#else
   hbext_RunApp( cExe + Iif( nConnType==2, " type=2", "" ) )
#endif
   RETURN Nil

STATIC FUNCTION CnvVal( xRes )

   LOCAL cRes := Valtype(xRes), nPos2

   IF cRes == "A"
      cRes := "Array"
   ELSEIF cRes == "O"
      cRes := "Object of " + xRes:Classname()
   ELSEIF cRes == "H"
      cRes := "Hash array"
   ELSEIF cRes == "U"
      cRes := "Nil"
   ELSEIF cRes == "C"
      cRes := xRes
   ELSEIF cRes == "L"
      cRes := Iif( xRes, "t", "f" )
   ELSE
      cRes := Trim( Transform( xReS, "@B" ) )
   ENDIF
   IF Valtype( xRes ) == "N" .AND. Rat( ".", cRes ) > 0
     nPos2 := Len( cRes )
     DO WHILE Substr( cRes, nPos2, 1 ) == '0'; nPos2 --; ENDDO
     IF Substr( cRes, nPos2, 1 ) == '.'
        nPos2 --
     ENDIF
     cRes := Left( cRes, nPos2 )
   ENDIF

   RETURN cRes

FUNCTION pGO( cFunc, aParams )

   LOCAL i

   FOR i := 1 TO Len(aParams)
      IF Valtype( aParams[i] ) != "C"
         aParams[i] := CnvVal( aParams[i] )
      ENDIF
   NEXT
   SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( aParams ) } ) )

   RETURN Nil

FUNCTION fGO( cFunc, aParams )

   LOCAL cRes := SendOut( hb_jsonEncode( { "runfunc", cFunc, hb_jsonEncode( aParams ) } ) )

   IF Left( cRes,1 ) == '"'
      RETURN Substr( cRes, 2, Len(cRes)-2 )
   ENDIF
   RETURN cRes

STATIC FUNCTION SendOut( s )

   LOCAL cRes
   gWritelog( "   " + Ltrim(Str(nConnType)) + " " + s )

   IF nConnType == 1
#ifdef __IP_SUPPORT
      cRes := gs_Send2SocketOut( "+" + s + cn )
      IF gs_CheckSockError()
         Panic()
      ENDIF
#endif
   ELSEIF nConnType == 2
      cRes := conn_Send2SocketOut( "+" + s + cn )
   ENDIF

   RETURN Iif( Empty(cRes), "", cRes )

STATIC FUNCTION SendIn( s )

   IF nConnType == 1
#ifdef __IP_SUPPORT
      gs_Send2SocketIn( s )
#endif
   ELSEIF nConnType == 2
      conn_Send2SocketIn( s )
   ENDIF

   RETURN Nil

FUNCTION MainHandler()

   LOCAL arr, cBuffer

   IF nConnType == 1
#ifdef __IP_SUPPORT
      cBuffer := gs_GetRecvBuffer()
#endif
   ELSEIF nConnType == 2
      cBuffer := conn_GetRecvBuffer()
   ENDIF

   gWritelog( cBuffer )

   hb_jsonDecode( cBuffer, @arr )
   IF Valtype(arr) != "A" .OR. Empty(arr)
      SendIn( "+Wrong" + cn )
      RETURN Nil
   ENDIF
   SendIn( "+Ok" + cn )

/*
   IF !Parse( arr, .F. )
      gWritelog( "Parsing error" )
   ENDIF
*/
   RETURN Nil

FUNCTION gs_Close()

   conn_SetNoWait( .T. )
   SendOut( '["endapp"]' )
   gs_Sleep( nInterval*2 )
   IF nConnType == 1
#ifdef __IP_SUPPORT
      gs_ipExit()
#endif
   ELSEIF nConnType == 2
      //gwritelog( "End" )
      conn_Exit()
   ENDIF
   gs_Sleep( nInterval*2 )

   RETURN Nil

FUNCTION gWritelog( s )

   LOCAL nHand

   IF nLogOn > 0
      IF ! File( cLogFile )
         nHand := FCreate( cLogFile )
      ELSE
         nHand := FOpen( cLogFile, 1 )
      ENDIF
      FSeek( nHand, 0, 2 )
      FWrite( nHand, s + cn )
      FClose( nHand )
   ENDIF
   RETURN Nil

EXIT PROCEDURE GS_EXIT

   gs_Close()

   RETURN

#pragma BEGINDUMP

#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
  #include <unistd.h>
  #include <sys/time.h>
  #include <sys/timeb.h>
#else
  #include <windows.h>
#endif

#include "hbapi.h"

#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
HB_FUNC( HBEXT_RUNAPP )
{
   //hb_retl( g_spawn_command_line_async( hb_parc(1), NULL ) );
   system( hb_parc(1) );
}
#else
HB_FUNC( HBEXT_RUNAPP )
{
      STARTUPINFO si;
      PROCESS_INFORMATION pi;

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
          CREATE_NO_WINDOW,  // No creation flags
          NULL,           // Use parent's environment block
          NULL,           // Use parent's starting directory
          &si,            // Pointer to STARTUPINFO structure
          &pi );          // Pointer to PROCESS_INFORMATION structure
}

#endif

static void sleep_ns( long int milliseconds )
{
#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
   struct timeval tv;
   tv.tv_sec = milliseconds / 1000;
   tv.tv_usec = milliseconds % 1000 * 1000;
   select(0, NULL, NULL, NULL, &tv);
#else
   Sleep( milliseconds );
#endif
}

HB_FUNC( GS_SLEEP )
{
   sleep_ns( hb_parni(1) );
}

#pragma ENDDUMP
