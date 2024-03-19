/*
 * Calls external program as a dll
 */

#define GUIS_VERSION   "1.4"

STATIC nConnType := 2
STATIC cn := e"\n"
STATIC nLogOn := 0, cLogFile := "extclient.log"
STATIC cFileRoot := "gs", cDirRoot
STATIC nInterval := 20

FUNCTION ecli_Run( cExe, nLog, cDir )

   LOCAL nSec

   IF Valtype( nLog ) == "N"
      nLogOn := nLog
   ENDIF

/* #ifdef __IP_SUPPORT
      gs_ipInit()
      IF nLogOn > 1
         gs_SetLogFile( "ac.log" )
      ENDIF
      gs_SetVersion( GUIS_VERSION )
      gs_SetHandler( "MAINHANDLER" )
      gs_CreateSocket( nPort )
#endif */

   cDirRoot := Iif( Empty( cDir ), hb_DirTemp(), cDir )
   IF !( Right( cDirRoot,1 ) $ "\/" )
      cDirRoot += hb_ps()
   ENDIF
   gwritelog( cdirroot )
   IF !srv_conn_Create( cDirRoot + cFileRoot, .F. )
      RETURN .F.
   ENDIF

   ecli_RunApp( cExe + ' dir="' + cDirRoot + '" ' + Iif( nLogOn>0, "log="+Str(nLogOn,1), "" ) + ;
      Iif( nConnType==2, " type=2", "" ) )

   nSec := Seconds()
   DO WHILE Seconds() - nSec < 1
      IF !Empty( ecli_CheckAnswer() )
         RETURN .T.
      ENDIF
      ecli_Sleep( nInterval*2 )
   ENDDO

   RETURN .F.

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

FUNCTION ecli_RunProc( cFunc, aParams )

   LOCAL i

   FOR i := 1 TO Len(aParams)
      IF Valtype( aParams[i] ) != "C"
         aParams[i] := CnvVal( aParams[i] )
      ENDIF
   NEXT
   SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( aParams ) } ) )

   RETURN Nil

FUNCTION ecli_RunFunc( cFunc, aParams, lNoWait )

   LOCAL cRes := SendOut( hb_jsonEncode( { "runfunc", cFunc, hb_jsonEncode( aParams ) } ), lNoWait )

   IF Left( cRes,1 ) == '"'
      RETURN Substr( cRes, 2, Len(cRes)-2 )
   ENDIF
   RETURN cRes

FUNCTION ecli_CheckAnswer()

   RETURN conn_CheckOut()

FUNCTION ecli_SetVar( cVarName, cValue )

   SendOut( hb_jsonEncode( { "setvar", cVarName, cValue } ) )

   RETURN Nil

FUNCTION ecli_GetVar( cVarName )

   LOCAL cRes := SendOut( hb_jsonEncode( { "getvar", cVarName } ) )

   RETURN Substr( cRes,2,Len(cRes)-2 )

STATIC FUNCTION SendOut( s, lNoWait )

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
      cRes := conn_Send2SocketOut( "+" + s + cn, lNoWait )
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

FUNCTION ecli_Close()

   conn_SetNoWait( .T. )
   SendOut( '["endapp"]' )
   ecli_Sleep( nInterval*2 )

   IF nConnType == 1
#ifdef __IP_SUPPORT
      gs_ipExit()
#endif
   ELSEIF nConnType == 2
      //gwritelog( "End" )
      conn_Exit()
   ENDIF
   ecli_Sleep( nInterval*2 )

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

EXIT PROCEDURE ECLI_EXIT

   ecli_Close()

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
HB_FUNC( ECLI_RUNAPP )
{
   //hb_retl( g_spawn_command_line_async( hb_parc(1), NULL ) );
   char scmd[2048];
   int nLen = hb_parclen( 1 );

   memcpy( scmd, hb_parc(1), nLen );
   scmd[nLen] = ' ';
   scmd[nLen+1] = '&';
   scmd[nLen+2] = '\0';
   hb_retl( system( scmd ) > 0 );
}
#else
HB_FUNC( ECLI_RUNAPP )
{
   STARTUPINFO si;
   PROCESS_INFORMATION pi;
#ifdef UNICODE
   TCHAR wc1[CMDLENGTH];
#endif
   BOOL bRes;

   ZeroMemory( &si, sizeof(si) );
   si.cb = sizeof(si);
   si.wShowWindow = SW_SHOW;
   si.dwFlags = STARTF_USESHOWWINDOW;
   ZeroMemory( &pi, sizeof(pi) );

#ifdef UNICODE
   MultiByteToWideChar( GetACP(), 0, hb_parc(1), -1, wc1, CMDLENGTH );
   bRes = CreateProcess( NULL,   // No module name (use command line)
       wc1,            // Command line
       NULL,           // Process handle not inheritable
       NULL,           // Thread handle not inheritable
       FALSE,          // Set handle inheritance to FALSE
       CREATE_NO_WINDOW,  // No creation flags
       NULL,           // Use parent's environment block
       NULL,           // Use parent's starting directory
       &si,            // Pointer to STARTUPINFO structure
       &pi );          // Pointer to PROCESS_INFORMATION structure
#else
   bRes = CreateProcess( NULL,   // No module name (use command line)
       (LPTSTR)hb_parc(1),  // Command line
       NULL,           // Process handle not inheritable
       NULL,           // Thread handle not inheritable
       FALSE,          // Set handle inheritance to FALSE
       CREATE_NO_WINDOW,  // No creation flags
       NULL,           // Use parent's environment block
       NULL,           // Use parent's starting directory
       &si,            // Pointer to STARTUPINFO structure
       &pi );          // Pointer to PROCESS_INFORMATION structure
#endif
   hb_retl( bRes != 0 );
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

HB_FUNC( ECLI_SLEEP )
{
   sleep_ns( hb_parni(1) );
}

#pragma ENDDUMP
