/*
 *  A server side for callproc
 */

#define GUIS_VERSION   "1.4"

STATIC nConnType := 2
STATIC nPort := 3101
STATIC cFileRoot := "gs", cDir
STATIC lEnd := .F., nInterval := 20
STATIC cn := e"\n"
STATIC nLogOn := 0, cLogFile := "extserver.log"

FUNCTION esrv_Init( ... )

   LOCAL i, aParams := hb_aParams(), x

   FOR i := 1 TO Len( aParams )
      //gWritelog( aParams[i] )
      IF ( x := Left( aParams[i],3 ) ) == "dir"
         cDir := Substr( aParams[i], 5 )
         IF Left( cDir,1 ) == '"'
            cDir := Substr( cDir, 2, Len(cDir)-2 )
         ENDIF
      ELSEIF x == "log"
         nLogOn := Val( Substr( aParams[i], 5 ) )
      ENDIF
   NEXT

   IF nConnType == 1
#ifdef __IP_SUPPORT
      gs_ipInit()
      IF nLogOn > 1
         gs_SetLogFile( "ac.log" )
      ENDIF
      gs_SetVersion( GUIS_VERSION )
      gs_SetHandler( "MAINHANDLER" )
      gWritelog( "Connect via ports " + Ltrim(Str(nPort)) + ", " + Ltrim(Str(nPort+1)) )
      gs_CreateSocket( nPort )
#endif
   ELSEIF nConnType == 2
      IF Empty( cDir )
         cDir := hb_DirTemp()
      ENDIF
      conn_SetVersion( GUIS_VERSION )
      gWritelog( "Connect via files "+ cDir + cFileRoot + ".*" )
      //srv_conn_Create( cDir + cFileRoot, .T. )
      IF Empty( client_conn_Connect( cDir + cFileRoot ) )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

FUNCTION esrv_Wait()

   DO WHILE !lEnd
      gs_Sleep( nInterval )
      IF nConnType == 1
#ifdef __IP_SUPPORT
         gs_ListenSocket()
         gs_CheckSocket()
         IF gs_CheckSockError()
            Panic()
         ENDIF
#endif
      ELSEIF nConnType == 2
         conn_CheckIn()
      ENDIF
   ENDDO
   gWritelog( "gs_Wait: exit" )

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

FUNCTION esrv_RunProc( cFunc, aParams )

   LOCAL i

   FOR i := 1 TO Len(aParams)
      IF Valtype( aParams[i] ) != "C"
         aParams[i] := CnvVal( aParams[i] )
      ENDIF
   NEXT
   SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( aParams ) } ) )

   RETURN Nil

STATIC FUNCTION Parse( arr, lPacket )

   LOCAL cCommand := Lower( arr[1] ), c := Left( cCommand, 1 ), arrp
   LOCAL o, lErr := .F., cRes := "", cFunc, xRes

   gwritelog( "Command: " + cCommand )
   SWITCH c
   CASE 's'
      IF cCommand == "setvar"
         lErr := ( Len(arr)<3 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            IF !__mvExist( cRes := Upper(arr[2]) )
               __mvPublic( cRes )
            ENDIF
            __mvPut( cRes, arr[3] )
         ENDIF
      ENDIF
      EXIT

   CASE 'g'
      IF cCommand == "getver"
         lErr := ( Len(arr)<2 )
         IF !lErr
            SendIn( "+" + hb_jsonEncode(gVersion(arr[2])) + cn )
         ENDIF
      ELSEIF cCommand == "getvar"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF __mvExist( cRes := Upper(arr[2]) )
               cRes := __mvGet( cRes )
            ELSE
               cRes := Nil
            ENDIF
            SendIn( "+" + hb_jsonEncode(cRes) + cn )
         ENDIF
      ENDIF
      EXIT

   CASE 'e'
      IF cCommand == "evalcode"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF Len( arr ) > 2 .AND. Lower( arr[3] ) == "t"
               //cRes := DoScript( RdScript( ,arr[2] ) )
               SendIn( "+" + hb_jsonEncode(cRes) + cn )
            ELSE
               IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
               //DoScript( RdScript( ,arr[2] ) )
            ENDIF
         ENDIF
      ELSEIF cCommand == "exit" .OR. cCommand == "endapp"
         lEnd := .T.
         IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
      ENDIF
      EXIT

   CASE 'r'
      IF cCommand == "runproc"

         cFunc := Lower( arr[2] )
         IF hb_isFunction( cFunc )
            SendIn( "+Ok"+cn )
            xRes := &( "{|a|"+cFunc+"(a)}" )
            IF Len( arr ) > 2
               hb_jsonDecode( arr[3], @arrp )
            ENDIF
            Eval( xRes, arrp )
         ELSE
            lErr := .T.
         ENDIF

      ELSEIF cCommand == "runfunc"

         cFunc := Lower( arr[2] )
         IF hb_isFunction( cFunc )
            xRes := &( "{|a|"+cFunc+"(a)}" )
            IF Len( arr ) > 2
               hb_jsonDecode( arr[3], @arrp )
            ENDIF
            xRes := Eval( xRes, arrp )
            SendIn( "+" + hb_jsonEncode(xRes) + cn )
         ELSE
            lErr := .T.
         ENDIF
      ENDIF
      EXIT

   OTHERWISE
      lErr := .T.

   END

   IF lErr .AND. !lPacket
      SendIn( "+Err" + cn )
   ENDIF

   RETURN !lErr

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

   IF !Parse( arr, .F. )
      gWritelog( "Parsing error" )
   ENDIF

   RETURN Nil

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

STATIC FUNCTION gVersion( n )
   RETURN Iif( n==0, GUIS_VERSION, "hbExtServer " + GUIS_VERSION )

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

#pragma BEGINDUMP

#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
  #include <unistd.h>
  #include <sys/time.h>
  #include <sys/timeb.h>
#else
  #include <windows.h>
#endif

#include "hbapi.h"

static void sleep_ns( long int milliseconds )
{
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
   Sleep( milliseconds );
#else
   struct timeval tv;
   tv.tv_sec = milliseconds / 1000;
   tv.tv_usec = milliseconds % 1000 * 1000;
   select(0, NULL, NULL, NULL, &tv);
#endif
}

#include "hbapi.h"

HB_FUNC( GS_SLEEP )
{
   sleep_ns( hb_parni(1) );
}

#pragma ENDDUMP
