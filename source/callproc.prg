/*
 * Calls external program as a dll
 */

#define GUIS_VERSION   "1.4"

STATIC nConnType := 1
STATIC cn := e"\n"
STATIC nLogOn := 0, cLogFile := "guiserver.log"
STATIC cFileRoot := "gs", cDirRoot

FUNCTION gs_Run( cExe, nLog, nType, cDir )

   IF Valtype( nLog ) == "N"
      nLogOn := nLog
   ENDIF

   IF nType == Nil .OR. nType == 1
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
   ELSEIF nType == 2
      nConnType := 2
      IF Empty( cDir )
         cDirRoot := Iif( Empty( cDir ), hb_DirTemp(), cDir )
      ENDIF
      srv_conn_Create( cDirRoot + cFileRoot, .F. )
   ENDIF

#ifdef __HWGUI__
   SET TIMER oMTimer OF HWindow():GetMain() VALUE nInterval ACTION {||TimerFunc()}
#endif
   extgui_RunApp( cExe + Iif( !Empty(nType).AND.nType==2, " type=2", "" ) , 1 )

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

STATIC FUNCTION gWritelog( s )

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
