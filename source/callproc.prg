/*
 * Calls external program as a dll
 */

FUNCTION gs_Run( cExe, nLog, nType, cDir )

   gs_ipInit()

   IF Valtype( nLog ) == "N"
      nLogOn := nLog
      IF nLogOn > 1
         gs_SetLogFile( "ac.log" )
      ENDIF
   ENDIF
   gs_SetVersion( GUIS_VERSION )

   IF nType == Nil .OR. nType == 1
      nConnType := 1
      gs_SetHandler( "MAINHANDLER" )
      gs_CreateSocket( nPort )
   ELSEIF nType == 2
      nConnType := 2
      IF Empty( cDir )
         cDir := hb_DirTemp()
      ENDIF
      srv_conn_Create( cDir + cFileRoot, .F. )
   ENDIF

   SET TIMER oMTimer OF HWindow():GetMain() VALUE nInterval ACTION {||TimerFunc()}
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
