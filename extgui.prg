/*
 *  extGUI - GUI framework for Harbour
 *  extGUI is a Harbour library to build GUI application, using external
 *       standalone GUI server application.
 *  extGUI sources includes extgui.prg (this file), extgui_classes.prg, hbip.c, listen.c, misc.c.
 *
 * Copyright 2018 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

Static cn := e"\n"
Static aMenu := Nil, aMenuStack

#ifdef __PLATFORM__UNIX
//ANNOUNCE HB_GTSYS
//REQUEST HB_GT_CGI_DEFAULT
#endif

FUNCTION eGUI_Init( cOptions )

   LOCAL cServer := "guiserver.exe", cIp := "localhost", nPort := 3101, cLogFile := "ac.log"
   LOCAL cSep := e"\r\n", arr, i, s

   IF cOptions != Nil
      IF !( cSep $ cOptions )
         IF !( ( cSep := cn ) $ cOptions )
            cSep := Nil
         ENDIF
      ENDIF
      IF cSep == Nil
         arr := { cOptions }
      ELSE
         arr := hb_aTokens( cOptions, cSep )
      ENDIF
      FOR i := 1 TO Len( arr )
         s := Lower( arr[i] )
         IF Left( s,9 ) == "guiserver"
            cServer := AllTrim( Substr( arr[i], 11 ) )
         ELSEIF Left( s,7 ) == "address"
            cIp := AllTrim( Substr( arr[i], 9 ) )
         ELSEIF Left( s,4 ) == "port"
            nPort := Val( AllTrim( Substr( arr[i], 6 ) ) )
         ENDIF
      NEXT
   ENDIF

   SetLogFile( cLogFile )
   SetVersion( "1.0" )
   SetPrefix( "   )" )
   ipInit()

   IF !Empty( cServer )
      extgui_RunApp( cServer + " -p" + Ltrim(Str(nPort)),1 )
   ENDIF
   hb_idleSleep( 0.2 )
   //Sleep_ns( 200 )


   IF ConnectSocket( cIp, nPort ) == Nil
      hb_idleSleep( 2 )
      IF ConnectSocket( cIp, nPort ) == Nil
         ipExit()
         RETURN .F.
      ENDIF
   ENDIF

   SetHandler( "GUIHANDLER" )
   //CreateSocket( nPort+1 )
   hb_IdleAdd( {|| FIdle() } )
   //Send2SocketOut( '+' + hb_jsonEncode( { "setbconn" } ) + cn )

   RETURN .T.

FUNCTION eGUI_OpenMainForm( cFormName )

   Send2SocketOut( '+' + hb_jsonEncode( { "openformmain", cFormName } ) + cn )
   eGUI_Wait()

   RETURN Nil

FUNCTION eGUI_OpenForm( cFormName )

   Send2SocketOut( '+' + hb_jsonEncode( { "openform", cFormName } ) + cn )

   RETURN Nil

FUNCTION eGUI_OpenReport( cRepName )

   Send2SocketOut( '+' + hb_jsonEncode( { "openreport", cRepName } ) + cn )

   RETURN Nil


FUNCTION eGUI_InitMainWindow( x1, y1, w, h, cTitle, aProps )

   LOCAL oMain, sProps, s
   IF Empty( x1 ); x1 := 0; ENDIF
   IF Empty( y1 ); y1 := 0; ENDIF
   IF Empty( w ); w := 100; ENDIF
   IF Empty( h ); h := 100; ENDIF
   IF Empty( cTitle ); cTitle := ""; ENDIF

   oMain := EWindow():New( "main", "", x1, y1, w, h, cTitle )
   sProps := setprops( aProps )
   s := '["crmainwnd",' + hb_jsonEncode({ x1,y1,w,h,cTitle}) + sProps + ']'

   Send2SocketOut( '+' + s + cn )

   RETURN oMain

FUNCTION eGUI_ActivateMainWindow( lCenter )

   Send2SocketOut( '+' + hb_jsonEncode( { "actmainwnd", { Iif(Empty(lCenter),"f","t") } } ) + cn )
   eGUI_Wait()

   RETURN Nil

FUNCTION eGUI_InitDialog( x1, y1, w, h, cTitle, aProps )

   LOCAL oDlg, sProps, s
   IF Empty( x1 ); x1 := 0; ENDIF
   IF Empty( y1 ); y1 := 0; ENDIF
   IF Empty( w ); w := 100; ENDIF
   IF Empty( h ); h := 100; ENDIF
   IF Empty( cTitle ); cTitle := ""; ENDIF

   oDlg := EWindow():New( "dialog", "", x1, y1, w, h, cTitle )
   EWindow():oCurrWindow := oDlg

   sProps := setprops( aProps )
   s := '["crdialog","' + oDlg:cName + '",' + hb_jsonEncode({ x1,y1,w,h,cTitle}) + sProps + ']'

   Send2SocketOut( '+' + s + cn )

   RETURN oDlg

FUNCTION eGUI_ActivateDialog( lNoModal, lCenter )

   LOCAL oDlg := EWindow():oCurrWindow
   Send2SocketOut( '+' + hb_jsonEncode( { "actdialog", oDlg:cName, Iif(Empty(lNoModal),"f","t"), { Iif(Empty(lCenter),"f","t") } } ) + cn )
   IF Empty( lNoModal )
      oDlg:lWait := .T.
      //eGUI_WaitDlg( oDlg )
   ENDIF

   RETURN Nil

FUNCTION eGUI_Menu( cName )

   LOCAL aSubMenu, aNewMenu
   IF aMenu == Nil
      aMenu := {}
      aMenuStack := {}
   ELSE
      aSubMenu := Iif( Empty(aMenuStack), aMenu, ATail(aMenuStack) )
      aNewMenu := {}
      Aadd( aSubMenu, { cName, aNewMenu } )
      Aadd( aMenuStack, aNewMenu )
   ENDIF

   RETURN Nil

FUNCTION eGUI_EndMenu()

   IF Empty( aMenuStack )
      Send2SocketOut( '+' + hb_jsonEncode( { "menu", aMenu } ) + cn )
      aMenu := aMenuStack := Nil
   ELSE
      aMenuStack := ASize( aMenuStack, Len(aMenuStack)-1 )
   ENDIF

   RETURN Nil

FUNCTION eGUI_AddMenuItem( cName, cFunc, ... )

   LOCAL aSubMenu, cCode, arr := hb_aParams(), i

   IF Empty( aMenu )
      RETURN Nil
   ENDIF

   aSubMenu := Iif( Empty(aMenuStack), aMenu, ATail(aMenuStack) )
   IF !Empty( cFunc )
      cCode := 'pgo("' + cFunc + '"'
      FOR i := 3 TO Len( arr )
         cCode += Iif( i==3, ',{"', ',"' ) + arr[i] + IIf( i==Len(arr), '"}', '"' )
      NEXT
      cCode += ')'
   ELSE
      cCode := arr[3]
   ENDIF
   Aadd( aSubMenu, { cName, cCode } )

   RETURN Nil

FUNCTION eGUI_AddMenuSeparator()

   LOCAL aSubMenu

   IF Empty( aMenu )
      RETURN Nil
   ENDIF

   aSubMenu := Iif( Empty(aMenuStack), aMenu, ATail(aMenuStack) )
   Aadd( aSubMenu, { "-" } )

   RETURN Nil


FUNCTION eGUI_EvalProc( cCode )

   Send2SocketOut( '+' + hb_jsonEncode( { "evalcode", cCode } ) + cn )

   RETURN Nil

FUNCTION eGUI_CreateFont( cName, cFamily, nHeight, lBold, lItalic, lUnderline, lStrikeout, nCharset )

   LOCAL oFont := EFont():New( cName, cFamily, nHeight, lBold, lItalic, lUnderline, lStrikeout, nCharset )

   Send2SocketOut( '+' + hb_jsonEncode( { "crfont", oFont:cName, oFont:cFamily, ;
         oFont:nHeight, oFont:lBold, oFont:lItalic, oFont:lUnderline, oFont:lStrikeout, oFont:nCharset } ) + cn )

   RETURN oFont

FUNCTION eGUI_CreateStyle( cName, aColors, nOrient, aCorners, nBorder, tColor, cBitmap )

   LOCAL oStyle := EStyle():New( cName, aColors, nOrient, aCorners, nBorder, tColor, cBitmap )

   Send2SocketOut( '+' + hb_jsonEncode( { "crstyle", oStyle:cName, oStyle:aColors, ;
         oStyle:nOrient, oStyle:aCorners, oStyle:nBorder, oStyle:tColor, oStyle:cBitmap } ) + cn )

   RETURN oStyle

FUNCTION eGUI_EvalFunc( cCode )

   LOCAL cRes
   cRes := Send2SocketOut( '+' + hb_jsonEncode( { "evalcode", cCode, "t" } ) + cn )
   IF !Empty(cRes) .AND. Left( cRes,2 ) == '+"' .AND. Right( cRes,2 ) == ('"'+cn)
      cRes := Substr( cRes,3,Len(cRes)-4 )
   ENDIF

   RETURN Iif( Empty(cRes), "", cRes )

FUNCTION eGUI_GetValues( oWnd, aNames )

   LOCAL cRes, arr
   cRes := Send2SocketOut( '+' + hb_jsonEncode( { "getvalues", oWnd:cName, aNames } ) + cn )
   IF !Empty(cRes) .AND. Left( cRes,1 ) == '+'
      hb_jsonDecode( Substr( cRes,2,Len(cRes)-2 ), @arr )
      RETURN arr
   ENDIF

   RETURN Nil

FUNCTION eGUI_MsgInfo( cMessage, cTitle, cFunc, cName )

   Send2SocketOut( '+' + hb_jsonEncode( { "common", "minfo", cFunc, cName, cMessage, cTitle } ) + cn )

   RETURN Nil

FUNCTION eGUI_MsgStop( cMessage, cTitle, cFunc, cName )

   Send2SocketOut( '+' + hb_jsonEncode( { "common", "mstop", cFunc, cName, cMessage, cTitle } ) + cn )

   RETURN Nil

FUNCTION eGUI_MsgYesNo( cMessage, cTitle, cFunc, cName )

   Send2SocketOut( '+' + hb_jsonEncode( { "common", "myesno", cFunc, cName, cMessage, cTitle } ) + cn )

   RETURN Nil

FUNCTION eGUI_SelectFile( cPath, cFunc, cName )

   Send2SocketOut( '+' + hb_jsonEncode( { "common", "cfile", cFunc, cName, cPath } ) + cn )

   RETURN Nil

FUNCTION eGUI_SelectColor( nColor, cFunc, cName )

   Send2SocketOut( '+' + hb_jsonEncode( { "common", "ccolor", cFunc, cName, nColor } ) + cn )

   RETURN Nil

FUNCTION eGUI_SelectFont( cFunc, cName )

   Send2SocketOut( '+' + hb_jsonEncode( { "common", "cfont", cFunc, cName } ) + cn )

   RETURN Nil

FUNCTION eGUI_SetImagePath( cPath )

   Send2SocketOut( '+' + hb_jsonEncode( { "setparam", "bmppath", cPath } ) + cn )

   RETURN Nil

FUNCTION eGUI_SetPath( cPath )

   Send2SocketOut( '+' + hb_jsonEncode( { "setparam", "path", cPath } ) + cn )

   RETURN Nil

FUNCTION eGUI_Wait()

   DO WHILE Inkey(1) != 27
      //hb_idleSleep(1)
   ENDDO

   RETURN Nil

FUNCTION eGUI_WaitDlg( oDlg )

   DO WHILE oDlg:lWait
      Inkey(1)
      //hb_idleSleep(1)
   ENDDO

   RETURN Nil

FUNCTION FIdle()

   CheckSocket()

   RETURN Nil

FUNCTION setprops( aProps )

   LOCAL i, j, cProp, sProps := ""

   IF !Empty( aProps )
      FOR i := 1 TO Len( aProps )
         cProp := Lower( aProps[i,1] )
         IF cProp == "winstyle"
            sProps += ',"Winstyle": ' + Ltrim(Str(aProps[i,2]))
         ELSEIF cProp == "tcolor"
            sProps += ',"TColor": ' + Ltrim(Str(aProps[i,2]))
         ELSEIF cProp == "bcolor"
            sProps += ',"BColor": ' + Ltrim(Str(aProps[i,2]))
         ELSEIF cProp == "font"
            sProps += ',"Font": "' + aProps[i,2]:cName + '"'
         ELSEIF cProp == "hstyle"
            sProps += ',"HStyle": "' + aProps[i,2]:cName + '"'
         ELSEIF cProp == "tooltip"
            sProps += ',"Tooltip": "' + aProps[i,2] + '"'
         ELSEIF cProp == "picture"
            sProps += ',"Picture": "' + aProps[i,2] + '"'
         ELSEIF cProp == "transpa"
            sProps += ',"Transpa": "t"'
         ELSEIF cProp == "cimage"
            sProps += ',"Image": "' + aProps[i,2] + '"'
         ELSEIF cProp == "aitems"
            sProps += ',"AItems": ' + hb_jsonEncode(aProps[i,2])
         ELSEIF cProp == "hstyles"
            sProps += ',"HStyles": ['
            FOR j := 1 TO Len(aProps[i,2])
               sProps += Iif( j==1,'"',',"' ) + aProps[i,2,j]:cName + '"'
            NEXT
            sProps += ']'
         ENDIF
      NEXT
      IF !Empty( sProps )
         sProps := ",{" + Substr(sProps,2) + "}"
      ENDIF
   ENDIF

   RETURN sProps

STATIC FUNCTION GetItemByName( arr, cName )

   LOCAL oItem
   FOR EACH oItem IN arr
      IF !Empty( oItem:cName ) .AND. oItem:cName == cName
         RETURN oItem
      ENDIF
   NEXT

   RETURN Nil

FUNCTION eGUI_GetWnd( cName )

   LOCAL oItem, aDialogs

   cName := Lower( cName )
   IF cName == "main"
      RETURN EWindow():oMain
   ELSE
      RETURN GetItemByName( EWindow():aDialogs, cName )
   ENDIF
   RETURN Nil

FUNCTION eGUI_GetWidg( cWidgName )

   LOCAL nPos := At( ".", cWidgName ), oWnd, aControls, oItem, cWnd

   IF nPos == 0
      RETURN eGUI_GetWnd( cWidgName )
   ENDIF

   cWnd := Left( cWidgName, nPos-1 )
   cWidgName := Lower(Substr( cWidgName, nPos+1 ))

   IF !Empty( oWnd := eGUI_GetWnd( cWnd ) )
      IF (nPos := At( ".", cWidgName )) != 0
         IF ( oWnd := GetItemByName( oWnd:aWidgets, Left( cWidgName, nPos-1 ) ) ) == Nil
            RETURN Nil
         ENDIF
         cWidgName := Substr( cWidgName, nPos+1 )
      ENDIF
      RETURN GetItemByName( oWnd:aWidgets, cWidgName )
   ENDIF

   RETURN Nil

FUNCTION GUIHandler()

   LOCAL cBuffer := GetRecvBuffer(), arr, arrp, cCommand, cFunc, lSend := .F., xRes, o
   //? "Handler"

   IF Left( cBuffer,1 ) == "+"

      hb_jsonDecode( Substr(cBuffer,2), @arr )
      IF Valtype(arr) != "A" .OR. Empty(arr)
         Send2SocketIn( "+Err"+cn )
         RETURN Nil
      ENDIF

      cCommand := Lower( arr[1] )
      IF cCommand == "runproc"

         Send2SocketIn( "+Ok"+cn )
         lSend := .T.
         cFunc := Lower( arr[2] )
         xRes := &( "{|a|"+cFunc+"(a)}" )
         IF Len( arr ) > 2
            hb_jsonDecode( arr[3], @arrp )
         ENDIF
         Eval( xRes, arrp )

      ELSEIF cCommand == "runfunc"

         cFunc := Lower( arr[2] )
         xRes := &( "{|a|"+cFunc+"(a)}" )
         IF Len( arr ) > 2
            hb_jsonDecode( arr[3], @arrp )
         ENDIF
         xRes := Eval( xRes, arrp )
         Send2SocketIn( "+" + hb_jsonEncode(xRes) + cn )
         lSend := .T.

      ELSEIF cCommand == "exit"
         IF !Empty( o := egui_GetWnd( arr[2] ) )
            o:lWait := .F.
            o:Delete()
         ENDIF
         lSend := .T.
         Send2SocketIn( "+Ok"+cn )

      ELSEIF cCommand == "endapp"
         lSend := .T.
         Send2SocketIn( "+Ok"+cn )
         //? "Quitting"
         ipExit()
         Quit

      ENDIF

      IF !lSend
         Send2SocketIn( "+Ok"+cn )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION eGUI_Exit

   ipExit()

   RETURN Nil
