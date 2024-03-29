/*
 *  extGUI - GUI framework for Harbour
 *  extGUI is a Harbour library to build GUI application, using external
 *       standalone GUI server application.
 *  extGUI sources includes extgui.prg (this file), extgui_classes.prg, hbip.c, listen.c, misc.c.
 *
 * Copyright 2018 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

STATIC nConnType := 1
STATIC cFileRoot := "gs", cDir
STATIC cn := e"\n"
STATIC aMenu := Nil, aMenuStack
STATIC lPacket := .F., cPacketBuff
STATIC aRunProc := {,,}, nRunProc := 0
STATIC cInitialPath

Memvar oLastWindow, oLastWidget, oLastPrinter

#ifdef __PLATFORM__UNIX
//ANNOUNCE HB_GTSYS
//REQUEST HB_GT_CGI_DEFAULT
#endif

FUNCTION eGUI_Init( cOptions )

   LOCAL cServer := "guiserver.exe", cIp := "localhost", nPort := 3101, cLogFile := "ac1.log", nLog := 0
   LOCAL cSep := e"\r\n", arr, i, s

   PUBLIC oLastWindow, oLastWidget, oLastPrinter

   cInitialPath := hb_fnameDir( hb_argv(0) )
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
         ELSEIF Left( s,3 ) == "log"
            s := Val( AllTrim( Substr( arr[i], 5 ) ) )
            nLog := Iif( s == 1, 1, Iif( s == 2, 2, 0 ) )
         ELSEIF Left( s,4 ) == "type"
            nConnType := Val( AllTrim( Substr( arr[i], 6 ) ) )
         ELSEIF Left( s,3 ) == "dir"
            cDir := AllTrim( Substr( arr[i], 5 ) )
            IF !( Right( cDir,1 ) ) $ "\/"
               cDir += hb_ps()
            ENDIF
         ELSEIF Left( s,4 ) == "file"
            cFileRoot := AllTrim( Substr( arr[i], 6 ) )
         ENDIF
      NEXT
   ENDIF

   IF nConnType == 1
      IF nLog > 1
         gs_SetLogFile( cLogFile )
         gs_SetPrefix( "   )" )
      ENDIF
      gs_SetVersion( "1.0" )
      gs_ipInit()
   ELSEIF nConnType == 2
      IF Empty( cDir )
         cDir := hb_DirTemp()
      ENDIF
      FErase( cDir + cFileRoot + ".gs1" )
      FErase( cDir + cFileRoot + ".gs2" )
      conn_SetVersion( "1.0" )

   ENDIF

   IF !Empty( cServer )
      extgui_RunApp( cServer + " -p" + Ltrim(Str(nPort)) + ;
         Iif(nLog>0," -log"+Str(nLog,1),"") + Iif(nConnType>1, ;
         " -t"+Ltrim(Str(nConnType))+" f"+cFileRoot,"") )
   ENDIF
   hb_idleSleep( 0.2 )
   //gs_Sleep_ns( 200 )

   IF nConnType == 1
      IF ( s := gs_ConnectSocket( cIp, nPort ) ) == Nil
         hb_idleSleep( 2 )
         IF ( s := gs_ConnectSocket( cIp, nPort ) ) == Nil
            gs_ipExit()
            eGUI_Writelog( "No connection" )
            RETURN 1
         ENDIF
      ENDIF
   ELSEIF nConnType == 2
      IF Empty( s := client_conn_Connect( cDir + cFileRoot ) )
         hb_idleSleep( 0.5 )
         IF Empty( s := client_conn_Connect( cDir + cFileRoot ) )
            hb_idleSleep( 1 )
            IF Empty( s := client_conn_Connect( cDir + cFileRoot ) )
               eGUI_Writelog( "No connection" )
               RETURN 1
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   s := Substr( s, At( '/', s )+1 )
   IF s != gs_proto_Version()
      eGUI_Writelog( "Wrong protocol version. Need " + gs_proto_Version() + ", received: " + s )
      RETURN 2
   ENDIF
   gs_SetHandler( "MAINHANDLER" )

   hb_IdleAdd( {|| FIdle() } )

   RETURN 0

FUNCTION eGUI_OpenMainForm( cFormName )

   SendOut( hb_jsonEncode( { "openformmain", cFormName } ) )
   eGUI_Wait()

   RETURN Nil

FUNCTION eGUI_OpenForm( cFormName )

   SendOut( hb_jsonEncode( { "openform", cFormName } ) )

   RETURN Nil

FUNCTION eGUI_OpenReport( cRepName )

   SendOut( hb_jsonEncode( { "openreport", cRepName } ) )

   RETURN Nil


FUNCTION eGUI_InitMainWindow( x1, y1, w, h, cTitle, aProps )

   LOCAL oMain, sProps, s
   IF Empty( x1 ); x1 := 0; ENDIF
   IF Empty( y1 ); y1 := 0; ENDIF
   IF Empty( w ); w := 100; ENDIF
   IF Empty( h ); h := 100; ENDIF
   IF Empty( cTitle ); cTitle := ""; ENDIF

   oLastWindow := oMain := EWindow():New( "main", "", x1, y1, w, h, cTitle )
   sProps := setprops( aProps )
   s := '["crmainwnd",' + hb_jsonEncode({ x1,y1,w,h,cTitle}) + sProps + ']'

   SendOut( s )

   RETURN oMain

FUNCTION eGUI_ActivateMainWindow( lCenter )

   SendOut( hb_jsonEncode( { "actmainwnd", { Iif(Empty(lCenter),"f","t") } } ) )
   eGUI_Wait()

   RETURN Nil

FUNCTION eGUI_InitDialog( cName, x1, y1, w, h, cTitle, aProps )

   LOCAL oDlg, sProps, s
   IF Empty( x1 ); x1 := 0; ENDIF
   IF Empty( y1 ); y1 := 0; ENDIF
   IF Empty( w ); w := 100; ENDIF
   IF Empty( h ); h := 100; ENDIF
   IF Empty( cTitle ); cTitle := ""; ENDIF

   oLastWindow := oDlg := EWindow():New( "dialog", cName, x1, y1, w, h, cTitle )
   EWindow():oCurrWindow := oDlg

   sProps := setprops( aProps )
   s := '["crdialog","' + oDlg:cName + '",' + hb_jsonEncode({ x1,y1,w,h,cTitle}) + sProps + ']'

   SendOut( s )

   RETURN oDlg

FUNCTION eGUI_ActivateDialog( lNoModal, lCenter )

   LOCAL oDlg := EWindow():oCurrWindow
   SendOut( hb_jsonEncode( { "actdialog", oDlg:cName, Iif(Empty(lNoModal),"f","t"), { Iif(Empty(lCenter),"f","t") } } ) )
   IF Empty( lNoModal )
      oDlg:lWait := .T.
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
      SendOut( hb_jsonEncode( { "menu", aMenu } ) )
      aMenu := aMenuStack := Nil
   ELSE
      aMenuStack := ASize( aMenuStack, Len(aMenuStack)-1 )
   ENDIF

   RETURN Nil

FUNCTION eGUI_AddMenuItem( cName, id, cFunc, ... )

   LOCAL aSubMenu, arr

   IF Empty( aMenu )
      RETURN Nil
   ENDIF

   aSubMenu := Iif( Empty(aMenuStack), aMenu, ATail(aMenuStack) )
   Aadd( aSubMenu, { cName, CreateCodeString( cName, cFunc, "menu", "pgo", hb_aParams(), 4 ), id } )

   RETURN Nil

FUNCTION eGUI_AddCheckMenuItem( cName, id, cFunc, ... )

   LOCAL aSubMenu, arr

   IF Empty( aMenu )
      RETURN Nil
   ENDIF

   aSubMenu := Iif( Empty(aMenuStack), aMenu, ATail(aMenuStack) )
   Aadd( aSubMenu, { cName, CreateCodeString( cName, cFunc, "menu", "pgo", hb_aParams(), 4 ), id, .T. } )

   RETURN Nil

FUNCTION eGUI_AddMenuSeparator()

   LOCAL aSubMenu

   IF Empty( aMenu )
      RETURN Nil
   ENDIF

   aSubMenu := Iif( Empty(aMenuStack), aMenu, ATail(aMenuStack) )
   Aadd( aSubMenu, { "-" } )

   RETURN Nil

FUNCTION eGUI_MenuItemEnable( cWndName, cMenuName, nItem, lValue )

   SendOut( hb_jsonEncode( { "menu", "enable", cWndName, cMenuName, nItem, lValue } ) )

   RETURN Nil

FUNCTION eGUI_MenuItemCheck( cWndName, cMenuName, nItem, lValue )

   SendOut( hb_jsonEncode( { "menu", "check", cWndName, cMenuName, nItem, lValue } ) )

   RETURN Nil

FUNCTION eGUI_EvalProc( cCode )

   SendOut( hb_jsonEncode( { "evalcode", cCode } ) )

   RETURN Nil

FUNCTION eGUI_CreateFont( cName, cFamily, nHeight, lBold, lItalic, lUnderline, lStrikeout, nCharset )

   LOCAL oFont := EFont():New( cName, cFamily, nHeight, lBold, lItalic, lUnderline, lStrikeout, nCharset )

   SendOut( hb_jsonEncode( { "crfont", oFont:cName, oFont:cFamily, ;
         oFont:nHeight, oFont:lBold, oFont:lItalic, oFont:lUnderline, oFont:lStrikeout, oFont:nCharset } ) )

   RETURN oFont

FUNCTION eGUI_CreateStyle( cName, aColors, nOrient, aCorners, nBorder, tColor, cBitmap )

   LOCAL oStyle := EStyle():New( cName, aColors, nOrient, aCorners, nBorder, tColor, cBitmap )

   SendOut( hb_jsonEncode( { "crstyle", oStyle:cName, oStyle:aColors, ;
         oStyle:nOrient, oStyle:aCorners, oStyle:nBorder, oStyle:tColor, oStyle:cBitmap } ) )

   RETURN oStyle

FUNCTION eGUI_CreateHighliter( cName, cCommands, cFuncs, cSComm, cMComm, lCase )

   SendOut( hb_jsonEncode( { "highl", cName, cCommands, cFuncs, cSComm, cMComm, lCase } ) )

   RETURN Nil

FUNCTION eGUI_InitPrinter( cName, cPrinter, lPreview, nFormType, lLandscape, cFunc, cMet )

   LOCAL oPrinter := EPrinter():New( cName, cPrinter )

   IF Valtype( cPrinter ) != "C"
      cPrinter := "..."
   ENDIF
   SendOut( hb_jsonEncode( { "prninit", oPrinter:cName, {cPrinter,!Empty(lPreview), ;
         nFormType,!Empty(lLandscape)}, cFunc, cMet } ) )

   RETURN ( oLastPrinter := oPrinter )

FUNCTION eGUI_EvalFunc( cCode )

   LOCAL cRes
   cRes := SendOut( hb_jsonEncode( { "evalcode", cCode, "t" } ) )
   IF !Empty(cRes) .AND. Left( cRes, 1 ) == '"' .AND. Right( cRes,1 ) == '"'
      cRes := Substr( cRes,2,Len(cRes)-2 )
   ENDIF

   RETURN Iif( Empty(cRes), "", cRes )

FUNCTION eGUI_GetValues( oWnd, aNames )

   LOCAL cRes, arr
   cRes := SendOut( hb_jsonEncode( { "getvalues", oWnd:cName, aNames } ) )
   IF !Empty(cRes)
      hb_jsonDecode( cRes, @arr )
      RETURN arr
   ENDIF

   RETURN Nil

FUNCTION eGUI_GetVersion( n )

   LOCAL cRes
   cRes := SendOut( hb_jsonEncode( { "getver", n } ) )
   IF !Empty(cRes) .AND. Left( cRes, 1 ) == '"' .AND. Right( cRes,1 ) == '"'
      cRes := Substr( cRes,2,Len(cRes)-2 )
   ENDIF

   RETURN Iif( Empty(cRes), "", cRes )

FUNCTION eGUI_MsgInfo( cMessage, cTitle, cFunc, cName )

   SendOut( hb_jsonEncode( { "common", "minfo", cFunc, cName, cMessage, cTitle } ) )

   RETURN Nil

FUNCTION eGUI_MsgStop( cMessage, cTitle, cFunc, cName )

   SendOut( hb_jsonEncode( { "common", "mstop", cFunc, cName, cMessage, cTitle } ) )

   RETURN Nil

FUNCTION eGUI_MsgYesNo( cMessage, cTitle, cFunc, cName )

   SendOut( hb_jsonEncode( { "common", "myesno", cFunc, cName, cMessage, cTitle } ) )

   RETURN Nil

FUNCTION eGUI_MsgGet( cMessage, cTitle, nStyle, cFunc, cName )

   nStyle := Iif( Empty(nStyle), 0, nStyle )
   SendOut( hb_jsonEncode( { "common", "mget", cFunc, cName, cMessage, cTitle, nStyle } ) )

   RETURN Nil

FUNCTION eGUI_Choice( arr, cTitle, cFunc, cName )

   SendOut( hb_jsonEncode( { "common", "mchoi", cFunc, cName, arr, cTitle } ) )

   RETURN Nil

FUNCTION eGUI_SelectFile( cPath, cFunc, cName )

   SendOut( hb_jsonEncode( { "common", "cfile", cFunc, cName, cPath } ) )

   RETURN Nil

FUNCTION eGUI_SelectColor( nColor, cFunc, cName )

   SendOut( hb_jsonEncode( { "common", "ccolor", cFunc, cName, nColor } ) )

   RETURN Nil

FUNCTION eGUI_SelectFont( cFunc, cName )

   LOCAL oFont := EFont():New( cName )
   SendOut( hb_jsonEncode( { "common", "cfont", cFunc, oFont:cName } ) )

   RETURN Nil

FUNCTION eGUI_InsertNode( oTree, cNodeName, cNodeNew, cTitle, cNodeNext, aImages, cProc, cCode )

   LOCAL cName := FullWidgName( oTree )

   IF !Empty( cProc )
      cCode := 'pgo("' + cProc + '",{"' + cName + '","' + cNodeNew + '"})'
   ENDIF

   SendOut( hb_jsonEncode( { "set", cName, "node", {cNodeName,cNodeNew,cTitle,cNodeNext,aImages,cCode} } ) )

   RETURN Nil

FUNCTION eGUI_PBarStep( oBar )

   LOCAL cName := FullWidgName( oBar )

   SendOut( hb_jsonEncode( { "set", cName, "step", 1 } ) )

   RETURN Nil

FUNCTION eGUI_PBarSet( oBar, nPos )

   LOCAL cName := FullWidgName( oBar )

   SendOut( hb_jsonEncode( { "set", cName, "setval", nPos } ) )

   RETURN Nil

FUNCTION eGUI_InitTray( cIcon, cMenuName, cTooltip )

   SendOut( hb_jsonEncode( { "tray", "init", cIcon, cMenuName, cTooltip } ) )

   RETURN Nil

FUNCTION eGUI_ModifyTrayIcon( cIcon )

   SendOut( hb_jsonEncode( { "tray", "icon", cIcon } ) )

   RETURN Nil

FUNCTION eGUI_RadioEnd( oRg, nSelected )

   LOCAL cName := FullWidgName( oRg )

   SendOut( hb_jsonEncode( { "set", cName, "radioend", nSelected } ) )

   RETURN Nil

FUNCTION eGUI_TabPage( oTab, cCaption )

   LOCAL cName := FullWidgName( oTab )

   SendOut( hb_jsonEncode( { "set", cName, "pagestart", cCaption } ) )

   RETURN Nil

FUNCTION eGUI_TabPageEnd( oTab )

   LOCAL cName := FullWidgName( oTab )

   SendOut( hb_jsonEncode( { "set", cName, "pageend", 1 } ) )

   RETURN Nil

FUNCTION eGUI_BrwSetArray( oBrw, arr )

   LOCAL cName := FullWidgName( oBrw )

   SendOut( hb_jsonEncode( { "set", cName, "brwarr", arr } ) )

   RETURN Nil

FUNCTION eGUI_BrwGetArray( oBrw )

   LOCAL cRes, arr
   cRes := SendOut( hb_jsonEncode( { "get", oBrw:cName, "brwarr" } ) )
   IF !Empty(cRes)
      hb_jsonDecode( cRes, @arr )
      RETURN arr
   ENDIF

   RETURN Nil

FUNCTION eGUI_BrwSetColumn( oBrw, nColumn, cHeadName, nAlignHead, nAlignData, lEditable, nLength )

   LOCAL cName := FullWidgName( oBrw )

   SendOut( hb_jsonEncode( { "set", cName, "brwcol", {nColumn, cHeadName, nAlignHead, nAlignData, lEditable, nLength} } ) )

   RETURN Nil


FUNCTION eGUI_BrwSetColumnEx( oBrw, nColumn, cParam, xParam )

   LOCAL cName := FullWidgName( oBrw )

   IF Valtype( xParam ) == "O" .AND. __ObjHasMsg( xParam, "CNAME" )
      SendOut( hb_jsonEncode( { "set", cName, "brwcolx", {nColumn,cParam,xParam:cName,.T.} } ) )
   ELSE
      SendOut( hb_jsonEncode( { "set", cName, "brwcolx", {nColumn,cParam,xParam} } ) )
   ENDIF

   RETURN Nil


FUNCTION eGUI_SetVar( cVarName, cValue )

   SendOut( hb_jsonEncode( { "setvar", cVarName, cValue } ) )

   RETURN Nil

FUNCTION eGUI_GetVar( cVarName )

   LOCAL cRes := SendOut( hb_jsonEncode( { "getvar", cVarName } ) )

   RETURN Substr( cRes,2,Len(cRes)-2 )

FUNCTION eGUI_SetImagePath( cPath )

   SendOut( hb_jsonEncode( { "setparam", "bmppath", cPath } ) )

   RETURN Nil

FUNCTION eGUI_SetPath( cPath )

   SendOut( hb_jsonEncode( { "setparam", "path", cPath } ) )

   RETURN Nil

FUNCTION eGUI_SetDateFormat( cFormat )

   SendOut( hb_jsonEncode( { "setparam", "datef", cFormat } ) )

   RETURN Nil


FUNCTION SendOut( s )

   IF lPacket
      cPacketBuff += ',' + s
   ELSE
      IF nConnType == 1
         RETURN gs_Send2SocketOut( '+' + s + cn )
      ELSEIF nConnType == 2
         RETURN conn_Send2SocketOut( '+' + s + cn )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION SendIn( s )

   IF nConnType == 1
      gs_Send2SocketIn( s )
   ELSEIF nConnType == 2
      conn_Send2SocketIn( s )
   ENDIF

   RETURN Nil

FUNCTION eGUI_BeginPacket()

   lPacket := .T.
   cPacketBuff := '["packet"'

   RETURN Nil

FUNCTION eGUI_EndPacket()

   lPacket := .F.
   SendOut( cPacketBuff + ']' )
   cPacketBuff := ""

   RETURN Nil

FUNCTION eGUI_Wait()

   DO WHILE Inkey(1) != 27
      //hb_idleSleep(1)
   ENDDO

   RETURN Nil

FUNCTION FIdle()

   LOCAL xRes, arr, arrp

   DO WHILE nRunProc > 0
      arr := aRunProc[1]
      DelRproc( 1 )

      xRes := &( "{|a|"+Lower(arr[1])+"(a)}" )
      hb_jsonDecode( arr[2], @arrp )
      Eval( xRes, arrp )
   ENDDO
   IF nConnType == 1
      gs_CheckSocket()
   ELSEIF nConnType == 2
      conn_CheckIn()
   ENDIF

   RETURN Nil

STATIC FUNCTION Add2Rproc( arr )

   IF ++ nRunProc > Len( aRunProc )
      aRunProc := ASize( aRunProc, Len(aRunProc)+5 )
   ENDIF
   aRunProc[nRunProc] := arr
   RETURN Nil

STATIC FUNCTION DelRproc( n )

   ADel( aRunProc, n )
   nRunProc --
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
         ELSEIF cProp == "anchor"
            sProps += ',"Anchor": ' + Ltrim(Str(aProps[i,2]))
         ELSEIF cProp == "picture"
            sProps += ',"Picture": "' + aProps[i,2] + '"'
         ELSEIF cProp == "transpa"
            sProps += ',"Transpa": "t"'
         ELSEIF cProp == "vertical"
            sProps += ',"Vertical": "t"'
         ELSEIF cProp == "image"
            sProps += ',"Image": "' + aProps[i,2] + '"'
         ELSEIF cProp == "aitems"
            sProps += ',"AItems": ' + hb_jsonEncode(aProps[i,2])
         ELSEIF cProp == "aparts"
            sProps += ',"AParts": ' + hb_jsonEncode(aProps[i,2])
         ELSEIF cProp == "hstyles" .OR. cProp == "aleft" .OR. cProp == "aright"
            sProps += ',"' + Iif( cProp == "hstyles", 'HStyles', ;
                  Iif( cProp == "aleft", 'ALeft','ARight' ) ) + '": ['
            FOR j := 1 TO Len(aProps[i,2])
               sProps += Iif( j==1,'"',',"' ) + ;
                  Iif(Valtype(aProps[i,2,j])=="O",aProps[i,2,j]:cName,aProps[i,2,j]) + '"'
            NEXT
            sProps += ']'
         ELSEIF cProp == "from"
            sProps += ',"From": ' + Ltrim(Str(aProps[i,2]))
         ELSEIF cProp == "to"
            sProps += ',"To": ' + Ltrim(Str(aProps[i,2]))
         ELSEIF cProp == "aimages"
            sProps += ',"AImages": ' + hb_jsonEncode(aProps[i,2])
         ELSEIF cProp == "editlabel"
            sProps += ',"Editlabel": "t"'
         ELSEIF cProp == "append"
            sProps += ',"Append": "t"'
         ELSEIF cProp == "autoedit"
            sProps += ',"Autoedit": "t"'
         ELSEIF cProp == "novscroll"
            sProps += ',"NoVScroll": "t"'
         ELSEIF cProp == "noborder"
            sProps += ',"NoBorder": "t"'
         ELSEIF cProp == "notoday"
            sProps += ',"NoToday": "t"'
         ELSEIF cProp == "notodaycirc"
            sProps += ',"NoTodayCirc": "t"'
         ELSEIF cProp == "weeknumb"
            sProps += ',"WeekNumb": "t"'
         ELSEIF cProp == "icon"
            sProps += ',"ICon": "' + aProps[i,2] + '"'
         ELSEIF cProp == "link"
            sProps += ',"Link": "' + aProps[i,2] + '"'
         ELSEIF cProp == "clrvisited"
            sProps += ',"ClrVisited": ' + Ltrim(Str(aProps[i,2]))
         ELSEIF cProp == "clrlink"
            sProps += ',"ClrLink": ' + Ltrim(Str(aProps[i,2]))
         ELSEIF cProp == "clrover"
            sProps += ',"ClrOver": ' + Ltrim(Str(aProps[i,2]))
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

FUNCTION eGUI_GetFont( cName )
   RETURN GetItemByName( EFont():aFonts, cName )

FUNCTION eGUI_GetStyle( cName )
   RETURN GetItemByName( EStyle():aStyles, cName )

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
      DO WHILE (nPos := At( ".", cWidgName )) != 0
         IF ( oWnd := GetItemByName( oWnd:aWidgets, Left( cWidgName, nPos-1 ) ) ) == Nil
            RETURN Nil
         ENDIF
         cWidgName := Substr( cWidgName, nPos+1 )
      ENDDO
      RETURN GetItemByName( oWnd:aWidgets, cWidgName )
   ENDIF

   RETURN Nil

FUNCTION FullWidgName( oWidg )

   LOCAL cName := oWidg:cName
   DO WHILE !Empty( oWidg:oParent )
      oWidg := oWidg:oParent
      cName := oWidg:cName + "." + cName
   ENDDO

   RETURN cName


FUNCTION eGUI_Writelog( cText, fname )

   LOCAL nHand

   fname := cInitialPath + IIf( fname == Nil, "extgui.log", fname )
   IF ! File( fname )
      nHand := FCreate( fname )
   ELSE
      nHand := FOpen( fname, 1 )
   ENDIF
   FSeek( nHand, 0, 2 )
   FWrite( nHand, cText + cn )
   FClose( nHand )

   RETURN Nil

FUNCTION CreateCodeString( cName, cFunc, cWidgName, cPgo, arr, n )

   LOCAL cCode, i

   IF !Empty( cFunc )
      cCode := cPgo + '("' + cFunc + '",{"' + cWidgName + '"'
      FOR i := n TO Len( arr )
         cCode += ',"' + arr[i] + '"'
      NEXT
      cCode += '})'
   ELSE
      cCode := arr[n]
   ENDIF
   RETURN cCode

FUNCTION MainHandler()

   LOCAL cBuffer, arr, arrp, cCommand, cFunc, lSend := .F., xRes, o

   IF nConnType == 1
      cBuffer := gs_GetRecvBuffer()
   ELSEIF nConnType == 2
      cBuffer := conn_GetRecvBuffer()
   ENDIF
   hb_jsonDecode( cBuffer, @arr )
   IF Valtype(arr) != "A" .OR. Empty(arr)
      SendIn( "+Err"+cn )
      RETURN Nil
   ENDIF

   cCommand := Lower( arr[1] )
   IF cCommand == "runproc"

      SendIn( "+Ok"+cn )
      lSend := .T.
      Add2Rproc( { arr[2], Iif( Len(arr)>2,arr[3],{} ) } )

   ELSEIF cCommand == "runfunc"

      cFunc := Lower( arr[2] )
      xRes := &( "{|a|"+cFunc+"(a)}" )
      IF Len( arr ) > 2
         hb_jsonDecode( arr[3], @arrp )
      ENDIF
      xRes := Eval( xRes, arrp )
      SendIn( "+" + hb_jsonEncode(xRes) + cn )
      lSend := .T.

   ELSEIF cCommand == "exit"
      IF !Empty( o := egui_GetWnd( arr[2] ) )
         o:lWait := .F.
         o:Delete()
      ENDIF
      lSend := .T.
      SendIn( "+Ok"+cn )

   ELSEIF cCommand == "endapp"
      lSend := .T.
      SendIn( "+Ok"+cn )
      eGUI_Exit()
      Quit

   ENDIF

   IF !lSend
      SendIn( "+Ok"+cn )
   ENDIF

   RETURN Nil

FUNCTION eGUI_Exit

   IF nConnType == 1
      gs_ipExit()
   ELSEIF nConnType == 2
      conn_Exit()
   ENDIF

   RETURN Nil
