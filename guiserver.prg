/*
 * GuiServer main file
 *
 * Copyright 2018 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */
#include "hwgui.ch"
#include "hwgextern.ch"

EXTERNAL DBCREATE, DBUSEAREA, DBCREATEINDEX, DBSEEK, DBCLOSEAREA, DBSELECTAREA, DBUNLOCK, DBUNLOCKALL
EXTERNAL BOF, EOF, DBF, DBAPPEND, DBCLOSEALL, DBCLOSEAREA, DBCOMMIT,DBCOMMITALL, DBCREATE
EXTERNAL DBDELETE, DBFILTER, DBSETFILTER, DBGOBOTTOM, DBGOTO, DBGOTOP, DBRLOCK, DBRECALL, DBDROP, DBEXISTS
EXTERNAL DBRLOCKLIST, DBRUNLOCK, LOCK, RECNO,  DBSETFILTER, DBFILEGET, DBFILEPUT, FIELDBLOCK
EXTERNAL DBSKIP, DBSTRUCT, DBTABLEEXT, DELETED, DBINFO, DBORDERINFO, DBRECORDINFO
EXTERNAL FCOUNT, FIELDDEC, FIELDGET, FIELDNAME, FIELDLEN, FIELDPOS, FIELDPUT
EXTERNAL FIELDTYPE, FLOCK, FOUND, HEADER, LASTREC, LUPDATE, NETERR, AFIELDS
EXTERNAL RECCOUNT, RECSIZE, SELECT, ALIAS, RLOCK
EXTERNAL __DBZAP, USED, RDDSETDEFAULT, __DBPACK, __DBAPP, __DBCOPY
EXTERNAL DBFCDX, DBFFPT
EXTERNAL FOPEN, FCLOSE, FSEEK, FREAD, FWRITE, FERASE
EXTERNAL HB_BITAND, HB_BITSHIFT
EXTERNAL ASORT, ASCAN

EXTERNAL HB_FNAMEDIR, HB_FNAMENAME

REQUEST HB_CODEPAGE_UTF8
REQUEST HB_CODEPAGE_RU1251
REQUEST HB_STRTOUTF8, HB_TRANSLATE

#ifdef __GTK__
REQUEST HB_GT_CGI_DEFAULT
#else
ANNOUNCE HB_GTSYS
REQUEST HB_GT_GUI_DEFAULT
#endif

STATIC nPort := 3101
STATIC lEnd := .F., oMTimer := Nil, nInterval := 20
STATIC cn := e"\n"
STATIC cLogFile := "guiserver.log"

/*
 * GuiServer
 */

STATIC oMainWnd, oCurrWindow, cCurrwindow := ""
STATIC cDefPath := ""

FUNCTION Main( ... )

   LOCAL i, aParams := hb_aParams(), c, sp

   IF hwg__isUnicode()
      hb_cdpSelect( "UTF8" )
   ENDIF

   FOR i := 1 TO Len( aParams )
      IF Left( aParams[i],1 ) $ "-/"
         sp := Substr( aParams[i],3 )
         IF ( c := Lower( Substr( aParams[i],2,1 ) ) ) == "p"
            IF !Empty(sp) .AND. IsDigit(sp)
               nPort := Val( sp )
            ENDIF
         ENDIF
      ENDIF
   NEXT

   ipInit()

   SetLogFile( "ac.log" )
   SetVersion( "1.1" )
   SetHandler( "MAINHANDLER" )
   hwg_writelog( "Start at port "+ Ltrim(Str(nPort)),cLogFile )
   CreateSocket( nPort )

   DO WHILE !lEnd
      Sleep_ns( nInterval )
      ListenSocket()
      CheckSocket()
   ENDDO

   Send2SocketOut( '+["endapp"]' + cn )
   Sleep_ns( nInterval*2 )
   ipExit()
   Sleep_ns( nInterval*2 )

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

   Send2SocketOut( "+" + hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( aParams ) } ) + cn )

   RETURN Nil

FUNCTION fGO( cFunc, aParams )

   LOCAL cRes := Send2SocketOut( "+" + hb_jsonEncode( { "runfunc", cFunc, hb_jsonEncode( aParams ) } ) + cn )

   IF Substr( cRes,2,1 ) == '"'
      RETURN Substr( cRes, 3, Len(cRes)-4 )
   ENDIF
   RETURN Substr( cRes, 2, Len(cRes)-2 )

STATIC FUNCTION CrMainWnd( arr, hash )

   LOCAL x1 := arr[1], y1 := arr[2], w := arr[3], h := arr[4]
   LOCAL cTitle := arr[5]
   LOCAL nStyle, bColor, oFont
   LOCAL bExit := {||
      LOCAL lRes := .T.
      IF lRes
         oMTimer:End()
         oMTimer := Nil
         Send2SocketOut( '+["exit","main"]' + cn )
      ENDIF

      RETURN lRes
   }

   IF !Empty( hash )
      nStyle := hb_hGetDef( hash, "Winstyle", Nil )
      bColor := hb_hGetDef( hash, "BColor", Nil )
      IF hb_hHaskey( hash, "Font" )
         oFont := GetFont( hash["Font"] )
      ENDIF
   ENDIF

   INIT WINDOW oMainWnd MAIN TITLE cTitle AT x1,y1 SIZE w,h STYLE nStyle ;
         BACKCOLOR bColor FONT oFont ON EXIT bExit
   oCurrWindow := oMainWnd
   cCurrWindow := "MAIN"

   RETURN Nil

STATIC FUNCTION ActMainWnd( arr )

   SET TIMER oMTimer OF oMainWnd VALUE nInterval ACTION {||TimerFunc()}

   ACTIVATE WINDOW oMainWnd

   RETURN Nil

STATIC FUNCTION CrDialog( cName, arr, hash )

   LOCAL oDlg
   LOCAL x1 := arr[1], y1 := arr[2], w := arr[3], h := arr[4]
   LOCAL cTitle := arr[5]
   LOCAL nStyle, bColor, oFont
   LOCAL bInit := {|o|
      IF oMTimer == Nil
         //hwg_writelog( "DActTimer "+str(millisec()),cLogFile )
         SET TIMER oMTimer OF oDlg VALUE nInterval ACTION {||TimerFunc()}
      ENDIF
      RETURN .T.
   }
   LOCAL bExit := {|o|
      LOCAL lRes := .T.
      IF lRes
         IF oMTimer != Nil
            //hwg_writelog( "Dexit "+str(millisec()),cLogFile )
            oMTimer:End()
            oMTimer := Nil
         ENDIF
         IF !(HWindow():GetMain() == Nil .AND. ;
            ( Len(HDialog():aModalDialogs) + Len(HDialog():aDialogs) == 1 ))
            SET TIMER oMTimer OF oMainWnd VALUE nInterval ACTION {||TimerFunc()}
         ENDIF
         /*
         IF HWindow():GetMain() == Nil .AND. ;
            ( Len(HDialog():aModalDialogs) + Len(HDialog():aDialogs) == 1 )
            oMTimer:End()
            oMTimer := Nil
         ENDIF
         */
         Send2SocketOut( '+["exit","' + o:objName + '"]' + cn )
      ENDIF

      RETURN lRes
   }

   IF !Empty( hash )
      nStyle := hb_hGetDef( hash, "Winstyle", Nil )
      bColor := hb_hGetDef( hash, "BColor", Nil )
      IF hb_hHaskey( hash, "Font" )
         oFont := GetFont( hash["Font"] )
      ENDIF
   ENDIF

   INIT DIALOG oDlg TITLE cTitle AT x1,y1 SIZE w,h STYLE nStyle ;
         BACKCOLOR bColor FONT oFont ON INIT bInit ON EXIT bExit
   cCurrWindow := oDlg:objname := Upper( cName )
   oCurrWindow := oDlg

   RETURN Nil

STATIC FUNCTION ActDialog( cName, cNoModal )

   LOCAL oDlg

   cName := Upper( cName )
   IF ( oDlg := oCurrWindow ) != Nil
      IF oMTimer != Nil
         oMTimer:End()
         oMTimer := Nil
      ENDIF
      oDlg:Activate( (cNoModal=="t") )
   ENDIF

   RETURN Nil

STATIC FUNCTION CrFont( cName, cFamily, nHeight, lBold, lIta, lUnder, lStrike, nCharset )

   LOCAL oFont := HFont():Add( cFamily,, nHeight, Iif(lBold,700,400), ;
      Iif(nCharset>0,nCharset,Nil), Iif(lIta,1,0), Iif(lUnder,1,0), Iif(lStrike,1,0) )

   oFont:objname := cName

   RETURN Nil

STATIC FUNCTION CrStyle( cName, aColors, nOrient, aCorners, nBorder, tColor, cBitmap )

   LOCAL oStyle := HStyle():New( aColors, nOrient, aCorners, nBorder, tColor )

   oStyle:objname := cName

   RETURN Nil

STATIC FUNCTION WinClose( cName )

   LOCAL oDlg := GetWnd( cName )

   //hwg_writelog( "wclose-1: " + cName + Iif(!Empty( oDlg ), " T"," F" ),cLogFile )
   IF !Empty( oDlg )
      oDlg:Close()
   ENDIF

   RETURN Nil

STATIC FUNCTION GetValues( cName, aNames )

   LOCAL oWnd := GetWnd( cName ), i, oWidg, xRes, ares := {}

   IF Empty( oWnd )
      RETURN Nil
   ENDIF
   FOR i := 1 TO Len( aNames )
      IF !Empty( oWidg := GetWidg( aNames[i], oWnd ) )
         IF __ObjHasMsg( oWidg, "VALUE" )
            xRes := oWidg:Value()
         ELSEIF Valtype(oWidg:cargo) == "O" .AND. __ObjHasMsg( oWidg:cargo, "VALUE" )
            xRes := oWidg:cargo:Value()
         ELSE
            xRes := oWidg:GetText()
         ENDIF
         Aadd( ares, CnvVal( xRes ) )
      ELSE
         Aadd( ares, CnvVal( Nil ) )
      ENDIF
   NEXT

   RETURN ares

STATIC FUNCTION SetMenu( arr )

   MENU OF oCurrWindow
   SetMenu2( arr )
   ENDMENU

   RETURN Nil

STATIC FUNCTION SetMenu2( arr )

   LOCAL i, arr1

   FOR i := 1 TO Len( arr )
      arr1 := arr[i]
      IF Len(arr1) > 1 .AND. Valtype( arr1[2] ) == "A"
         MENU TITLE arr1[1]
         SetMenu2( arr1[2] )
         ENDMENU
      ELSEIF arr1[1] == "-"
         SEPARATOR
      ELSE
         Hwg_DefineMenuItem( arr1[1],, SetCB( , arr1[2] ), .F.,,,,, .f. )
      ENDIF
   NEXT

   RETURN Nil


STATIC FUNCTION AddWidget( cWidg, cName, arr, hash )

   LOCAL oParent, oCtrl, nPos := Rat( '.', cName ), i, xTemp
   LOCAL x1 := arr[1], y1 := arr[2], w := arr[3], h := arr[4], cCaption := arr[5]
   LOCAL nStyle, tColor, bColor, cTooltip, cPicture, lTransp, bSize, oFont, oStyle
   LOCAL cImage, lResou, trColor, aItems, lEdit, lText, nDisplay, lVert
   LOCAL lFlat, lCheck, aStyles, aParts

   oParent := GetWidg( Left( cName, nPos-1 ) )
   cName := Substr( cName, nPos+1 )

   IF Valtype(x1) != "N"; x1 := 0; ENDIF
   IF Valtype(y1) != "N"; y1 := 0; ENDIF
   IF Valtype(w) != "N"; w := 0; ENDIF
   IF Valtype(h) != "N"; h := 0; ENDIF

   IF !Empty( hash )
      nStyle := hb_hGetDef( hash, "Winstyle", Nil )
      tColor := hb_hGetDef( hash, "TColor", Nil )
      bColor := hb_hGetDef( hash, "BColor", Nil )
      cTooltip := hb_hGetDef( hash, "Tooltip", Nil )
      bSize := hb_hGetDef( hash, "Anchor", Nil )
      cPicture := hb_hGetDef( hash, "Picture", Nil )
      lTransp := Iif( hb_hHaskey( hash, "Transpa" ), .T., Nil )
      cImage := hb_hGetDef( hash, "Image", Nil )
      lVert := Iif( hb_hHaskey( hash, "Vertical" ), .T., Nil )
      IF hb_hHaskey( hash, "AItems" )
         aItems := hash["AItems"]
      ENDIF
      IF hb_hHaskey( hash, "Font" )
         oFont := GetFont( hash["Font"] )
      ENDIF
      IF hb_hHaskey( hash, "HStyle" )
         oStyle := GetStyle( hash["HStyle"] )
      ENDIF
   ENDIF

   i := Left( cWidg,1 )
   SWITCH i
   CASE 'l'
      IF cWidg == "label"

         oCtrl := HStatic():New( oParent,, nStyle, x1, y1, w, h, cCaption, oFont,, bSize,, ;
               cTooltip, tColor, bColor, lTransp )
      ELSEIF cWidg == "line"

         oCtrl := HLine():New( oParent,, lVert, x1, y1, w, bSize )
      ENDIF
      EXIT
   CASE 'e'
      IF cWidg == "edit"

         oCtrl := HEdit():New( oParent,, cCaption,, nStyle, x1, y1, w, h, oFont,, bSize,,, ;
               cTooltip, tColor, bColor, cPicture )
      ENDIF
      EXIT

   CASE 'b'
      IF cWidg == "button"

         oCtrl := HButton():New( oParent,, nStyle, x1, y1, w, h, cCaption, oFont,, bSize,, ;
               cTooltip, tColor, bColor )
      ELSEIF cWidg == "bitmap"

         oCtrl := HSayBmp():New( oParent,, x1, y1, w, h, cImage, lResou,,, cTooltip,,, ;
               lTransp,, trcolor, bColor )
      ENDIF
      EXIT

   CASE 'c'
      IF cWidg == "check"

         oCtrl := HCheckButton():New( oParent,,,, nStyle, x1, y1, w, h, ;
               cCaption, oFont,, bSize,,, cTooltip, tColor, bColor,, lTransp )
      ELSEIF cWidg == "combo"

         oCtrl := HCombobox():New( oParent,,,, nStyle, x1, y1, w, h, ;
               aItems, oFont,, bSize,,, cTooltip, lEdit, lText,, tcolor, bcolor,, nDisplay )
      ENDIF
      EXIT

   CASE 'r'
      IF cWidg == "radio"

         oCtrl := HRadioButton():New( oParent,, nStyle, x1, y1, w, h, ;
               cCaption, oFont,, bSize,,, cTooltip, tColor, bColor,, lTransp )

      ELSEIF cWidg == "radiogr"

         oCtrl := HRadioGroup():NewRg( oParent,, nStyle,,, x1, y1, w, h, ;
               cCaption, oFont,, bSize, tcolor, bColor )
         oCtrl:oHGroup:objName := Upper( cName )
         oCtrl:oHGroup:cargo := oCtrl
      ENDIF
      EXIT

   CASE 'g'
      IF cWidg == "group"

         oCtrl := HGroup():New( oParent,, nStyle, x1, y1, w, h, cCaption, oFont,, bSize,, tColor, bColor )
      ENDIF
      EXIT

   CASE 'o'
      IF cWidg == "ownbtn"

         IF !Empty( hash )
            IF hb_hHaskey( hash, "HStyles" )
               IF Valtype( xTemp := hash["HStyles"] ) == "A"
                  aStyles := {}
                  FOR i := 1 TO Len( xTemp )
                     Aadd( aStyles, GetStyle( xTemp[i] ) )
                  NEXT
               ENDIF
            ENDIF
         ENDIF
         oCtrl := HOwnButton():New( oParent,, aStyles, x1, y1, w, h,,,,, lFlat, ;
               cCaption, tcolor, oFont,,,,, cImage, lResou,,,,, lTransp, trColor, ;
               cTooltip,, lCheck, bColor )
      ENDIF
      EXIT

   CASE 'p'
      IF cWidg == "panel"

         oCtrl := HPanel():New( oParent,, nStyle, x1, y1, w, h,, bSize,, bcolor, oStyle )
      ELSEIF cWidg == "paneltop"

         oCtrl := HPanel():New( oParent,, nStyle, 0, 0, oParent:nWidth, h,, ANCHOR_TOPABS+ANCHOR_LEFTABS+ANCHOR_RIGHTABS,, bcolor, oStyle )

      ELSEIF cWidg == "panelbot"

         IF !Empty( hash )
            IF hb_hHaskey( hash, "AParts" )
               aParts := hash["AParts"]
            ENDIF
         ENDIF
         oCtrl := HPanelSts():New( oParent,, h, oFont,,, bcolor, oStyle, aParts )
      ENDIF
      EXIT

   END

   IF !Empty(cName) .AND. !Empty(oCtrl)
      oCtrl:objName := Upper( cName )
   ENDIF

   RETURN Nil

STATIC FUNCTION SetProperty( cWidgName, cPropName,  xProp )

   LOCAL oWnd, lWidg := .T.

   //A window or a dialog ?
   lWidg := ("." $ cWidgName)

   oWnd := GetWidg( cWidgName )
   IF Empty( oWnd )
      RETURN .F.
   ENDIF

   IF Left( cPropName,3 ) == "cb."
      SetCallback( oWnd, Substr(cPropName,4), xProp )
   ELSEIF cPropName == "text"
      IF lWidg
         oWnd:SetText( xProp )
      ELSE
         oWnd:SetTitle( xProp )
      ENDIF

   ELSEIF cPropName == "value"
      IF __ObjHasMsg( oWnd, "SETVALUE" )
         oWnd:SetValue( xProp )
      ELSE
         oWnd:SetText( xProp )
      ENDIF

   ELSEIF cPropName == "color"
      IF Valtype(xProp) == "A"
         oWnd:SetColor( Iif(xProp[1]!=Nil.AND.xProp[1]==-1,Nil,xProp[1]), ;
            Iif(xProp[2]!=Nil.AND.xProp[2]==-1,Nil,xProp[2]), .T. )
      ENDIF
   ELSEIF cPropName == "image"
      IF __ObjHasMsg( oWnd, "REPLACEBITMAP" )
         oWnd:ReplaceBitmap( xProp )
      ENDIF
   ENDIF

   RETURN .T.

STATIC FUNCTION SetCB( oWidg, cCode )
   LOCAL aScript
   LOCAL block := {||
      Return DoScript( aScript, {oWidg} )
   }

   IF cn $ cCode
      aScript := RdScript( ,cCode )
   ELSE
      cCode := AllTrim(cCode)
      IF Lower(Left(cCode,7)) == "return "
         cCode := Ltrim( Substr( cCode,8 ) )
      ENDIF
      block := &( "{||" + cCode + "}" )
   ENDIF

   RETURN block

STATIC FUNCTION SetCallback( oWidg, cbName, cCode )

   LOCAL block
   IF !__ObjHasMsg( oWidg, "B" + Substr(Upper(cbname),3) )
      RETURN .F.
   ENDIF

   block := SetCB( oWidg, cCode )

   IF cbName == "oninit"
      oWidg:bInit := block
   ELSEIF cbName == "onclick"
      oWidg:bClick := block
   ENDIF

   RETURN .T.

STATIC FUNCTION SetParam( cName, xValue )

   IF cName == "bmppath"
      HBitmap():cPath := HIcon():cPath := xValue

   ELSEIF cName == "path"
      SET DEFAULT TO &xValue
      SET PATH TO &xValue
      cDefPath := xValue

   ENDIF

   RETURN Nil

STATIC FUNCTION GetProperty( cWidgName, cPropName )

   LOCAL oWnd, lWidg := .T., cRes

   //A window or a dialog ?
   lWidg := ("." $ cWidgName)

   oWnd := GetWidg( cWidgName )
   IF Empty( oWnd )
      RETURN .F.
   ENDIF

   IF cPropName == "text"
      IF lWidg
         IF __ObjHasMsg( oWnd, "GETTEXT" )
            cRes := oWnd:GetText()
         ELSE
            cRes := ""
         ENDIF
      ELSE
         cRes := hwg_Getwindowtext( oWnd:handle )
      ENDIF

   ELSEIF cPropName == "value"
      IF __ObjHasMsg( oWnd, "VALUE" )
         cRes := CnvVal( oWnd:Value() )
      ELSEIF Valtype(oWnd:cargo) == "O" .AND. __ObjHasMsg( oWnd:cargo, "VALUE" )
         cRes := CnvVal( oWnd:cargo:Value() )
      ELSE
         cRes := oWnd:GetText()
      ENDIF
      
   ELSEIF cPropName == "color"
   ENDIF

   RETURN cRes


STATIC FUNCTION f_MsgInfo( cMess, cTitle, cFunc, cName )

   hwg_MsgInfo( cMess, cTitle )
   IF !Empty( cFunc )
      Send2SocketOut( "+" + hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName} ) } ) + cn )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_MsgStop( cMess, cTitle, cFunc, cName )

   hwg_MsgStop( cMess, cTitle )
   IF !Empty( cFunc )
      Send2SocketOut( "+" + hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName} ) } ) + cn )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_MsgYesNo( cMess, cTitle, cFunc, cName )

   LOCAL lYes := hwg_MsgYesNo( cMess, cTitle )
   IF !Empty( cFunc )
      Send2SocketOut( "+" + hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,Iif(lYes,"t","f")} ) } ) + cn )
   ENDIF

   RETURN Nil


STATIC FUNCTION f_SeleFile( cPath, cFunc, cName )

   LOCAL fname
#ifdef __PLATFORM__UNIX
   fname := hwg_SelectfileEx( ,, { { "All files", "*" } } )
#else
   fname := hwg_Selectfile( { "All files" }, { "*.*" }, cPath )
#endif

   IF !Empty( cFunc )
      IF Empty( fname )
         fname := ""
      ENDIF
      Send2SocketOut( "+" + hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,fname} ) } ) + cn )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_SeleFont( cFunc, cName )

   LOCAL oFont := HFont():Select()
   IF !Empty( cFunc )
      IF Empty( oFont )
         Send2SocketOut( "+" + hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,""} ) } ) + cn )
      ELSE
         Send2SocketOut( "+" + hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( ;
            {cName,oFont:name,oFont:height,(oFont:weight>400),(oFont:italic>0), ;
            (oFont:underline>0),(oFont:strikeout>0),oFont:charset } ) } ) + cn )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION f_SeleColor( nColor, cFunc, cName )

   nColor := Hwg_ChooseColor( nColor,.F. )
   IF !Empty( cFunc )
      Send2SocketOut( "+" + hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,Iif(nColor==Nil,"",Ltrim(Str(nColor)))} ) } ) + cn )
   ENDIF

   RETURN Nil

STATIC FUNCTION GetItemByName( arr, cName )

   LOCAL oItem
   FOR EACH oItem IN arr
      IF !Empty( oItem:objname ) .AND. oItem:objname == cName
         RETURN oItem
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION GetStyle( cName )

   RETURN GetItemByName( HStyle():aStyles, cName )

STATIC FUNCTION GetFont( cName )

   RETURN GetItemByName( HFont():aFonts, cName )

STATIC FUNCTION GetWnd( cName )

   LOCAL oWnd

   cName := Upper( cName )
   IF cName == cCurrwindow
      RETURN oCurrwindow
   ENDIF
   IF cName == "MAIN"
      RETURN oMainWnd
   ELSE
      IF ( oWnd := GetItemByName( HDialog():aModalDialogs, cName ) ) == Nil
         RETURN GetItemByName( HDialog():aDialogs, cName )
      ENDIF
   ENDIF
   RETURN oWnd
   
FUNCTION GetWidg( cWidgName, oWnd )

   LOCAL nPos

   IF oWnd == Nil
      IF (nPos := At( ".", cWidgName )) == 0
         RETURN GetWnd( cWidgName )
      ELSE
         oWnd := GetWnd( Left( cWidgName, nPos-1 ) )
         cWidgName := Upper(Substr( cWidgName, nPos+1 ))
      ENDIF
   ELSE
      cWidgName := Upper( cWidgName )
   ENDIF
   IF !Empty( oWnd )
      IF (nPos := At( ".", cWidgName )) != 0
         IF ( oWnd := GetItemByName( oWnd:aControls, Left( cWidgName, nPos-1 ) ) ) == Nil
            RETURN Nil
         ENDIF
         cWidgName := Substr( cWidgName, nPos+1 )
      ENDIF
      RETURN GetItemByName( oWnd:aControls, cWidgName )
   ENDIF

   RETURN Nil


STATIC FUNCTION SetFormTimer( oForm )

   LOCAL i, cType, blockI, blockE
   LOCAL bInit := {||
      oMainWnd := HWindow():GetMain()
      SET TIMER oMTimer OF oMainWnd VALUE nInterval ACTION {||TimerFunc()}
      IF !Empty( blockI )
         Eval( blockI )
      ENDIF
      RETURN Nil
   }
   LOCAL bExit := {||
      LOCAL lRes := .T.
      IF !Empty( blockE )
         lRes := Eval( blockE )
      ENDIF
      IF lRes
         oMTimer:End()
         oMTimer := Nil
         Send2SocketOut( '+["exit","main"]' + cn )
      ENDIF

      RETURN lRes
   }

   FOR i := 1 TO Len( oForm:aMethods )
      IF oForm:aMethods[ i,1 ] == "ondlginit"
         IF ( cType := ValType( oForm:aMethods[ i,2 ] ) ) == "B"
            blockI := oForm:aMethods[ i,2 ]
         ELSEIF cType == "A"
            blockI := oForm:aMethods[ i,2,1 ]
         ENDIF
         oForm:aMethods[ i,2 ] := bInit
      ELSEIF oForm:aMethods[ i,1 ] == "ondlgexit"
         IF ( cType := ValType( oForm:aMethods[ i,2 ] ) ) == "B"
            blockE := oForm:aMethods[ i,2 ]
         ELSEIF cType == "A"
            blockE := oForm:aMethods[ i,2,1 ]
         ENDIF
         oForm:aMethods[ i,2 ] := bExit
      ENDIF
   NEXT
   IF Empty( blockI )
      Aadd( oForm:aMethods, { "ondlginit", bInit } )
   ENDIF
   IF Empty( blockE )
      Aadd( oForm:aMethods, { "ondlgexit", bExit } )
   ENDIF

   RETURN Nil

STATIC FUNCTION TimerFunc()

   //hwg_writelog( "timer: " + Str(Millisec()) )
   CheckSocket()
   RETURN Nil

FUNCTION MainHandler()

   LOCAL cBuffer := GetRecvBuffer()
   LOCAL arr, c, cCommand, cRes
   LOCAL oForm, oWnd, lErr := .F.

   hwg_writelog( "> "+ cBuffer,cLogFile )
   IF Left( cBuffer,1 ) == "+"

      hb_jsonDecode( Substr(cBuffer,2), @arr )
      IF Valtype(arr) != "A" .OR. Empty(arr)
         Send2SocketIn( "+Wrong" + cn )
         RETURN Nil
      ENDIF

      cCommand := Lower( arr[1] )
      c := Left( cCommand, 1 )
      SWITCH c
      CASE 's'
         IF cCommand == "set"
            lErr := ( Len(arr)<4 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               SetProperty( Lower(arr[2]), Lower(arr[3]),  arr[4] )
            ENDIF
         ELSEIF cCommand == "setparam"
            lErr := ( Len(arr)<3 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               SetParam( Lower(arr[2]), arr[3] )
            ENDIF
         ENDIF
         EXIT

      CASE 'g'
         IF cCommand == "get"
            lErr := ( Len(arr)<3 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               cRes := GetProperty( arr[2], Lower(arr[3]) )
               Send2SocketIn( "+" + hb_jsonEncode(cRes) + cn )
            ENDIF

         ELSEIF cCommand == "getvalues"
            lErr := ( Len(arr)<3 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               arr := GetValues( arr[2], arr[3] )
               Send2SocketIn( "+" + hb_jsonEncode(arr) + cn )
            ENDIF
         ENDIF
         EXIT

      CASE 'a'
         IF cCommand == "addwidg"

            lErr := ( Len(arr)<4 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               AddWidget( Lower(arr[2]), Lower(arr[3]),  arr[4], Iif( Len(arr)>4,arr[5],Nil ) )
            ENDIF
         ELSEIF cCommand == "actmainwnd"
            lErr := ( Len(arr)<2 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               ActMainWnd( arr[2] )
               lEnd := .T.
            ENDIF

         ELSEIF cCommand == "actdialog"

            lErr := ( Len(arr)<3 .OR. Valtype(arr[2]) != "C" .OR. Valtype(arr[3]) != "C" )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               ActDialog( arr[2], arr[3] )
               lEnd := .T.
            ENDIF
            lEnd := .T.
         ENDIF
         EXIT

      CASE 'e'
         IF cCommand == "evalcode"
            lErr := ( Len(arr)<2 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               IF Len( arr ) > 2 .AND. Lower( arr[3] ) == "t"
                  cRes := DoScript( RdScript( ,arr[2] ) )
                  Send2SocketIn( "+" + hb_jsonEncode(cRes) + cn )
               ELSE
                  Send2SocketIn( "+Ok" + cn )
                  DoScript( RdScript( ,arr[2] ) )
               ENDIF
            ENDIF
         ELSEIF cCommand == "exit"
            lEnd := .T.
            IF ( oWnd := HWindow():GetMain() ) != Nil
               oWnd:Close()
            ENDIF
            Send2SocketIn( "+Ok" + cn )
            ENDIF
         EXIT

      CASE 'o'
         IF cCommand == "openform"
            lErr := ( Len(arr)<2 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               oForm := HFormTmpl():Read( arr[2] )
               Send2SocketIn( "+Ok" + cn )
               oForm:Show()
            ENDIF
         ELSEIF cCommand == "openformmain"
            lErr := ( Len(arr)<2 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               oForm := HFormTmpl():Read( arr[2] )
               SetFormTimer( oForm )
               Send2SocketIn( "+Ok" + cn )
               oForm:ShowMain()
               lEnd := .T.
            ENDIF

         ELSEIF cCommand == "openreport"
            lErr := ( Len(arr)<2 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               oForm := HRepTmpl():Read( arr[2] )
               Send2SocketIn( "+Ok" + cn )
               oForm:Print()
            ENDIF
         ENDIF
         EXIT

      CASE 'm'
         IF cCommand == "menu"
            lErr := ( Len(arr)<2 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               SetMenu( arr[2] )
            ENDIF
         ENDIF
         EXIT

      CASE 'c'
         IF cCommand == "common"
            lErr := ( Len(arr)<4 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               IF arr[2] == "minfo"
                  f_MsgInfo( Iif(Len(arr)>4,arr[5],""), Iif(Len(arr)>5,arr[6],""), arr[3], arr[4] )
               ELSEIF arr[2] == "mstop"
                  f_MsgStop( Iif(Len(arr)>4,arr[5],""), Iif(Len(arr)>5,arr[6],""), arr[3], arr[4] )
               ELSEIF arr[2] == "myesno"
                  f_MsgYesNo( Iif(Len(arr)>4,arr[5],""), Iif(Len(arr)>5,arr[6],""), arr[3], arr[4] )
               ELSEIF arr[2] == "cfont"
                  f_selefont( arr[3], arr[4] )
               ELSEIF arr[2] == "cfile"
                  f_selefile( Iif(Len(arr)>4,arr[5],""), arr[3], arr[4] )
               ELSEIF arr[2] == "ccolor"
                  f_selecolor( Iif(Len(arr)>4,arr[5],""), arr[3], arr[4] )
               ENDIF
            ENDIF

         ELSEIF cCommand == "crmainwnd"
            lErr := ( Len(arr)<2 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               CrMainWnd( arr[2], Iif( Len(arr)>2,arr[3],Nil ) )
            ENDIF
         ELSEIF cCommand == "crdialog"
            lErr := ( Len(arr)<3 .OR. Valtype(arr[2]) != "C" .OR. Valtype(arr[3]) != "A" )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               CrDialog( arr[2], arr[3], Iif( Len(arr)>3,arr[4],Nil ) )
            ENDIF
         ELSEIF cCommand == "close"
            lErr := ( Len(arr)<2 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               WinClose( arr[2] )
            ENDIF

         ELSEIF cCommand == "crfont"
            lErr := ( Len(arr)<9 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               CrFont( arr[2], arr[3], arr[4], arr[5], arr[6], arr[7], arr[8], arr[9] )
            ENDIF

         ELSEIF cCommand == "crstyle"
            lErr := ( Len(arr)<8 )
            IF lErr
               Send2SocketIn( "+Err" + cn )
            ELSE
               Send2SocketIn( "+Ok" + cn )
               CrStyle( arr[2], arr[3], arr[4], arr[5], arr[6], arr[7], arr[8] )
            ENDIF
         ENDIF
         EXIT

      OTHERWISE
         lErr := .T.
         Send2SocketIn( "+Wrong" + cn )

      END
      IF lErr
         hwg_writelog( "Parsing error",cLogFile )
      ENDIF
   ELSE
      Send2SocketIn( "+Wrong" + cn )
   ENDIF

   RETURN Nil
