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
EXTERNAL FOPEN, FCREATE, FCLOSE, FSEEK, FREAD, FWRITE, FERASE
EXTERNAL HB_BITAND, HB_BITSHIFT, HB_BITNOT, HB_BITOR, HB_BITRESET, HB_BITSET, HB_BITTEST, HB_BITXOR
EXTERNAL ASORT, ASCAN
EXTERNAL MEMOREAD, MEMOWRIT, HB_MEMOWRIT
EXTERNAL HB_FNAMEDIR, HB_FNAMENAME, HB_FNAMEEXT, HB_FNAMENAMEEXT, HB_FNAMEEXTSET, HB_FNAMEEXTSETDEF
EXTERNAL HB_STRFORMAT, HB_HEXTONUM, HB_NUMTOHEX, HB_BASE64ENCODE, HB_BASE64DECODE
EXTERNAL HB_RAND32, HB_RANDOMINT, HB_RANDOM, HB_RANDOMSEED
EXTERNAL HB_GETENV

REQUEST HB_CODEPAGE_UTF8
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RUKOI8
REQUEST HB_STRTOUTF8, HB_TRANSLATE

#ifdef __GTK__
REQUEST HB_GT_CGI_DEFAULT
#else
ANNOUNCE HB_GTSYS
REQUEST HB_GT_GUI_DEFAULT
#endif

#define GUIS_VERSION   "1.4"

STATIC nConnType := 1
STATIC nPort := 3101
STATIC cFileRoot := "gs", cDir
STATIC lEnd := .F., oMTimer := Nil, nInterval := 20
STATIC cn := e"\n"
STATIC nLogOn := 0, cLogFile := "guiserver.log"

/*
 * GuiServer
 */

STATIC oMainWnd, oCurrWindow, cCurrwindow := ""
STATIC cDefPath := ""
STATIC aPrinters := {}
STATIC aHighLs := {}
STATIC aContextMenus := {}
STATIC aDeferred := {,,,,}, nDeferred := 0
STATIC lPanic := .F.

#ifdef GUIS_LIB

FUNCTION gs_Run( cExe, nLog, nType, cDir )

   IF Valtype( nLog ) == "N"
      nLogOn := nLog
      IF nLogOn > 1
         gs_SetLogFile( "ac.log" )
      ENDIF
   ENDIF
   gs_SetVersion( GUIS_VERSION )

   IF nType == Nil .OR. nType == 1
      nConnType := 1
      gs_ipInit()
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
   extgui_RunApp( cExe + Iif( !Empty(nType).AND.nType==2, " type=2", "" ) )

   RETURN Nil
#else

FUNCTION Main( ... )

   LOCAL i, aParams := hb_aParams(), c, sp

   IF hwg__isUnicode()
      hb_cdpSelect( "UTF8" )
   ENDIF
   SET CONFIRM ON

   FOR i := 1 TO Len( aParams )
      IF Left( aParams[i],1 ) $ "-/"
         IF !Empty( sp := Substr( aParams[i],3 ) )
            IF ( c := Lower( Substr( aParams[i],2,1 ) ) ) == "p"
               IF !Empty(sp) .AND. IsDigit(sp)
                  nPort := Val( sp )
               ENDIF
            ELSEIF c == 'l'
               IF sp == "og+" .OR. sp == "og2"
                  nLogOn := 2
               ELSEIF sp == "og1"
                  nLogOn := 1
               ENDIF
            ELSEIF c == 't'
               IF !Empty(sp) .AND. IsDigit(sp)
                  nConnType := Val( sp )
               ENDIF

            ELSEIF c == 'f'
               cFileRoot := sp

            ELSEIF c == 'd'
               cDir := sp
               IF !( Right( cDir,1 ) ) $ "\/"
                  cDir += hb_ps()
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   IF nConnType == 1
      gs_ipInit()
      IF nLogOn > 1
         gs_SetLogFile( "ac.log" )
      ENDIF
      gs_SetVersion( GUIS_VERSION )
      gs_SetHandler( "MAINHANDLER" )
      gWritelog( "Connect via ports " + Ltrim(Str(nPort)) + ", " + Ltrim(Str(nPort+1)) )
      gs_CreateSocket( nPort )

   ELSEIF nConnType == 2
      IF Empty( cDir )
         cDir := hb_DirTemp()
      ENDIF
      conn_SetVersion( GUIS_VERSION )
      gWritelog( "Connect via files "+ cDir + cFileRoot + ".*" )
      srv_conn_Create( cDir + cFileRoot, .T. )
   ENDIF

   DO WHILE !lEnd
      gs_Sleep_ns( nInterval )
      TimerFunc()
   ENDDO

   RETURN Nil

#endif

EXIT PROCEDURE GS_EXIT

   IF !lPanic
      SendOut( '["endapp"]' )
      gs_Sleep_ns( nInterval*2 )
      IF nConnType == 1
         gs_ipExit()
      ELSEIF nConnType == 2
         conn_Exit()
      ENDIF
      gs_Sleep_ns( nInterval*2 )
   ENDIF

   RETURN

STATIC FUNCTION Panic()

   LOCAL oWnd := HWindow():GetMain()

   lPanic := .T.
   IF oWnd != Nil
      oWnd:bDestroy := Nil
      IF !Empty( oMTimer )
         oMTimer:End()
      ENDIF
      oMTimer := Nil
      oWnd:Close()
   ENDIF

   gWritelog( "GuiServer closed due to socket error" )
   lEnd := .T.
   IF nConnType == 1
      gs_ipExit()
   ELSEIF nConnType == 2
      conn_Exit()
   ENDIF
   Quit

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

STATIC FUNCTION CrMainWnd( arr, hash )

   LOCAL x1 := arr[1], y1 := arr[2], w := arr[3], h := arr[4]
   LOCAL cTitle := arr[5]
   LOCAL nStyle, bColor, oFont, oIcon
   LOCAL bInit := {|o|
      IF Valtype(o:cargo) == "A" .AND. Valtype(o:cargo[1]) == "B"
         Eval( o:cargo[1], o )
      ENDIF
      RETURN .T.
   }
   LOCAL bExit := {|o|
      LOCAL lRes := .T.
      IF Valtype(o:cargo) == "A" .AND. Valtype(o:cargo[2]) == "B"
         lRes := Eval( o:cargo[2], o )
         lRes := Iif( Valtype(lRes)=="L", lRes, .T. )
      ENDIF
      IF lRes
         oMTimer:End()
         oMTimer := Nil
         SendOut( '["exit","main"]' )
      ENDIF

      RETURN lRes
   }

   IF !Empty( hash )
      nStyle := hb_hGetDef( hash, "Winstyle", Nil )
      bColor := hb_hGetDef( hash, "BColor", Nil )
      IF hb_hHaskey( hash, "Font" )
         oFont := GetFont( hash["Font"] )
      ENDIF
      IF hb_hHaskey( hash, "Icon" )
         oIcon := HIcon():AddFile( hash["Icon"] )
      ENDIF
   ENDIF

   INIT WINDOW oMainWnd MAIN TITLE cTitle AT x1,y1 SIZE w,h STYLE nStyle ;
         BACKCOLOR bColor FONT oFont ICON oIcon ON INIT bInit ON EXIT bExit
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
   LOCAL nStyle, bColor, oFont, oIcon
   LOCAL lExitOnEsc, lNoClosable
   LOCAL bInit := {|o|
      IF Valtype(o:cargo) == "A" .AND. Valtype(o:cargo[1]) == "B"
         Eval( o:cargo[1], o )
      ENDIF
      IF oMTimer == Nil
         SET TIMER oMTimer OF oDlg VALUE nInterval ACTION {||TimerFunc()}
      ENDIF
      RETURN .T.
   }
   LOCAL bExit := {|o|
      LOCAL lRes := .T.
      IF Valtype(o:cargo) == "A" .AND. Valtype(o:cargo[2]) == "B"
         lRes := Eval( o:cargo[2], o )
         lRes := Iif( Valtype(lRes)=="L", lRes, .T. )
      ENDIF
      IF lRes
         o:bDestroy := Nil
         IF oMTimer != Nil
            oMTimer:End()
            oMTimer := Nil
         ENDIF
         IF !(HWindow():GetMain() == Nil .AND. ;
            ( Len(HDialog():aModalDialogs) + Len(HDialog():aDialogs) == 1 ))
            SET TIMER oMTimer OF oMainWnd VALUE nInterval ACTION {||TimerFunc()}
         ENDIF
         SendOut( '["exit","' + o:objName + '"]' )

      ENDIF

      RETURN lRes
   }

   IF !Empty( hash )
      nStyle := hb_hGetDef( hash, "Winstyle", Nil )
      bColor := hb_hGetDef( hash, "BColor", Nil )
      IF hb_hHaskey( hash, "Font" )
         oFont := GetFont( hash["Font"] )
      ENDIF
      IF hb_hHaskey( hash, "Icon" )
         oIcon := HIcon():AddFile( hash["Icon"] )
      ENDIF
      lExitOnEsc := Iif( Left(hb_hGetDef( hash, "NoExitOnEsc","l"),1)=="t", .T., Nil )
      lNoClosable := Iif( Left(hb_hGetDef( hash, "NoCloseAble","l"),1)=="t", .T., Nil )
   ENDIF

   //INIT DIALOG oDlg TITLE cTitle AT x1,y1 SIZE w,h STYLE nStyle ;
   //      BACKCOLOR bColor FONT oFont ICON oIcon ON INIT bInit ON EXIT bExit

   oDlg := HDialog():New( WND_DLG_NORESOURCE, nStyle, x1, y1, w, h, cTitle, oFont, bInit, bExit,, ;
      ,,,,,, oIcon,,,, lExitOnEsc, bColor, lNoClosable )
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

   LOCAL oStyle := HStyle():New( aColors, nOrient, aCorners, nBorder, tColor, ;
      Iif( !Empty(cBitmap), HBitmap():AddFile(cBitmap), Nil ) )

   oStyle:objname := cName

   RETURN Nil

STATIC FUNCTION WinClose( cName )

   LOCAL oDlg := Wnd( cName )

   IF !Empty( oDlg )
      oDlg:Close()
   ENDIF

   RETURN Nil

STATIC FUNCTION GetValues( cName, aNames )

   LOCAL oWnd := Wnd( cName ), i, oWidg, xRes, ares := {}

   IF Empty( oWnd )
      RETURN Nil
   ENDIF
   FOR i := 1 TO Len( aNames )
      IF !Empty( oWidg := Widg( aNames[i], oWnd ) )
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

STATIC FUNCTION MenuContext( cmd, cName, arr )

   LOCAL oMenu, oWnd

   IF cmd == "create"
      CONTEXT MENU oMenu
      SetMenu2( arr )
      ENDMENU
      oMenu:objname := cName
      AAdd( aContextMenus, oMenu )
   ELSEIF cmd == "show"
      IF !Empty( oMenu := GetContextMenu( cName ) )
         IF !Empty( arr )
            oWnd := Wnd( arr )
         ENDIF
         oMenu:Show( Iif( Empty(oWnd), hwg_Getwindowobject(hwg_Getactivewindow()), oWnd ) )
      ENDIF
   ENDIF
   RETURN Nil

STATIC FUNCTION SetMenu2( arr )

   LOCAL i, arr1, id, lCheck

   FOR i := 1 TO Len( arr )
      arr1 := arr[i]
      IF Len(arr1) > 1 .AND. Valtype( arr1[2] ) == "A"
         MENU TITLE arr1[1]
         SetMenu2( arr1[2] )
         ENDMENU
      ELSEIF arr1[1] == "-"
         SEPARATOR
      ELSE
         id := Iif( Len(arr1)>2 .AND. !Empty(arr1[3]), arr1[3], Nil )
         lCheck := Iif( Len(arr1)>3 .AND. !Empty(arr1[4]), .T., .F. )
         Hwg_DefineMenuItem( arr1[1], id, SetCB( , arr1[2] ), .F.,,,,, lCheck )
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION SetMenuItem( cmd, cWnd, cMenu, nItem, lValue )

   LOCAL oMenu, oWnd

   IF Empty( cMenu )
      IF !Empty( oWnd :=  Iif( Empty( cWnd ), oMainWnd, Wnd( cWnd ) ) )
         IF cmd == "enable"
            hwg_Enablemenuitem( oWnd:handle, nItem, lValue, .T. )
         ELSEIF cmd == "check"
            hwg_Checkmenuitem( oWnd:handle, nItem, lValue )
         ENDIF
      ENDIF
   ELSEIF !Empty( oMenu := GetContextMenu( cMenu ) )
      IF cmd == "enable"
         hwg_Enablemenuitem( oMenu, nItem, lValue, .T. )
      ELSEIF cmd == "check"
         hwg_Checkmenuitem( oMenu, nItem, lValue )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION Get_setget()

   LOCAL x
   RETURN {|v|Iif(v==Nil,x,x:=v)}

STATIC FUNCTION AddWidget( cWidg, cName, arr, hash )

   LOCAL oParent, oCtrl, nPos := Rat( '.', cName ), i, xTemp
   LOCAL x1 := arr[1], y1 := arr[2], w := arr[3], h := arr[4], cCaption := arr[5]
   LOCAL nStyle, tColor, bColor, cTooltip, bSetGet, cPicture, lTransp, bSize, oFont, oStyle
   LOCAL cImage, lResou, trColor, aItems, lEdit, lText, nDisplay, lVert
   LOCAL lFlat, lCheck, aStyles, aParts
   LOCAL aLeft, aRight, nInit, nFrom, nTo, nMaxPos
   LOCAL lNoVScroll, lNoBorder, lAppend, lAutoedit
   LOCAL cLink, vColor, lColor, hColor
   LOCAL xt, yt, lBtnClose, lBtnMax, lBtnMin
   LOCAL lNoToday, lNoTodayCircle, lWeekNumbers

   oParent := Widg( Left( cName, nPos-1 ) )
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
      lTransp := Iif( Left(hb_hGetDef( hash, "Transpa","l"),1)=="t", .T., Nil )
      cImage := hb_hGetDef( hash, "Image", Nil )
      lVert := Iif( Left(hb_hGetDef( hash, "Vertical","l"),1)=="t", .T., Nil )
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

      ELSEIF cWidg == "link"

         IF !Empty( hash )
            cLink  := hb_hGetDef( hash, "Link", Nil )
            vColor := hb_hGetDef( hash, "ClrVisited", Nil )
            lColor := hb_hGetDef( hash, "ClrLink", Nil )
            hColor := hb_hGetDef( hash, "ClrOver", Nil )
         ENDIF
         IF Empty( cLink ) .AND. !Empty( cCaption )
            cLink := cCaption
         ELSEIF !Empty( cLink ) .AND. Empty( cCaption )
            cCaption := cLink
         ENDIF
         oCtrl := HStaticLink():New( oParent,, nStyle, x1, y1, w, h, cCaption, oFont,, bSize,, ;
               ctooltip, tcolor, bcolor, lTransp, cLink, vColor, lColor, hColor )
      ENDIF
      EXIT

   CASE 'e'
      IF cWidg == "edit"

         IF !Empty( cPicture )
            IF Left( cPicture,1 ) $ "ND"
               IF Valtype( cCaption ) == "C"
                  IF Left( cPicture,1 ) == "N"
                     cCaption := Val( cCaption )
                  ELSE
                     cCaption := Stod( cCaption )
                  ENDIF
               ENDIF
               cPicture := Substr( cPicture,2 )
            ENDIF
            bSetGet := Get_setget()
            Eval( bSetGet, cCaption )
         ENDIF
         oCtrl := HEdit():New( oParent,, cCaption, bSetGet, nStyle, x1, y1, w, h, oFont,, bSize,,, ;
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

      ELSEIF cWidg == "browse"
         IF !Empty( hash )
            lNoVScroll := Iif( Left(hb_hGetDef( hash, "NoVScroll","l"),1)=="t", .T., .F. )
            lNoBorder  := Iif( Left(hb_hGetDef( hash, "NoBorder","l"),1)=="t", .T., .F. )
         ENDIF
         oCtrl := HBrowse():New( 1, oParent,, nStyle, x1, y1, w, h, oFont, ;
            , bSize,,,,, lNoVScroll, lNoBorder, lAppend, lAutoedit,,,,, )
      ENDIF
      EXIT

   CASE 'c'
      IF cWidg == "check"

         oCtrl := HCheckButton():New( oParent,,,, nStyle, x1, y1, w, h, ;
               cCaption, oFont,, bSize,,, cTooltip, tColor, bColor,, lTransp )
      ELSEIF cWidg == "combo"

         IF !Empty( hash )
            IF hb_hHaskey( hash, "AItems" )
               aItems := hash["AItems"]
            ENDIF
         ENDIF
         oCtrl := HCombobox():New( oParent,,,, nStyle, x1, y1, w, h, ;
               aItems, oFont,, bSize,,, cTooltip, lEdit, lText,, tcolor, bcolor,, nDisplay )

      ELSEIF cWidg == "cedit"
         IF !Empty( hash )
            lNoVScroll := Iif( Left(hb_hGetDef( hash, "NoVScroll","l"),1)=="t", .T., .F. )
            lNoBorder  := Iif( Left(hb_hGetDef( hash, "NoBorder","l"),1)=="t", .T., .F. )
         ENDIF
         oCtrl := HCEdit():New( oParent,, nStyle, x1, y1, w, h, oFont, ;
            , bSize,, tcolor, bcolor,,, lNoVScroll, lNoBorder )
         IF hwg__isUnicode()
            oCtrl:lUtf8 := .T.
         ENDIF
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
            xt := hb_hGetDef( hash, "Xt", Nil )
            yt := hb_hGetDef( hash, "yt", Nil )
         ENDIF
         oCtrl := HOwnButton():New( oParent,, aStyles, x1, y1, w, h,, bSize,,, lFlat, ;
               cCaption, tcolor, oFont, xt, yt,,, cImage, lResou,,,,, lTransp, trColor, ;
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

      ELSEIF cWidg == "panelhead"

         IF !Empty( hash )
            xt := hb_hGetDef( hash, "Xt", Nil )
            yt := hb_hGetDef( hash, "yt", Nil )
            lBtnClose := Iif( Left(hb_hGetDef( hash, "BtnClose","l"),1)=="t", .T., .F. )
            lBtnMax := Iif( Left(hb_hGetDef( hash, "BtnMax","l"),1)=="t", .T., .F. )
            lBtnMin := Iif( Left(hb_hGetDef( hash, "BtnMin","l"),1)=="t", .T., .F. )
         ENDIF
         oCtrl := HPanelHea():New( oParent,, h, oFont,,, tcolor, bcolor, oStyle, ;
            cCaption, xt, yt, lBtnClose, lBtnMax, lBtnMin )

      ELSEIF cWidg == "progress"
         IF !Empty( hash )
            nMaxPos := hb_hGetDef( hash, "Maxpos", Nil )
         ENDIF
         oCtrl := HProgressBar():New( oParent,, x1, y1, w, h, nMaxPos, nMaxPos,, bSize,, cTooltip,, lVert )

      ENDIF
      EXIT

   CASE 's'
      IF cWidg == "splitter"
         IF !Empty( hash )
            IF hb_hHaskey( hash, "ALeft" )
               aLeft := hash["ALeft"]
               FOR i := 1 TO Len( aLeft )
                  aLeft[i] := Widg( aLeft[i], oParent )
               NEXT
            ENDIF
            IF hb_hHaskey( hash, "ARight" )
               aRight := hash["ARight"]
               FOR i := 1 TO Len( aRight )
                  aRight[i] := Widg( aRight[i], oParent )
               NEXT
            ENDIF
            nFrom := hb_hGetDef( hash, "From", Nil )
            nTo := hb_hGetDef( hash, "To", Nil )
         ENDIF
         oCtrl := HSplitter():New( oParent,, x1, y1, w, h, ;
               bSize,, tcolor, bcolor, aLeft, aRight, nFrom, nTo, oStyle )
      ENDIF
      EXIT

   CASE 't'
      IF cWidg == "tree"
         IF !Empty( hash )
            lEdit  := Iif( Left(hb_hGetDef( hash, "Editlabel","l"),1)=="t", .T., Nil )
            IF hb_hHaskey( hash, "AImages" )
               aItems := hash["AImages"]
            ENDIF
         ENDIF
         oCtrl := HTree():New( oParent,, nStyle, x1, y1, w, h, oFont,, ;
            bSize, tcolor, bcolor, aItems, lResou, lEdit )

      ELSEIF cWidg == "tab"
         oCtrl := HTab():New( oParent,, nStyle, x1, y1, w, h, oFont,, bSize )
      ENDIF
      EXIT

   CASE 'u'
      IF cWidg == "updown"
         IF !Empty( hash )
            nInit := hb_hGetDef( hash, "Init", 0 )
            nFrom := hb_hGetDef( hash, "From", Nil )
            nTo := hb_hGetDef( hash, "To", Nil )
         ENDIF
         oCtrl := HUpDown():New( oParent,, nInit,, nStyle, x1, y1, w, h, ;
            oFont,, bSize,,,, cTooltip, tcolor, bcolor,, nFrom, nTo )
      ENDIF
      EXIT

   CASE 'm'
      IF cWidg == "monthcal"
         IF !Empty( hash )
            lNoToday  := Iif( Left(hb_hGetDef( hash, "NoToday","l"),1)=="t", .T., Nil )
            lNoTodayCircle  := Iif( Left(hb_hGetDef( hash, "NoTodayCirc","l"),1)=="t", .T., Nil )
            lWeekNumbers  := Iif( Left(hb_hGetDef( hash, "WeekNumb","l"),1)=="t", .T., Nil )
         ENDIF
         oCtrl := HMonthCalendar():New( oParent,,, nStyle, x1, y1, w, h, ;
            oFont,,, cTooltip, lNoToday, lNoTodayCircle, lWeekNumbers )
      ENDIF
      EXIT

   END

   IF !Empty(cName) .AND. !Empty(oCtrl)
      oCtrl:objName := Upper( cName )
   ENDIF

   RETURN Nil

STATIC FUNCTION SetProperty( cWidgName, cPropName,  xProp )

   LOCAL oWnd, lWidg := .T., o, lErr := .F., i, j, l

   //A window or a dialog ?
   lWidg := ("." $ cWidgName)

   oWnd := Widg( cWidgName )
   IF Empty( oWnd )
      RETURN .F.
   ENDIF

   i := Left( cPropName,1 )
   SWITCH i
   CASE 'c'

      IF Left( cPropName,3 ) == "cb."
         SetCallback( oWnd, Substr(cPropName,4), xProp )
      ELSEIF cPropName == "color"
         lErr := !(Valtype(xProp) == "A")
         IF !lErr
            oWnd:SetColor( Iif(xProp[1]!=Nil.AND.xProp[1]==-1,Nil,xProp[1]), ;
               Iif(xProp[2]!=Nil.AND.xProp[2]==-1,Nil,xProp[2]), .T. )
         ENDIF
      ENDIF
      EXIT

   CASE 't'
      IF cPropName == "text"
         lErr := !(Valtype(xProp) == "C")
         IF !lErr
            IF lWidg
               oWnd:SetText( xProp )
            ELSE
               oWnd:SetTitle( xProp )
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE 'v'
      IF cPropName == "value"
         IF __ObjHasMsg( oWnd, "SETVALUE" )
            oWnd:SetValue( xProp )
         ELSE
            oWnd:SetText( xProp )
         ENDIF
      ENDIF
      EXIT

   CASE 'b'
      IF cPropName == "brwcol"
         lErr := !( __ObjHasMsg( oWnd, "INITBRW" )) .OR. Valtype(xProp) != "A" ;
               .OR. Len(xProp) < 2 .OR. Valtype(i := xProp[1]) != "N" .OR. ;
               i <= 0 .OR. i > Len( oWnd:aColumns )
         IF !lErr
            IF Valtype(xProp[2]) == "C" .AND. !Empty( xProp[2] )
               oWnd:aColumns[i]:heading := xProp[2]
            ENDIF
            IF Valtype(xProp[3]) == "N" .AND. xProp[3] > 0
               oWnd:aColumns[i]:nJusHead := xProp[3]
            ENDIF
            IF Valtype(xProp[4]) == "N" .AND. xProp[4] > 0
               oWnd:aColumns[i]:nJusLin := xProp[4]
            ENDIF
            IF Valtype(xProp[5]) == "L"
               oWnd:aColumns[i]:lEditable := xProp[5]
            ENDIF
            IF Len(xProp)>5 .AND. Valtype(xProp[6]) == "N" .AND. xProp[6] > 0
               oWnd:aColumns[i]:length := xProp[6]
            ENDIF
         ENDIF

      ELSEIF cPropName == "brwcolx"
         lErr := !(Valtype(xProp) == "A") .OR. !(Valtype(xProp[1]) == "N") .OR. ;
               Valtype(i := xProp[1]) != "N" .OR. i <= 0 .OR. i > Len( oWnd:aColumns ) ;
               .OR. !(Valtype(xProp[2]) == "C")
         IF !lErr
            l := .T.
            IF Len( xProp ) == 4
               IF (Valtype( xProp[4] ) == "L" .AND. xProp[4]) .OR. ;
                  (Valtype( xProp[4] ) == "C" .AND. xProp[4] == "o")
                  IF !Empty( o := GetStyle(xProp[3]) ) .OR. !Empty( o := GetFont(xProp[3]) )
                     __objSendMsg( oWnd:aColumns[i], '_'+xProp[2], o )
                  ENDIF
                  l := .F.
               ELSEIF Valtype( xProp[4] ) == "C" .AND. xProp[4] == "b"
                  o := SetCB( oWnd, xProp[3] )
                  __objSendMsg( oWnd:aColumns[i], '_'+xProp[2], o )
                  l := .F.
               ENDIF
            ENDIF
            IF l
               __objSendMsg( oWnd:aColumns[i], '_'+xProp[2], xProp[3] )
            ENDIF
         ENDIF

      ELSEIF cPropName == "brwcoldel"
         lErr := !( __ObjHasMsg( oWnd, "INITBRW" )) .OR. Valtype(xProp) != "N" ;
            .OR. xProp <= 0 .OR. xProp > Len( oWnd:aColumns )
         IF !lErr
            oWnd:DelColumn( xProp )
            oWnd:Refresh()
         ENDIF

      ELSEIF cPropName == "brwarr"
         lErr := !( __ObjHasMsg( oWnd, "INITBRW" )) .OR. Valtype(xProp) != "A" ;
               .OR. Valtype(xProp[1]) != "A"
         IF !lErr
            IF !Empty( oWnd:aColumns ) .AND. Len(oWnd:aColumns) == Len(xProp[1])
               oWnd:aArray := xProp
            ELSE
               oWnd:aColumns := {}
               hwg_CREATEARLIST( oWnd, xProp )
               FOR j := 1 TO Len(xProp[1])
                  IF Valtype(xProp[1,j]) == "C" .AND. (xProp[1,j] == "t" .OR. xProp[1,j] == "f")
                     l := j
                     FOR i := 1 TO Len(xProp)
                        IF !(xProp[i,j] == "t") .AND. !(xProp[i,j] == "f")
                           l := 0
                           EXIT
                        ENDIF
                     NEXT
                     IF l > 0
#ifdef __GTK__
                        o := HBitmap():AddStandard( "gtk-apply" )
#else
                        o := HBitmap():AddStandard( OBM_CHECK )
#endif
                        oWnd:aColumns[l]:aBitmaps := { { {|c|c=='t'}, o } }
                        oWnd:bKeyDown := SetCB1( l )
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
            oWnd:Refresh()
         ENDIF
      ENDIF
      EXIT

   CASE 'x'
      IF cPropName == "xparam"
         lErr := !(Valtype(xProp) == "A") .OR. !(Valtype(xProp[1]) == "C") .OR. ;
               !( __ObjHasMsg( oWnd, Upper(xProp[1]) ))
         IF !lErr
            l := .T.
            IF Len( xProp ) == 3
               IF (Valtype( xProp[3] ) == "L" .AND. xProp[3]) .OR. ;
                  (Valtype( xProp[3] ) == "C" .AND. xProp[3] == "o")
                  IF !Empty( o := GetStyle(xProp[2]) ) .OR. !Empty( o := GetFont(xProp[2]) ) ;
                        .OR. !Empty( o := Widg(xProp[2]) ) .OR. !Empty( o := GetHighl(xProp[2]) )
                     __objSendMsg( oWnd, '_'+xProp[1], o )
                  ENDIF
                  l := .F.
               ELSEIF Valtype( xProp[3] ) == "C" .AND. xProp[3] == "b"
                  o := SetCB( oWnd, xProp[2] )
                  __objSendMsg( oWnd, '_'+xProp[1], o )
                  l := .F.
               ENDIF
            ENDIF
            IF l
               __objSendMsg( oWnd, '_'+xProp[1], xProp[2] )
               IF __ObjHasMsg( oWnd, "INITBRW") .AND. Lower(xProp[1]) == "lchanged"
                  oWnd:Refresh()
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE 'e'
      IF cPropName == "enable"
         lErr := !(Valtype(xProp) == "L") .OR. !__ObjHasMsg( oWnd, "ENABLE" )
         IF !lErr
            IF xProp
               oWnd:Enable()
            ELSE
               oWnd:Disable()
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE 'f'
      IF cPropName == "font"
         lErr := !(Valtype(xProp) == "C" .AND. ( xProp := GetFont( xProp ) ) != Nil)
         IF !lErr
            oWnd:oFont := xProp
            IF lWidg
               IF __ObjHasMsg( oWnd, "SETHILI" )
                  oWnd:SetFont( xProp )
               ELSE
#ifdef __GTK__
                  hwg_SetCtrlFont( oWnd:handle,, xProp:handle )
#else
                  hwg_Setctrlfont( oWnd:oParent:handle, oWnd:id, xProp:handle )
#endif
                  oWnd:Refresh()
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE 'i'
      IF cPropName == "image"
         lErr := !__ObjHasMsg( oWnd, "REPLACEBITMAP" )
         IF !lErr
            oWnd:ReplaceBitmap( xProp )
            oWnd:Refresh()
         ENDIF
      ENDIF
      EXIT

   CASE 'm'
      IF cPropName == "move"
         lErr := Valtype(xProp) != "A" .OR. Len(xProp) != 4
         IF !lErr
            FOR i := 1 TO 4
               IF xProp[i] < 0
                  xProp[i] := Nil
               ENDIF
            NEXT
            oWnd:Move( xProp[1], xProp[2], xProp[3], xProp[4] )
         ENDIF
      ENDIF
      EXIT

   CASE 'n'
      IF cPropName == "node"
         lErr := !(__ObjHasMsg( oWnd, "ADDNODE" ) .AND. Valtype(xProp) == "A")
         IF !lErr
            o := Iif( Empty(xProp[1]), oWnd, GetNode( oWnd,xProp[1] ) )
            IF !Empty( o )
               o := o:AddNode( xProp[3],, Iif( Empty(xProp[4]), Nil, GetNode(o,xProp[4]) ),, ;
                  Iif( Empty(xProp[5]), Nil, xProp[5] ) )
               o:objName := xProp[2]
               IF !Empty( xProp[6] )
                  SetCallback( o, "onclick", xProp[6] )
               ENDIF
            ENDIF
         ENDIF
      ELSEIF cPropName == "nodesele"
         lErr := !(__ObjHasMsg( oWnd, "ADDNODE" ) .AND. Valtype(xProp) == "C")
         IF !lErr
            o := Iif( Empty(xProp), oWnd, GetNode( oWnd,xProp ) )
            IF !Empty( o )
               oWnd:Select( o )
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE 'p'
      IF cPropName == "pagestart"
         lErr := !( __ObjHasMsg( oWnd, "STARTPAGE" ))
         IF !lErr
            oWnd:StartPage( xProp )
         ENDIF

      ELSEIF cPropName == "pageend"
         lErr := !( __ObjHasMsg( oWnd, "ENDPAGE" ))
         IF !lErr
            oWnd:EndPage()
         ENDIF
      ENDIF
      EXIT

   CASE 'r'
      IF cPropName == "radioend"
         lErr := Valtype(oWnd:cargo) != "O" .OR. !( __ObjHasMsg( oWnd:cargo, "ENDGROUP" ))
         IF !lErr
            oWnd:cargo:EndGroup( xProp )
         ENDIF
      ENDIF
      EXIT

   CASE 's'
      IF cPropName == "step"
         lErr := !( __ObjHasMsg( oWnd, "STEP" ))
         IF !lErr
            oWnd:Step()
         ENDIF

      ELSEIF cPropName == "setval"
         lErr := !( __ObjHasMsg( oWnd, "STEP" ))
         IF !lErr
            oWnd:Set( ,xProp )
         ENDIF
      ENDIF
      EXIT

   CASE 'h'
      IF cPropName == "hiliopt"
         lErr := !( __ObjHasMsg( oWnd, "SETHILI" ))
         IF !lErr
            oWnd:SetHili( xProp[1], Iif(Empty(xProp[2]), Nil, GetFont(xProp[2]) ), ;
               xProp[3], xProp[4] )
         ENDIF
      ELSEIF cPropName == "hili"
         lErr := !( __ObjHasMsg( oWnd, "SETHILI" ))
         IF !lErr
            IF Empty(xProp)
               oWnd:HighLighter()
            ELSE
               oWnd:HighLighter( GetHighl(xProp) )
            ENDIF
         ENDIF
      ELSEIF cPropName == "hide"
         lErr := !(Valtype(xProp) == "L") .OR. !__ObjHasMsg( oWnd, "HIDE" )
         IF !lErr
            IF xProp
               oWnd:Hide()
            ELSE
               oWnd:Show()
            ENDIF
         ENDIF
      ENDIF
      EXIT

   OTHERWISE
      lErr := .T.
   END

   IF lErr
      gWritelog( "Parameter error" )
   ENDIF

   RETURN .T.

STATIC FUNCTION SetCB1( nCol )
   LOCAL block := {|o,key|
   IF key == 32 .AND. !Empty( o:aColumns[o:ColPos]:aBitmaps )
      o:aArray[o:nCurrent,o:ColPos] := Iif(o:aArray[o:nCurrent,o:ColPos]=='t','f','t')
      o:RefreshLine()
   ENDIF
   RETURN .T.
   }
   RETURN block

STATIC FUNCTION SetCB( oWidg, cCode )
   LOCAL aScript
   LOCAL block := {||
      Return DoScript( aScript, {oWidg} )
   }

   IF Left(cCode,1) == '{'
      block := &( cCode )
   ELSEIF cn $ cCode
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
      gWritelog( "Wrong event name" )
      RETURN .F.
   ENDIF

   block := SetCB( oWidg, cCode )

   IF cbName == "oninit"
      IF __ObjHasMsg( oWidg, "MENU" )
         IF Empty(oWidg:cargo)
            oWidg:cargo := Array(2)
         ENDIF
         oWidg:cargo[1] := block
      ELSE
         oWidg:bInit := block
      ENDIF
   ELSEIF cbName == "ondestroy"
      IF __ObjHasMsg( oWidg, "MENU" )
         IF Empty(oWidg:cargo)
            oWidg:cargo := Array(2)
         ENDIF
         oWidg:cargo[2] := block
      ELSE
         oWidg:bDestroy := block
      ENDIF
   ELSEIF cbName == "onclick"
      oWidg:bClick := block
   ELSEIF cbName == "onsize"
      oWidg:bSize := block
   ELSEIF cbName == "onpaint"
      oWidg:bPaint := block
   ELSEIF cbName == "onrclick"
      oWidg:bRClick := block
   ELSEIF cbName == "ondblclick"
      oWidg:bdblClick := block
   ELSEIF cbName == "onenter"
      oWidg:bEnter := block
   ELSEIF cbName == "onposchanged"
      oWidg:bPosChanged := block
   ELSE
      gWritelog( "Wrong event name" )
   ENDIF

   RETURN .T.

#ifndef __GTK__
STATIC FUNCTION SetTray( cmd, cIconName, cMenuName, cTooltip )

   LOCAL oMenu, oIcon

   IF cmd == "init"
      IF !Empty( oMenu := GetContextMenu( Upper(cMenuName) ) )
         oIcon := Iif( Empty(cIconName), oMainWnd:oIcon, HIcon():AddFile( cIconName ) )
         oMainWnd:InitTray( oIcon, oMenu:aMenu[1,1,1], oMenu, cTooltip )
      ENDIF

   ELSEIF cmd == "icon"
      IF ( oIcon := HIcon():AddFile( cIconName ) ) != Nil
         hwg_ShellModifyicon( oMainWnd:handle, oIcon:handle )
      ENDIF
   ENDIF
   RETURN Nil
#endif

STATIC FUNCTION SetParam( cName, xValue )

   IF cName == "bmppath"
      HBitmap():cPath := HIcon():cPath := xValue

   ELSEIF cName == "path"
      SET DEFAULT TO &xValue
      SET PATH TO &xValue
      cDefPath := xValue

   ELSEIF cName == "datef"
      SET DATE FORMAT xValue

   ELSE
      gWritelog( "Wrong parameter name" )
   ENDIF

   RETURN Nil

STATIC FUNCTION GetProperty( cWidgName, cPropName )

   LOCAL oWnd, lWidg := .T., cRes

   //A window or a dialog ?
   lWidg := ("." $ cWidgName)

   oWnd := Widg( cWidgName )
   IF Empty( oWnd )
      RETURN .F.
   ENDIF

   IF cPropName == "text"
      IF lWidg
         IF __ObjHasMsg( oWnd, "CPICFUNC" ) .AND. ;
               ( !Empty(oWnd:cPicFunc) .OR. !Empty(oWnd:cPicMask) )
            cRes := CnvVal( oWnd:Value() )
         ELSEIF __ObjHasMsg( oWnd, "MARKLINE" )
            cRes := oWnd:GetText( ,, .T. )
         ELSEIF __ObjHasMsg( oWnd, "GETTEXT" )
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

   ELSEIF cPropName == "brwarr"
      IF __ObjHasMsg( oWnd, "AARRAY" )
         cRes := oWnd:aArray
      ENDIF
   ENDIF

   RETURN cRes


STATIC FUNCTION f_MsgInfo( cMess, cTitle, cFunc, cName )

   hwg_MsgInfo( cMess, cTitle )
   IF !Empty( cFunc )
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName} ) } ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_MsgStop( cMess, cTitle, cFunc, cName )

   hwg_MsgStop( cMess, cTitle )
   IF !Empty( cFunc )
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName} ) } ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_MsgYesNo( cMess, cTitle, cFunc, cName )

   LOCAL lYes := hwg_MsgYesNo( cMess, cTitle )
   IF !Empty( cFunc )
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,Iif(lYes,"t","f")} ) } ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_MsgGet( cMess, cTitle, nStyle, cFunc, cName )

   LOCAL cRes := hwg_MsgGet( cTitle, cMess )
   IF !Empty( cFunc )
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,cRes} ) } ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_Choice( arr, cTitle, cFunc, cName )

   LOCAL nRes := hwg_WChoice( arr, cTitle )
   IF !Empty( cFunc )
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,Ltrim(Str(nRes))} ) } ) )
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
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,fname} ) } ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_SeleFolder( cFunc, cName )

   LOCAL fname
   fname := hwg_SelectFolder()

   IF !Empty( cFunc )
      IF Empty( fname )
         fname := ""
      ENDIF
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,fname} ) } ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION f_SeleFont( cFunc, cName )

   LOCAL oFont := HFont():Select()
   IF !Empty( cFunc )
      IF Empty( oFont )
         SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName} ) } ) )
      ELSE
         oFont:objname := Upper( cName )
         SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( ;
            {cName, oFont:name, Ltrim(Str(oFont:height)), Iif(oFont:weight>400,"t","f"), ;
            Iif(oFont:italic>0,"t","f"), Iif(oFont:underline>0,"t","f"), ;
            Iif(oFont:strikeout>0,"t","f"),Ltrim(Str(oFont:charset)) } ) } ) )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION f_SeleColor( nColor, cFunc, cName )

   nColor := Hwg_ChooseColor( nColor,.F. )
   IF !Empty( cFunc )
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cName,Iif(nColor==Nil,"",Ltrim(Str(nColor)))} ) } ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION PrintInit( cName, aParams, cFunc, cMet )

   LOCAL nFormType := Iif( Empty(aParams[3]), Nil, aParams[3] )
   LOCAL oPrinter

   IF aParams[1] == "..."
      aParams[1] := Nil
   ELSE
      cFunc := cMet := Nil
   ENDIF

   IF Empty( oPrinter := HPrinter():New( aParams[1], .T., nFormType,, !Empty(aParams[4]) ) )
      RETURN Nil
   ENDIF
   oPrinter:objname := cName
   Aadd( aPrinters, oPrinter )

   IF !Empty( cFunc )
      SendOut( hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( {cMet} ) } ) )
   ENDIF

   oPrinter:StartDoc( aParams[2] )

   RETURN Nil

STATIC FUNCTION PrintFuncs( cFunc, cName, aParams )

   LOCAL oPrinter, i, oFont

   FOR i := 1 TO Len(aPrinters)
      IF aPrinters[i]:objname == cName
         oPrinter := aPrinters[i]
         EXIT
      ENDIF
   NEXT
   IF Empty( oPrinter )
      gWritelog( "Printer not found: " + cName )
      RETURN Nil
   ENDIF
   IF cFunc == "text"
      oPrinter:Say( aParams[1], aParams[2], aParams[3], aParams[4], aParams[5], aParams[6] )

   ELSEIF cFunc == "box"
      oPrinter:Box( aParams[1], aParams[2], aParams[3], aParams[4] )

   ELSEIF cFunc == "line"
      oPrinter:Line( aParams[1], aParams[2], aParams[3], aParams[4] )

   ELSEIF cFunc == "bitmap"

   ELSEIF cFunc == "fontadd"
      oFont := oPrinter:AddFont( aParams[2], aParams[3], aParams[4], ;
         aParams[5], aParams[6], aParams[7] )
      oFont:objname := Upper(aParams[1])

   ELSEIF cFunc == "fontset"
      IF ( oFont := GetPrinterFont( aParams[1] ) ) != Nil
         oPrinter:SetFont( oFont )
      ENDIF

   ELSEIF cFunc == "startpage"
      oPrinter:StartPage()

   ELSEIF cFunc == "endpage"
      oPrinter:EndPage()

/*
   ELSEIF cFunc == "startdoc"
      oPrinter:StartDoc( aParams[1] )

   ELSEIF cFunc == "enddoc"
      oPrinter:EndDoc()

   ELSEIF cFunc == "preview"
      oPrinter:Preview()
*/
   ELSEIF cFunc == "end"
      oPrinter:EndDoc()
      IF oPrinter:lPreview()
         oPrinter:Preview()
      ENDIF
      oPrinter:End()

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

   RETURN GetItemByName( HStyle():aStyles, Upper(cName) )

STATIC FUNCTION GetFont( cName )

   RETURN GetItemByName( HFont():aFonts, Upper(cName) )

STATIC FUNCTION GetHighl( cName )

   RETURN GetItemByName( aHighls, Upper(cName) )

STATIC FUNCTION GetContextMenu( cName )

   RETURN GetItemByName( aContextMenus, Upper(cName) )

STATIC FUNCTION GetPrinterFont( cName )

#ifdef __GTK__
   RETURN GetItemByName( HGP_Font():aFonts, Upper(cName) )
#else
   RETURN GetItemByName( HFont():aFonts, Upper(cName) )
#endif

FUNCTION Wnd( cName )

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

FUNCTION Widg( cWidgName, oWnd )

   LOCAL nPos

   IF oWnd == Nil
      IF (nPos := At( ".", cWidgName )) == 0
         RETURN Wnd( cWidgName )
      ELSE
         oWnd := Wnd( Left( cWidgName, nPos-1 ) )
         cWidgName := Upper(Substr( cWidgName, nPos+1 ))
      ENDIF
   ELSE
      cWidgName := Upper( cWidgName )
      IF (nPos := Rat( ".", cWidgName )) != 0
         cWidgName := Substr( cWidgName, nPos+1 )
      ENDIF
   ENDIF
   IF !Empty( oWnd )
      DO WHILE (nPos := At( ".", cWidgName )) != 0
         IF ( oWnd := GetItemByName( oWnd:aControls, Left( cWidgName, nPos-1 ) ) ) == Nil
            RETURN Nil
         ENDIF
         cWidgName := Substr( cWidgName, nPos+1 )
      ENDDO
      RETURN GetItemByName( oWnd:aControls, cWidgName )
   ENDIF

   RETURN Nil

FUNCTION GetNode( o, cNodeName )

   LOCAL aItems := o:aItems, i, nlen := Len( aItems ), oNode

   FOR i := 1 TO nlen
      IF aItems[i]:objname == cNodeName
         RETURN aItems[i]
      ELSEIF ! Empty( aItems[i]:aItems )
         IF ( oNode := GetNode( aItems[i],cNodeName ) ) != Nil
            RETURN oNode
         ENDIF
      ENDIF
   NEXT

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
         SendOut( '["exit","main"]' )
      ENDIF

      RETURN lRes
   }

   FOR i := 1 TO Len( oForm:aMethods )
      IF oForm:aMethods[ i,1 ] == "ondlginit"
         IF ( cType := ValType( oForm:aMethods[ i,2 ] ) ) == "B"
            blockI := oForm:aMethods[ i,2 ]
            oForm:aMethods[ i,2 ] := bInit
         ELSEIF cType == "A"
            blockI := oForm:aMethods[ i,2,1 ]
            oForm:aMethods[ i,2,1 ] := bInit
         ENDIF
      ELSEIF oForm:aMethods[ i,1 ] == "ondlgexit"
         IF ( cType := ValType( oForm:aMethods[ i,2 ] ) ) == "B"
            blockE := oForm:aMethods[ i,2 ]
            oForm:aMethods[ i,2 ] := bExit
         ELSEIF cType == "A"
            blockE := oForm:aMethods[ i,2,1 ]
            oForm:aMethods[ i,2,1 ] := bExit
         ENDIF
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

   LOCAL arr, cCommand, i

   IF nConnType == 1
      gs_ListenSocket()
      gs_CheckSocket()
      IF gs_CheckSockError()
         Panic()
      ENDIF
   ELSEIF nConnType == 2
      conn_CheckIn()
   ENDIF

   DO WHILE nDeferred > 0
      arr := aDeferred[1]
      DelDeferred( 1 )
      IF ( cCommand := arr[1] ) == "close"
         WinClose( arr[2] )

      ELSEIF cCommand == "common"
         IF arr[2] == "minfo"
            f_MsgInfo( Iif(Len(arr)>4,arr[5],""), Iif(Len(arr)>5,arr[6],""), arr[3], arr[4] )
         ELSEIF arr[2] == "mstop"
            f_MsgStop( Iif(Len(arr)>4,arr[5],""), Iif(Len(arr)>5,arr[6],""), arr[3], arr[4] )
         ELSEIF arr[2] == "myesno"
            f_MsgYesNo( Iif(Len(arr)>4,arr[5],""), Iif(Len(arr)>5,arr[6],""), arr[3], arr[4] )
         ELSEIF arr[2] == "mget"
            f_MsgGet( Iif(Len(arr)>4,arr[5],""), Iif(Len(arr)>5,arr[6],""), Iif(Len(arr)>6,arr[7],0), arr[3], arr[4] )
         ELSEIF arr[2] == "mchoi"
            f_Choice( Iif(Len(arr)>4,arr[5],{}), Iif(Len(arr)>5,arr[6],""), arr[3], arr[4] )
         ELSEIF arr[2] == "cfont"
            f_selefont( arr[3], arr[4] )
         ELSEIF arr[2] == "cfile"
            f_selefile( Iif(Len(arr)>4,arr[5],""), arr[3], arr[4] )
         ELSEIF arr[2] == "cfold"
            f_selefolder( arr[3], arr[4] )
         ELSEIF arr[2] == "ccolor"
            f_selecolor( Iif(Len(arr)>4,arr[5],""), arr[3], arr[4] )
         ENDIF

      ELSEIF cCommand == "packet"
         FOR i := 2 TO Len( arr )
            IF Valtype(arr[i]) != "A" .OR. !Parse( arr[i], .T. )
               RETURN .F.
            ENDIF
         NEXT

      ELSEIF cCommand == "prninit"
         PrintInit( Upper(arr[2]), arr[3], Iif(Len(arr)>3,arr[4],Nil), ;
               Iif(Len(arr)>4,arr[5],Nil) )

      ELSEIF cCommand == "actmainwnd"
         ActMainWnd( arr[2] )

      ELSEIF cCommand == "actdialog"
         ActDialog( arr[2], arr[3] )

      ENDIF
   ENDDO

   RETURN Nil

STATIC FUNCTION Add2Deferred( arr )

   IF ++ nDeferred > Len( aDeferred )
      aDeferred := ASize( aDeferred, Len(aDeferred)+5 )
   ENDIF
   aDeferred[nDeferred] := arr
   RETURN Nil

STATIC FUNCTION DelDeferred( n )

   ADel( aDeferred, n )
   nDeferred --
   RETURN Nil

STATIC FUNCTION Parse( arr, lPacket )

   LOCAL cCommand := Lower( arr[1] ), c := Left( cCommand, 1 )
   LOCAL oForm, oWnd, o, lErr := .F., cRes

   SWITCH c
   CASE 's'
      IF cCommand == "set"
         lErr := ( Len(arr)<4 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            SetProperty( Lower(arr[2]), Lower(arr[3]),  arr[4] )
         ENDIF
      ELSEIF cCommand == "setparam"
         lErr := ( Len(arr)<3 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            SetParam( Lower(arr[2]), arr[3] )
         ENDIF
      ELSEIF cCommand == "setvar"
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
      IF cCommand == "get"
         lErr := ( Len(arr)<3 )
         IF !lErr
            cRes := GetProperty( arr[2], Lower(arr[3]) )
            SendIn( "+" + hb_jsonEncode(cRes) + cn )
         ENDIF

      ELSEIF cCommand == "getvalues"
         lErr := ( Len(arr)<3 )
         IF !lErr
            arr := GetValues( arr[2], arr[3] )
            SendIn( "+" + hb_jsonEncode(arr) + cn )
         ENDIF
      ELSEIF cCommand == "getver"
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

   CASE 'a'
      IF cCommand == "addwidg"

         lErr := ( Len(arr)<4 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            AddWidget( Lower(arr[2]), Lower(arr[3]),  arr[4], Iif( Len(arr)>4,arr[5],Nil ) )
         ENDIF
      ELSEIF cCommand == "actmainwnd"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            Add2Deferred( arr )
         ENDIF
         lEnd := .T.

      ELSEIF cCommand == "actdialog"

         lErr := ( Len(arr)<3 .OR. Valtype(arr[2]) != "C" .OR. Valtype(arr[3]) != "C" )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            Add2Deferred( arr )
            //ActDialog( arr[2], arr[3] )
         ENDIF
         lEnd := .T.
      ENDIF
      EXIT

   CASE 'e'
      IF cCommand == "evalcode"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF Len( arr ) > 2 .AND. Lower( arr[3] ) == "t"
               cRes := DoScript( RdScript( ,arr[2] ) )
               SendIn( "+" + hb_jsonEncode(cRes) + cn )
            ELSE
               IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
               DoScript( RdScript( ,arr[2] ) )
            ENDIF
         ENDIF
      ELSEIF cCommand == "exit"
         lEnd := .T.
         IF ( oWnd := HWindow():GetMain() ) != Nil
            oWnd:Close()
         ENDIF
         IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
      ENDIF
      EXIT

   CASE 'o'
      IF cCommand == "openform"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            oForm := HFormTmpl():Read( arr[2] )
            oForm:Show()
         ENDIF
         lEnd := .T.

      ELSEIF cCommand == "openformmain"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            oForm := HFormTmpl():Read( arr[2] )
            SetFormTimer( oForm )
            oForm:ShowMain()
         ENDIF
         lEnd := .T.

      ELSEIF cCommand == "openreport"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            oForm := HRepTmpl():Read( arr[2] )
            oForm:Print()
         ENDIF
      ENDIF
      EXIT

   CASE 'm'
      IF cCommand == "menu"
         lErr := ( Len(arr)<2 .OR. (Valtype(arr[2]) == "C" .AND. Len(arr)<6) )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            IF Valtype(arr[2]) == "C"
               SetMenuItem( Lower(arr[2]), Upper(arr[3]), Upper(arr[4]), arr[5], arr[6] )
            ELSE
               SetMenu( arr[2] )
            ENDIF
         ENDIF
      ELSEIF cCommand == "menucontext"
         lErr := ( Len(arr)<4 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            MenuContext( Lower(arr[2]), Upper(arr[3]), arr[4] )
         ENDIF
      ENDIF
      EXIT

   CASE 'c'
      IF cCommand == "common"
         lErr := ( Len(arr)<4 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            Add2Deferred( arr )
         ENDIF

      ELSEIF cCommand == "crmainwnd"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            CrMainWnd( arr[2], Iif( Len(arr)>2,arr[3],Nil ) )
         ENDIF
      ELSEIF cCommand == "crdialog"
         lErr := ( Len(arr)<3 .OR. Valtype(arr[2]) != "C" .OR. Valtype(arr[3]) != "A" )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            CrDialog( arr[2], arr[3], Iif( Len(arr)>3,arr[4],Nil ) )
         ENDIF
      ELSEIF cCommand == "close"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            //WinClose( arr[2] )
            Add2Deferred( arr )
         ENDIF

      ELSEIF cCommand == "crfont"
         lErr := ( Len(arr)<9 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            CrFont( Upper(arr[2]), arr[3], arr[4], arr[5], arr[6], arr[7], arr[8], arr[9] )
         ENDIF

      ELSEIF cCommand == "crstyle"
         lErr := ( Len(arr)<8 )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            CrStyle( Upper(arr[2]), arr[3], arr[4], arr[5], arr[6], arr[7], arr[8] )
         ENDIF
      ENDIF
      EXIT

   CASE 'p'
      IF cCommand == "print"
         lErr := ( Len(arr)<4 .OR. Valtype(arr[2]) != "C" .OR. Valtype(arr[3]) != "C" )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            PrintFuncs( Lower(arr[2]), Upper(arr[3]), arr[4] )
         ENDIF

      ELSEIF cCommand == "prninit"
         lErr := ( Len(arr)<3 .OR. Valtype(arr[2]) != "C" )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            Add2Deferred( arr )
         ENDIF

      ELSEIF cCommand == "packet"
         lErr := lPacket
         IF !lErr
            SendIn( "+Ok" + cn )
            Add2Deferred( arr )
         ENDIF
      ENDIF
      EXIT

   CASE 'h'
      IF cCommand == "highl"
         lErr := ( Len(arr)<7 .OR. Valtype(arr[2]) != "C" .OR. Valtype(arr[3]) != "C" ;
            .OR. Valtype(arr[4]) != "C" .OR. Valtype(arr[5]) != "C" .OR. Valtype(arr[6]) != "C" .OR. Valtype(arr[7]) != "L" )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            o := Hilight():New( ,, arr[3], arr[4], arr[5], arr[6], arr[7] )
            o:objName := Upper(arr[2])
            Aadd( aHighLs, o )
         ENDIF

      ENDIF
      EXIT

   CASE 't'
      IF cCommand == "tray"
#ifdef __GTK__
         SendIn( "+Ok" + cn )
#else
         lErr := ( Len(arr)<3 .OR. Valtype(arr[2]) != "C" .OR. Valtype(arr[3]) != "C" )
         IF !lErr
            IF !lPacket; SendIn( "+Ok" + cn ); ENDIF
            SetTray( Lower(arr[2]), arr[3], Iif(Len(arr)>3,arr[4],Nil), Iif(Len(arr)>4,arr[5],Nil) )
         ENDIF
#endif
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
      cBuffer := gs_GetRecvBuffer()
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
      cRes := gs_Send2SocketOut( "+" + s + cn )
      IF gs_CheckSockError()
         Panic()
      ENDIF
   ELSEIF nConnType == 2
      cRes := conn_Send2SocketOut( "+" + s + cn )
   ENDIF

   RETURN Iif( Empty(cRes), "", cRes )

STATIC FUNCTION SendIn( s )

   IF nConnType == 1
      gs_Send2SocketIn( s )
   ELSEIF nConnType == 2
      conn_Send2SocketIn( s )
   ENDIF

   RETURN Nil

STATIC FUNCTION gWritelog( s )

   IF nLogOn > 0
      hwg_writelog( s, cLogFile )
   ENDIF
   RETURN Nil

FUNCTION gVersion( n )
   RETURN Iif( n==0, GUIS_VERSION, Iif( n==1, "GuiServer " + GUIS_VERSION, ;
      "GuiServer " + GUIS_VERSION + Chr(13)+Chr(10) + hb_Version() + Chr(13)+Chr(10) + hwg_Version() ) )
