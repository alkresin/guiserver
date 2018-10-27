/*
 * extGUI - GUI framework for Harbour
 * This test demonstrates base windows and widgets creation
 *
 * Copyright 2018 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "extgui.ch"

#define  CLR_LBLUE   16759929  // #79BCFF
#define  CLR_LIGHT1  15132390
#define  CLR_LIGHT2  12632256  // #C0C0C0
#define  CLR_LIGHTG  12507070

#define  CLR_LBLUE0  12164479  // #7F9DB9
#define  CLR_LBLUE1  16773866  // #EAF2FF
#define  CLR_LBLUE2  16770002  // #D2E3FF
#define  CLR_LBLUE3  16772062  // #DEEBFF
#define  CLR_LBLUE4  16775920  // #F0FAFF

Static oDlg, oLabel, oEdi1, oStyle1

Memvar oLastWidget

FUNCTION Main

   LOCAL oMainWindow, oPane, oStyle2, oStyle3, oStyle4
   LOCAL cInitString := Memoread( "test.ini" ), nInitRes

   IF ( nInitRes := eGUI_Init( cInitString ) ) != 0
      ? Iif( nInitRes==1, "No connection...", "Protocol differs" )
      RETURN Nil
   ENDIF

   oStyle1 := eGUI_CreateStyle( , {CLR_LBLUE,CLR_LBLUE3}, 1 )
   oStyle2 := eGUI_CreateStyle( , {CLR_LBLUE}, 1,, 3 )
   oStyle3 := eGUI_CreateStyle( , {CLR_LBLUE}, 1,, 2, CLR_LBLUE0 )
   oStyle4 := eGUI_CreateStyle( , {CLR_LBLUE2,CLR_LBLUE3}, 1,, 1, CLR_LBLUE )

   oMainWindow := eGUI_InitMainWindow( 100, 100, 400, 350, "extGUI test", ;
      {{"bcolor",CLR_LBLUE3}} )

   egui_Menu()
      egui_Menu( "File" )
         eGUI_AddMenuItem( "New", "newfile", "New file item" )
         eGUI_AddMenuItem( "Open", "openfile" )
         eGUI_AddMenuItem( "Browse", "fbrowse" )
         eGUI_AddMenuSeparator()
         eGUI_AddMenuItem( "msgbox", "mbox" )
         eGUI_AddMenuItem( "Select color", "SetLblColor" )
         eGUI_AddMenuItem( "Select font", "SetLblFont" )
         eGUI_AddMenuItem( "Select file", "SeleFile" )
         eGUI_AddMenuSeparator()
         eGUI_AddMenuItem( "Printing", "PrnTest" )
         eGUI_AddMenuSeparator()
         eGUI_AddMenuItem( "Exit",,"hwg_EndWindow()" )
      egui_EndMenu()
      egui_Menu( "Help" )
         eGUI_AddMenuItem( "About",, 'hwg_MsgInfo(hb_version()+chr(10)+chr(13)+hwg_version(),"About")' )
      egui_EndMenu()
   egui_EndMenu()

   oPane := oMainWindow:AddWidget( "paneltop",, 0,0, 400, 36,, {{"HStyle",oStyle1}})

   oPane:AddWidget( "ownbtn",, 0,0, 64, 36, "Date", {{"HStyles",{oStyle1,oStyle2,oStyle3}}} )
   oLastWidget:SetCallbackProc( "onclick",, "hwg_WriteStatus(HWindow():GetMain(),1,Dtoc(Date()),.T.)" )
   oPane:AddWidget( "ownbtn",, 64,0, 64, 36, "Time", {{"HStyles",{oStyle1,oStyle2,oStyle3}}} )
   oLastWidget:SetCallbackProc( "onclick",, "hwg_WriteStatus(HWindow():GetMain(),2,Time(),.T.)" )
   oPane:AddWidget( "ownbtn",, 128,0, 64, 36, "Get", {{"HStyles",{oStyle1,oStyle2,oStyle3}}} )
   oLastWidget:SetCallbackProc( "onclick","openfile" )

   oLabel := oMainWindow:AddWidget( "label","l1", 20,50, 180, 24, "This is a label", {{"tcolor",255},{"Transpa",.T.}} )

   oMainWindow:AddWidget( "button",, 200,46, 100, 32, "Click!" )
   oLastWidget:SetCallbackProc( "onclick", "fmenu1" )

   oMainWindow:AddWidget( "group",, 20, 110, 300, 100, "Group" )
   oMainWindow:AddWidget( "check",, 40, 140, 120, 24, "Check - 1" )
   oMainWindow:AddWidget( "check",, 40, 158, 120, 24, "Check - 2" )

   oMainWindow:AddWidget( "panelbot",, 0,0, 400, 32,, {{"HStyle",oStyle4},{"AParts",{120,120,0}}})

   eGUI_ActivateMainWindow()

   eGUI_Exit()

   RETURN Nil

FUNCTION fmenu1()

   oLabel:SetText( "Just a test" )
   RETURN Nil

FUNCTION newfile( arr )

   oLabel:SetText( arr[1] )
   RETURN Nil

FUNCTION openfile()

   LOCAL oFont, o, arr1 := { "First", "Second", "Third"}

   eGUI_BeginPacket()
   eGUI_SetDateFormat( "DD.MM.YYYY" )
   oFont := eGUI_CreateFont( "f1", "Georgia", 22 )
   oDlg := eGUI_InitDialog( "dlg1", 300, 200, 220, 400, "GetValues", {{"Font",oFont}} )

   oDlg:AddWidget( "label",, 20, 20, 180, 24, "Идентификатор:" )
   oEdi1 := oDlg:AddWidget( "edit","edi1", 20,44, 180, 26,, {{"Picture","@!R /XXX:XXX/"}})

   oDlg:AddWidget( "label",, 20, 72, 180, 24, "Параметр:" )
   oDlg:AddWidget( "edit","edi2", 20, 96, 120, 26 )
   oDlg:AddWidget( "button",, 150, 96, 50, 26, "==" )
   oLastWidget:SetCallbackProc( "onclick","fthird" )

   oDlg:AddWidget( "label",, 20, 122, 180, 24, "Дата изменения:" )
   oDlg:AddWidget( "edit","edi2", 20, 144, 120, 26, Dtos(Date()), {{"Picture","D@D"}})

   o := oDlg:AddWidget( "radiogr","rg", 20, 190, 180, 80, "Group" )
   oDlg:AddWidget( "radio",, 50, 220, 120, 24, "radio1" )
   oDlg:AddWidget( "radio",, 50, 246, 120, 24, "radio2" )
   egui_RadioEnd( o,1 )

   oDlg:AddWidget( "combo",, 20, 280, 180, 26,, {{"AItems",arr1}})

   oDlg:AddWidget( "button",, 50, 350, 100, 32, "Ok",{{"Anchor",ANCHOR_HORFIX+ANCHOR_BOTTOMABS}} )
   oLastWidget:SetCallbackProc( "onclick","fclose" )

   eGUI_ActivateDialog()
   eGUI_EndPacket()

   RETURN Nil

FUNCTION fclose()

   LOCAL arr := egui_GetValues( oDlg, {"edi1","edi2","rg"} )

   oDlg:Close()
   egui_MsgInfo( "Ид: "+arr[1]+Chr(13)+Chr(10)+"Пар: "+arr[2]+Chr(13)+Chr(10)+"Радио: "+arr[3]+Chr(13)+Chr(10), "Answer" )

   RETURN Nil

FUNCTION fthird()

   LOCAL oDlg3

   oDlg3 := eGUI_InitDialog( "dlg3", 100, 100, 220, 180, "3" )

   oDlg3:AddWidget( "label",, 20,20, 180, 24, "Name:" )
   oDlg3:AddWidget( "edit", "edi", 20,44, 180, 26 )

   oDlg3:AddWidget( "button",, 60, 120, 100, 32, "Set" )
   oLastWidget:SetCallbackProc( "onclick","fclose3" )

   eGUI_ActivateDialog()

   RETURN Nil

FUNCTION fclose3( arr )

   LOCAL oDlg := eGUI_GetWidg(arr[1]):oParent
   LOCAL oEdi := eGUI_GetWidg( "dlg3.edi" )
   LOCAL oEdi1 := eGUI_GetWidg( "dlg1.edi2" )
   LOCAL s := oEdi:GetText()

   IF !Empty( s )
      oEdi1:SetText( s )
   ENDIF

   oDlg:Close()

   RETURN Nil

FUNCTION fbrowse()

   LOCAL oDlg3, oBrw
   LOCAL aSample := { {"Alex",23,1500}, {"Victor",42,2400}, {"John",31,2800} }

   oDlg3 := eGUI_InitDialog( "dlgb", 100, 100, 300, 280, "Browse" )

   oBrw := oDlg3:AddWidget( "browse",, 10,10, 280, 210,, {{"Anchor",ANCHOR_TOPABS+ANCHOR_BOTTOMABS+ANCHOR_LEFTABS+ANCHOR_RIGHTABS}} )
   oBrw:SetParam( "oStyleHead", oStyle1 )
   eGUI_BrwSetArray( oBrw, aSample )
   eGUI_BrwSetColumn( oBrw, 1, "Name", 1 )
   eGUI_BrwSetColumn( oBrw, 2, "Age", 1 )
   eGUI_BrwSetColumn( oBrw, 3, "Salary", 1, 2, .T. )

   oDlg3:AddWidget( "button",, 100, 240, 100, 32, "Close", {{"Anchor",ANCHOR_HORFIX+ANCHOR_BOTTOMABS}} )
   oLastWidget:SetCallbackProc( "onclick",,"hwg_EndDialog()" )

   eGUI_ActivateDialog()

   RETURN Nil


FUNCTION MBox( aParams )

   IF aParams == Nil
      egui_MsgYesNo( "Yes or No?", "Msgbox", "mbox", "mm1" )
   ELSEIF aParams[1] == "mm1"
      IF aParams[2] == "t"
         egui_MsgInfo( "Yes!", "Answer" )
      ELSE
         egui_MsgStop( "No...", "Answer" )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION SetLblColor( aParams )

   IF aParams == Nil
      eGUI_SelectColor( , "SetLblColor", "mm1" )
   ELSEIF aParams[1] == "mm1"
      IF !Empty( aParams[2] )
         oLabel:SetColor( Val(aParams[2]) )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION SetLblFont( aParams )

   LOCAL oFont

   IF aParams == Nil
      eGUI_SelectFont( "SetLblFont" )
   ELSE
      IF ( oFont := eGUI_GetFont( aParams[1] ) ) != Nil
         IF Len( aParams ) < 8
            oFont:Delete()
         ELSE   
            oFont:Fill( aParams )
            oLabel:SetFont( oFont )
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION SeleFile( aParams )

   IF aParams == Nil
      eGUI_SelectFile( , "SeleFile", "mm1" )
   ELSE
      IF Empty( aParams[2] )
         egui_MsgStop( "Nothing selected", "Result" )
      ELSE
         egui_MsgInfo( aParams[2], "File selected" )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION PrnTest( aParams )

   LOCAL oFont, oPrinter

   IF aParams == Nil
      oPrinter := eGUI_InitPrinter( ,, .T.,,, "prntest", "mm1" )
   ELSE
      oPrinter := m->oLastPrinter
      oFont := oPrinter:AddFont( ,"Times New Roman",10 )

      oPrinter:StartPage()
      oPrinter:SetFont( oFont )
      oPrinter:Box( 5,5,200,282 )
      oPrinter:Say( "Windows printing first sample !", 50,10,165,26,DT_CENTER,oFont  )
      oPrinter:Line( 45,30,170,30 )
      oPrinter:Line( 45,5,45,30 )
      oPrinter:Line( 170,5,170,30 )
      oPrinter:Say( "----------", 50,120,150,132,DT_CENTER  )
      oPrinter:Box( 50,134,160,146 )
      oPrinter:Say( "End Of Report", 50,135,160,146,DT_CENTER  )
      oPrinter:EndPage()
      oPrinter:End()
   ENDIF
   RETURN Nil
