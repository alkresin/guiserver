/*
 * extGUI - GUI framework for Harbour
 * This test demonstrates base windows and widgets creation
 *
 * Copyright 2018 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define  CLR_LBLUE   16759929  // #79BCFF
#define  CLR_LIGHT1  15132390
#define  CLR_LIGHT2  12632256  // #C0C0C0
#define  CLR_LIGHTG  12507070

#define  CLR_LBLUE0  12164479  // #7F9DB9
#define  CLR_LBLUE1  16773866  // #EAF2FF
#define  CLR_LBLUE2  16770002  // #D2E3FF
#define  CLR_LBLUE3  16772062  // #DEEBFF
#define  CLR_LBLUE4  16775920  // #F0FAFF

Static oDlg, oLabel, oEdi1

FUNCTION Main

   LOCAL oMainWindow, oBtn, oBtnOk, oPane, oStyle, own1
   LOCAL cInitString := ""

   IF !eGUI_Init( cInitString )
      ? "No connection..."
      RETURN Nil
   ENDIF

   oStyle := eGUI_CreateStyle( , {CLR_LBLUE0,CLR_LBLUE4}, 1 )
   oMainWindow := eGUI_InitMainWindow( 100, 100, 400, 350, "extGUI test", ;
      {{"bcolor",CLR_LBLUE3}} )

   egui_Menu()
      egui_Menu( "File" )
         eGUI_AddMenuItem( "New", "newfile", "New file item" )
         eGUI_AddMenuItem( "Open", "openfile" )
         eGUI_AddMenuSeparator()
         eGUI_AddMenuItem( "msgbox", "mbox" )
         eGUI_AddMenuSeparator()
         eGUI_AddMenuItem( "Exit",,"hwg_EndWindow()" )
      egui_EndMenu()
      egui_Menu( "Help" )
         eGUI_AddMenuItem( "About",, 'hwg_MsgInfo("Test","About")' )
      egui_EndMenu()
   egui_EndMenu()

   oPane := oMainWindow:AddWidget( "panel",, 0,0, 400, 36,, {{"HStyle",oStyle}})
   own1 := oPane:AddWidget( "ownbtn",, 0,0, 48, 36, "Btn1", {{"HStyles",{oStyle}}} )
   own1:SetCallbackProc( "onclick",, "hwg_MsgInfo('Test')" )

   oLabel := oMainWindow:AddWidget( "label","l1", 20,50, 180, 24, "This is a label", {{"tcolor",255},{"Transpa",.T.}} )

   oBtn := oMainWindow:AddWidget( "button",, 200,46, 100, 32, "Click!" )
   oBtn:SetCallbackProc( "onclick", "fmenu1" )

   oMainWindow:AddWidget( "group",, 20, 110, 300, 100, "Group" )
   oMainWindow:AddWidget( "check",, 40, 140, 120, 24, "Check - 1" )
   oMainWindow:AddWidget( "check",, 40, 158, 120, 24, "Check - 2" )

   oBtnOk := oMainWindow:AddWidget( "button",, 150, 250, 100, 32, "Ok" )
   oBtnOk:SetCallbackProc( "onclick", "fbtnok" )

   eGUI_ActivateMainWindow()

   eGUI_Exit()

   RETURN Nil

FUNCTION fmenu1()

   oLabel:SetText( "Just a test" )
   RETURN Nil

FUNCTION fbtnok()

   LOCAL s := egui_EvalFunc( 'Return GetWidg("main.l1"):GetText()' )
   ? s
   RETURN Nil

FUNCTION newfile( arr )

   oLabel:SetText( arr[1] )
   RETURN Nil

FUNCTION openfile()

   LOCAL oBtnOk, oFont, arr1 := { "First", "Second", "Third"}

   oFont := eGUI_CreateFont( "f1", "Georgia", 22 )
   oDlg := eGUI_InitDialog( 300, 200, 220, 400, "GetValues", {{"Font",oFont}} )

   oDlg:AddWidget( "label",, 20,20, 180, 24, "Имя:" )
   oEdi1 := oDlg:AddWidget( "edit","edi1", 20,44, 180, 26 )

   oDlg:AddWidget( "label",, 20,72, 180, 24, "Фамилия:" )
   oDlg:AddWidget( "edit","edi2", 20,96, 180, 26 )

   oDlg:AddWidget( "radiogr","rg", 20,130, 180, 80, "Group" )
   oDlg:AddWidget( "radio",, 50,160, 120, 24, "radio1" )
   oDlg:AddWidget( "radio",, 50,186, 120, 24, "radio2" )

   oDlg:AddWidget( "combo",, 20,220, 180, 26,, {{"AItems",arr1}})

   oBtnOk := oDlg:AddWidget( "button",, 50, 350, 100, 32, "Ok" )
   //oBtnOk:SetCallbackProc( "onclick",,"hwg_EndDialog()" )
   oBtnOk:SetCallbackProc( "onclick","fclose" )

   eGUI_ActivateDialog()

   RETURN Nil

FUNCTION fclose()

   LOCAL arr
   ? oEdi1:GetText()
   arr := egui_GetValues( oDlg, {"edi1","edi2","rg"} )
   IF Valtype(arr) == "A"
      ? arr[1], arr[2]
      ? arr[3]
   ENDIF
   oDlg:Close()

   RETURN Nil

FUNCTION MBox( aParams )

   IF aParams == Nil
      egui_MsgInfo( "Test1", "Msgbox", "mbox", "mm1" )
   ELSEIF aParams[1] == "mm1"
      egui_MsgInfo( "Test2", "Msgbox" )
   ENDIF


   RETURN Nil
