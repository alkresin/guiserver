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

#define  ES_MULTILINE       4

Memvar oLastWidget

FUNCTION Main

   LOCAL oMainWindow, oPane, oEdi, oTree, oStyle1, oStyle2, oStyle3, oStyle4
   LOCAL cInitString := Memoread( "test.ini" )

   IF !eGUI_Init( cInitString )
      ? "No connection..."
      RETURN Nil
   ENDIF

   eGUI_SetImagePath( "images/" )

   oStyle1 := eGUI_CreateStyle( , {CLR_LBLUE,CLR_LBLUE3}, 1 )
   oStyle2 := eGUI_CreateStyle( , {CLR_LBLUE}, 1,, 3 )
   oStyle3 := eGUI_CreateStyle( , {CLR_LBLUE}, 1,, 2, CLR_LBLUE0 )
   oStyle4 := eGUI_CreateStyle( , {CLR_LBLUE2,CLR_LBLUE3}, 1,, 1, CLR_LBLUE )

   oMainWindow := eGUI_InitMainWindow( 100, 100, 400, 280, "extGUI test", ;
      {{"bcolor",CLR_LBLUE3}} )

   oPane := oMainWindow:AddWidget( "paneltop",, 0,0, 400, 36,, {{"HStyle",oStyle1}})

   oPane:AddWidget( "ownbtn",, 0,0, 64, 36, "Date", {{"HStyles",{oStyle1,oStyle2,oStyle3}}} )
   oLastWidget:SetCallbackProc( "onclick",, "hwg_WriteStatus(HWindow():GetMain(),1,Dtoc(Date()),.T.)" )

   //oPane := oMainWindow:AddWidget( "panel",, 0, 40, 200, 208 )
   //oPane:SetCallBackProc( "onsize",, "{|o,x,y|o:Move(,,,y-72)}")

   oTree := oMainWindow:AddWidget( "tree",, 0,40, 200, 208,, {{"AImages",{"cl_fl.bmp","op_fl.bmp"}}}  )
   oTree:SetCallBackProc( "onsize",, "{|o,x,y|o:Move(,,,y-72)}")
   eGUI_InsertNode( oTree, "", "n1", "First" )
   eGUI_InsertNode( oTree, "", "n2", "Second" )
   eGUI_InsertNode( oTree, "n2", "n2a", "second-1",, {"book.bmp"},, "hwg_MsgInfo('n2a')" )
   eGUI_InsertNode( oTree, "", "n3", "Third" )

   oEdi := oMainWindow:AddWidget( "edit","edim", 204,40, 196, 180,, {{"Winstyle",ES_MULTILINE}})
   oEdi:SetCallBackProc( "onsize",, "{|o,x,y|o:Move(,,x-o:nLeft,y-72)}")

   oMainWindow:AddWidget( "splitter",, 200,40, 4, 208,, {{"ALeft",{oTree}},{"ARight",{oEdi}}} )
   oLastWidget:SetCallbackProc( "onsize",, "{|o,x,y|o:Move(,,,y-72)}" )

   oMainWindow:AddWidget( "panelbot",, 0,0, 400, 32,, {{"HStyle",oStyle4},{"AParts",{120,120,0}}})

   eGUI_ActivateMainWindow()

   eGUI_Exit()

   RETURN Nil

