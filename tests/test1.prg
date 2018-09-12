/*
 * This test demonstrates using of a main window form, created by Designer
 */

FUNCTION Main

   eGUI_Init()

   eGUI_OpenMainForm( "..\forms\example.xml" )

   eGUI_Exit()

   RETURN Nil

FUNCTION fmenu1()

   eGUI_EvalProc( 'oLabel1:SetText("Hoho")' )

   RETURN Nil