/*
 * This test demonstrates using of a dialog form, created by Designer
 */

FUNCTION Main

   LOCAL cInitString := ""

   IF !eGUI_Init( cInitString )
      ? "No connection..."
      RETURN Nil
   ENDIF

   eGUI_InitMainWindow( 100, 100, 400, 350, "extGUI test" )

   egui_Menu()
      egui_Menu( "File" )
         eGUI_AddMenuItem( "Open form", "openfile" )
         eGUI_AddMenuSeparator()
         eGUI_AddMenuItem( "Exit",,"hwg_EndWindow()" )
      egui_EndMenu()
      egui_Menu( "Help" )
         eGUI_AddMenuItem( "About",, 'hwg_MsgInfo("Dialog form testing","About")' )
      egui_EndMenu()
   egui_EndMenu()

   eGUI_ActivateMainWindow()

   eGUI_Exit()

   RETURN Nil

FUNCTION openfile()

   eGUI_OpenForm( "..\forms\testget2.xml" )

   RETURN Nil
