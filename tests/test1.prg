
FUNCTION Main

   LOCAL cn := e"\n", n := 0

   eGUI_Init()

   eGUI_OpenMainForm( "..\forms\example.xml" )

   eGUI_Exit()

   RETURN Nil

FUNCTION fmenu1()

   eGUI_EvalProc( 'oLabel1:SetText("Hoho")' )

   RETURN Nil