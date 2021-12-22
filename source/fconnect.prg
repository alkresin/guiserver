/*
 */

/*
 client:
   s := gs_ConnectSocket()             ->  client_conn_Create( cFile )
   gs_Send2SocketOut( '+' + s + cn )   ->  client_conn_Send2SocketOut( s )
   gs_Send2SocketIn( s )               ->  client_conn_Send2SocketIn( s )
   gs_CheckSocket()                    ->  client_conn_CheckIn()
   cBuffer := gs_GetRecvBuffer()       ->  conn_GetRecvBuffer()

 server:
   gs_CreateSocket( nPort )            ->  srv_conn_Create( cFile )
   gs_ListenSocket()
   gs_CheckSocket()                    ->  srv_conn_CheckIn()
   gs_CheckSockError()
   cBuffer := gs_GetRecvBuffer()       ->  conn_GetRecvBuffer()
   gs_Send2SocketOut( "+" + s + cn )   ->  srv_conn_Send2SocketOut( s )
   gs_Send2SocketIn( s )               ->  srv_conn_Send2SocketIn( s )
 */

#include "fileio.ch"
#define  BUFFLEN   512

STATIC handlIn, handlOut, cBufferIn, cBufferOut, cBuffRes
STATIC lActive := .F.

FUNCTION conn_Read( lOut )

   LOCAL n, nPos, s := ""
   LOCAL han := Iif( lOut, handlOut, handlIn )
   LOCAL cBuffer := Iif( lOut, cBufferOut, cBufferIn )

   FSeek( han, 1, 0 )
   DO WHILE ( n := FRead( han, @cBuffer, Len(cBuffer ) ) ) > 0
      IF ( nPos := At( cBuffer, Chr(10) ) ) > 0
         s += Left( cBuffer, nPos )
         EXIT
      ELSEIF n < Len(cBuffer )
         s += Left( cBuffer, n )
         EXIT
      ELSE
         s += cBuffer
      ENDIF
   ENDDO

   cBuffRes := s

   RETURN Len( s )

FUNCTION conn_GetRecvBuffer()

   RETURN Substr( cBuffRes, 2 )

/*  ----  Server functions ---- */

FUNCTION srv_conn_Create( cFile )

   handlIn := FCreate( cFile + ".gs1" )
   FClose( handl1 )

   handlOut := FCreate( cFile + ".gs2" )
   FClose( handl2 )

   handlIn := FOpen( cFile + ".gs1", FO_READWRITE + FO_SHARED )
   handlOut := FOpen( cFile + ".gs2", FO_READWRITE + FO_SHARED )

   cBufferIn := Space( BUFFLEN )
   cBufferOut := Space( BUFFLEN )
   lActive := .T.

   RETURN .T.

FUNCTION srv_conn_Listen()

   RETURN .T.

FUNCTION srv_conn_CheckIn()

   FSeek( handlIn, 0, 0 )
   IF FRead( handlIn, @cBufferIn, 1 ) > 0 .AND. Asc( cBufferIn ) == 1
      IF conn_Read( .F. ) > 0
         MainHandler()
      ENDIF
      RETURN .T.
   ENDIF

   RETURN .F.

FUNCTION  srv_conn_Send2SocketIn( s )

   srv_conn_Send( .F., s )

   RETURN Nil

FUNCTION  srv_conn_Send2SocketOut( s )

   srv_conn_Send( .T., s )
   DO WHILE lActive
      srv_conn_CheckIn()
      FSeek( handlOut, 0, 0 )
      IF FRead( handlOut, @cBufferOut, 1 ) > 0 .AND. Asc( cBufferOut ) == 1
         conn_Read( .T. )
         RETURN conn_GetRecvBuffer()
      ENDIF
   ENDDO

   RETURN Nil

FUNCTION srv_conn_Send( lOut, cLine )

   LOCAL han := Iif( lOut, handlOut, handlIn )

   FSeek( han, 1, 0 )
   FWrite( han, cLine )
   FSeek( han, 0, 0 )
   FWrite( han, Chr(2) )

   RETURN Nil

/*  ----  Client functions ---- */

FUNCTION client_conn_Connect( cFile )

   handlOut := FOpen( cFile + ".gs1", FO_READWRITE + FO_SHARED )
   handlIn := FOpen( cFile + ".gs2", FO_READWRITE + FO_SHARED )

   cBufferIn := Space( BUFFLEN )
   cBufferOut := Space( BUFFLEN )

   lActive := .T.

   RETURN .T.

FUNCTION client_conn_CheckIn()

   FSeek( handlIn, 0, 0 )
   IF FRead( handlIn, @cBufferIn, 1 ) > 0 .AND. Asc( cBufferIn ) == 2
      IF conn_Read( .F. ) > 0
         MainHandler()
      ENDIF
      RETURN .T.
   ENDIF

   RETURN .F.

FUNCTION  client_conn_Send2SocketIn( s )

   client_conn_Send( .F., s )

   RETURN Nil

FUNCTION  client_conn_Send2SocketOut( s )

   client_conn_Send( .T., s )
   DO WHILE lActive
      srv_conn_CheckIn()
      FSeek( handlOut, 0, 0 )
      IF FRead( handlOut, @cBufferOut, 1 ) > 0 .AND. Asc( cBufferOut ) == 2
         conn_Read( .T. )
         RETURN conn_GetRecvBuffer()
      ENDIF
   ENDDO

   RETURN Nil

FUNCTION client_conn_Send( lOut, cLine )

   LOCAL han := Iif( lOut, handlOut, handlIn )

   FSeek( han, 1, 0 )
   FWrite( han, cLine )
   FSeek( han, 0, 0 )
   FWrite( han, Chr(1) )

   RETURN Nil

PROCEDURE conn_Exit

   lActive := .F.
   FClose( handlIn )
   FClose( handlOut )

   RETURN
