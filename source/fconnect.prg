/*
 */

/*
 client:
   s := gs_ConnectSocket()             ->  client_conn_Create( cFile )
   gs_Send2SocketOut( '+' + s + cn )   ->  conn_Send2SocketOut( s )
   gs_Send2SocketIn( s )               ->  conn_Send2SocketIn( s )
   gs_CheckSocket()                    ->  conn_CheckIn()
   cBuffer := gs_GetRecvBuffer()       ->  conn_GetRecvBuffer()

 server:
   gs_CreateSocket( nPort )            ->  srv_conn_Create( cFile )
   gs_ListenSocket()                       xxx
   gs_CheckSocket()                    ->  conn_CheckIn()
   gs_CheckSockError()                     xxx
   cBuffer := gs_GetRecvBuffer()       ->  conn_GetRecvBuffer()
   gs_Send2SocketOut( "+" + s + cn )   ->  conn_Send2SocketOut( s )
   gs_Send2SocketIn( s )               ->  conn_Send2SocketIn( s )
 */

#include "fileio.ch"
#define PROTOCOL_VER "1.1"
#define  BUFFLEN   512

STATIC handlIn := -1, handlOut := -1, cBufferIn, cBufferOut, cBuffRes
STATIC lActive := .F., cVersion := "1.0"
STATIC nMyId, nHisId

FUNCTION conn_SetVersion( s )

   cVersion := s
   RETURN Nil

FUNCTION conn_Read( lOut )

   LOCAL n, nPos, s := ""
   LOCAL han := Iif( lOut, handlOut, handlIn )
   LOCAL cBuffer := Iif( lOut, cBufferOut, cBufferIn )

   FSeek( han, 1, 0 )
   DO WHILE ( n := FRead( han, @cBuffer, Len(cBuffer ) ) ) > 0
      IF ( nPos := At( Chr(10), cBuffer ) ) > 0
         s += Left( cBuffer, nPos-1 )
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

FUNCTION conn_Send( lOut, cLine )

   LOCAL han := Iif( lOut, handlOut, handlIn )

   IF lActive
      FSeek( han, 1, 0 )
      FWrite( han, cLine )
      FSeek( han, 0, 0 )
      FWrite( han, Chr(nMyId) )
   ENDIF

   RETURN Nil

FUNCTION conn_Send2SocketIn( s )

   IF lActive
      conn_Send( .F., s )
   ENDIF

   RETURN Nil

FUNCTION conn_Send2SocketOut( s )

   IF lActive
      conn_Send( .T., s )
      DO WHILE lActive
         conn_CheckIn()
         FSeek( handlOut, 0, 0 )
         IF FRead( handlOut, @cBufferOut, 1 ) > 0 .AND. Asc( cBufferOut ) == nHisId
            conn_Read( .T. )
            RETURN conn_GetRecvBuffer()
         ENDIF
      ENDDO
   ENDIF

   RETURN Nil

FUNCTION conn_CheckIn()

   IF lActive
      FSeek( handlIn, 0, 0 )
      IF FRead( handlIn, @cBufferIn, 1 ) > 0 .AND. Asc( cBufferIn ) == nHisId
         IF conn_Read( .F. ) > 0
            MainHandler()
         ENDIF
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION srv_conn_Create( cFile )

   nMyId := 2
   nHisId := 1

   handlIn := FCreate( cFile + ".gs1" )
   FClose( handlIn )

   handlOut := FCreate( cFile + ".gs2" )
   FClose( handlOut )

   handlIn := FOpen( cFile + ".gs1", FO_READWRITE + FO_SHARED )
   handlOut := FOpen( cFile + ".gs2", FO_READWRITE + FO_SHARED )

   cBufferIn := Space( BUFFLEN )
   cBufferOut := Space( BUFFLEN )

   lActive := ( handlIn >= 0 .AND. handlOut >= 0 )

   conn_Send( .F., "+v" + cVersion + "/" + PROTOCOL_VER + Chr(10) )
   conn_Send( .T., "+Ok" + Chr(10) )

   RETURN lActive

FUNCTION client_conn_Connect( cFile )

   nMyId := 1
   nHisId := 2

   handlOut := FOpen( cFile + ".gs1", FO_READWRITE + FO_SHARED )
   handlIn := FOpen( cFile + ".gs2", FO_READWRITE + FO_SHARED )

   cBufferIn := Space( BUFFLEN )
   cBufferOut := Space( BUFFLEN )

   lActive := ( handlIn >= 0 .AND. handlOut >= 0 )

   RETURN lActive

PROCEDURE conn_Exit

   lActive := .F.
   FClose( handlIn )
   FClose( handlOut )

   RETURN
