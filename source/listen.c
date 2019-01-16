/*
 * GuiServer
 * a set of connection functions
 *
 * Copyright 2018 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbapifs.h"

#define PROTOCOL_VER "1.1"

#if defined( HB_OS_WIN_32 )
   #define HB_SOCKET_T SOCKET
#else
   #define HB_SOCKET_T int
#endif
#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
  #include <unistd.h>
  #include <sys/time.h>
  #include <sys/timeb.h>
#else
  #include <windows.h>
#endif

#define HB_SENDRECV_BUFFER_SIZE         16384
#define TIMEOUT 4000

void gs_ipInit( void );
void gs_ipCleanup( void );
int gs_ipclose( HB_SOCKET_T hSocket );
int gs_iperrorcode( void );
int gs_ipDataReady( HB_SOCKET_T hSocket, int timeout );

void gs_ip_rfd_zero( void );
int gs_ip_rfd_select( int iTimeOut );
void gs_ip_rfd_set( HB_SOCKET_T hSocket );
void gs_ip_rfd_clr( HB_SOCKET_T hSocket );
int gs_ip_rfd_isset( HB_SOCKET_T hSocket );
void hb_getLocalIP( HB_SOCKET_T hSocket, char * szIP );

HB_SOCKET_T gs_ipServer( int iPort, const char * szAddress, int iListen );
HB_SOCKET_T gs_ipAccept( HB_SOCKET_T hSocket, int timeout, char * szAddr, long int * lPort );
HB_SOCKET_T gs_ipConnect( const char * szHost, int iPort, int timeout );
int gs_ipSend( HB_SOCKET_T hSocket, const char *szBuffer, int iSend, int timeout );
int gs_ipRecv( HB_SOCKET_T hSocket, char * szBuffer, int iBufferLen );

static int iIpActive = 0;
static int iServerPort;
static int bSocketError = 0;
static HB_SOCKET_T hSocketMain1 = (HB_SOCKET_T)-1, hSocketMain2 = (HB_SOCKET_T)-1, hSocketIn = (HB_SOCKET_T)-1, hSocketOut = (HB_SOCKET_T)-1;
static char * pBufferIn = NULL, * pBufferOut = NULL;
static long int lBufferInLen = 0, lBufferOutLen = 0, lLastRcvIn = 0, lLastRcvOut = 0;
static const char * pLogFile = NULL;
static char szPrefix[16] = "";
static char szVersion[16] = "1.0";
int iSockIn_Check = 0;

static PHB_DYNS s_pSymHandler = NULL;

static void sleep_ns( long int milliseconds )
{
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
   Sleep( milliseconds );
#else
   struct timeval tv;
   tv.tv_sec = milliseconds / 1000;
   tv.tv_usec = milliseconds % 1000 * 1000;
   select(0, NULL, NULL, NULL, &tv);
#endif
}

static unsigned long milliSec( void )
{
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
   SYSTEMTIME st;
   GetLocalTime( &st );
   return (st.wMinute * 60 + st.wSecond) * 1000 + st.wMilliseconds;
#elif ( defined( HB_OS_LINUX ) || defined( HB_OS_BSD ) ) && !defined( __WATCOMC__ )
   struct timeval tv;
   gettimeofday( &tv, NULL );
   return tv.tv_sec * 1000 + tv.tv_usec / 1000;
#else
   struct timeb tb;
   ftime( &tb );
   return tb.time * 1000 + tb.millitm;
#endif
}

static void _writelog( const char * sFile, int n, const char * s, ... )
{

   if( !sFile )
      return;

   if( n )
   {
      HB_FHANDLE handle;
      if( hb_fsFile( sFile ) )
         handle = hb_fsOpen( sFile, FO_WRITE );
      else
         handle = hb_fsCreate( sFile, 0 );

      hb_fsSeek( handle,0, SEEK_END );
      hb_fsWrite( handle, s, n );
      hb_fsWrite( handle, "\r\n", 2 );
      hb_fsClose( handle );
   }
   else
   {
      FILE * hFile = hb_fopen( sFile, "a" );

      va_list ap;
      if( hFile )
      {
         fprintf( hFile, "%s%lu> ", szPrefix, milliSec() );
         va_start( ap, s );
         vfprintf( hFile, s, ap );
         va_end( ap );
         fclose( hFile );
      }
   }
}

static unsigned long s2l16( char * ptr1, char * ptr2 )
{
   char szbuff[16];
   int ilen = ptr2-ptr1+1;

   if( ptr2 >= ptr1 )
   {
      ilen = (ilen>15)? 15 : ilen;
      memcpy( szbuff, ptr1, ilen );
      szbuff[ilen] = '\0';
      return strtoul( ptr1, NULL, 16 );
   }
   else
      return 0;
}

static void return_buffer( int bOut )
{
   char * ptr = (bOut)? pBufferOut : pBufferIn;
   long int lLastReceived = (bOut)? lLastRcvOut : lLastRcvIn;
   if( *ptr == '+' )
      hb_retclen_const( (const char*) (ptr+1), lLastReceived-1 );
   else if( *ptr == '!' )
      hb_retclen_const( (const char*) (ptr+8), lLastReceived-8 );
}

static long int socket_Recv( HB_SOCKET_T hSocket, int iTimeout, char ** pBuffer, long int * pBufferLen, long int * pLastReceived )
{
   int iRet;
   char szRet[HB_SENDRECV_BUFFER_SIZE], * ptr;
   long int lLen = 0, lLenRcv;
   unsigned long ulms = milliSec();

   if( hSocket == (HB_SOCKET_T)-1 || !pBuffer )
      return 0;

   ptr = *pBuffer;
   *pLastReceived = 0;
   do
   {
      _writelog( pLogFile, 0, "recv1: %d\r\n", hSocket );
      while( gs_ipDataReady( hSocket,2 ) == 0 )
      {
         if( iTimeout > 0 && iTimeout < (int)(milliSec()-ulms) )
		 {
		    _writelog( pLogFile, 0, "recv1a\r\n" );
            return -1;
		 }
      }

      iRet = gs_ipRecv( hSocket, szRet, HB_SENDRECV_BUFFER_SIZE );
      _writelog( pLogFile, 0, "recv2: %d\r\n", iRet );
      if( iRet <= 0 )
	  {
	     _writelog( pLogFile, 0, "recv2a \r\n" );
         return -1;
      }
      else
      {
         if( ptr == *pBuffer && ( *szRet != '!' && *szRet != '+' ) )
         {
            szRet[iRet] = '\0';
            _writelog( pLogFile, 0, "recv3: %s\r\n", szRet );
            break;
         }
         if( (ptr - *pBuffer) + iRet > *pBufferLen )
         {
            char * szTemp;
            *pBufferLen += HB_SENDRECV_BUFFER_SIZE;
            szTemp = (char*) malloc( *pBufferLen );
            memcpy( szTemp, *pBuffer, ptr-*pBuffer );
            ptr = szTemp + (ptr-*pBuffer);
            free( *pBuffer );
            *pBuffer = szTemp;
            _writelog( pLogFile, 0, "recv4: %ld\r\n", *pBufferLen );
         }
         memcpy( ptr, szRet, iRet );
         ptr += iRet;
         lLenRcv = ptr-*pBuffer;
         if( !lLen && **pBuffer == '!' && lLenRcv > 8 )
            lLen = s2l16( *pBuffer+1, *pBuffer+7 );
         if( ( lLen > 0 && lLen <= lLenRcv ) ||
               ( !lLen && *(ptr-1) == '\n' ) )
            break;
      }
   }
   while(1);

   if( !lLen && *(ptr-1) == '\n' )
      ptr --;
   *ptr = '\0';
   *pLastReceived = (long int) (ptr - *pBuffer);
   _writelog( pLogFile, 0, "recv10: %d %s\r\n", *pLastReceived, *pBuffer );
   return *pLastReceived;
}

static long int sockIn_Recv( int iTimeout )
{
   long int lRes;
   _writelog( pLogFile, 0, "Inrecv1 %d\r\n", hSocketIn );
   lRes = socket_Recv( hSocketIn, iTimeout, &pBufferIn, &lBufferInLen, &lLastRcvIn );
   _writelog( pLogFile, 0, "Inrecv2 %ld \r\n", lRes );
   return lRes;
}

static long int sockOut_Recv( int iTimeout )
{
   long int lRes;
   _writelog( pLogFile, 0, "Outrecv1 %d\r\n", hSocketOut );
   lRes = socket_Recv( hSocketOut, iTimeout, &pBufferOut, &lBufferOutLen, &lLastRcvOut );
   _writelog( pLogFile, 0, "Outrecv2 %ld \r\n", lRes );
   return lRes;
}

static void socket_Send( HB_SOCKET_T hSocket, const char* szData, unsigned long ulLen )
{

   if( hSocket != (HB_SOCKET_T)-1 )
   {
      gs_ipSend( hSocket, szData, ulLen, -1 );
   }
}

static int sock_Listen( void )
{
   char szBuf[64];
   HB_SOCKET_T incoming;
   long int lTemp;

   if( iIpActive && gs_ip_rfd_select( 1 ) > 0 )
   {
      _writelog( pLogFile, 0, "listen-0\r\n" );
      if( hSocketMain1 != ((HB_SOCKET_T)-1) && gs_ip_rfd_isset( hSocketMain1 ) )
      {
         _writelog( pLogFile, 0, "listen-1\r\n" );
         incoming = gs_ipAccept( hSocketMain1, -1, szBuf, &lTemp );
         if( !gs_iperrorcode() )
         {

            gs_ip_rfd_set( hSocketMain2 );
            hSocketIn = incoming;
            gs_ip_rfd_set( incoming );

            sprintf( szBuf, "+v%s/%s\n", szVersion, PROTOCOL_VER );
            socket_Send( hSocketIn, szBuf, strlen(szBuf) );
            _writelog( pLogFile, 0, "hSocketIn: %lu\r\n", hSocketIn );

            gs_ip_rfd_clr( hSocketMain1 );
            gs_ipclose( hSocketMain1 );
            hSocketMain1 = (HB_SOCKET_T)-1;
            return 1;
         }
      }
      if( hSocketMain2 != ((HB_SOCKET_T)-1) && gs_ip_rfd_isset( hSocketMain2 ) )
      {
         _writelog( pLogFile, 0, "listen-2\r\n" );
         incoming = gs_ipAccept( hSocketMain2, -1, szBuf, &lTemp );
         if( !gs_iperrorcode() )
         {

            hSocketOut = incoming;
            gs_ip_rfd_set( incoming );

            sprintf( szBuf, "+OK\n" );
            socket_Send( hSocketOut, szBuf, strlen(szBuf) );
            _writelog( pLogFile, 0, "hSocketOut: %lu\r\n", hSocketOut );

            gs_ip_rfd_clr( hSocketMain2 );
            gs_ipclose( hSocketMain2 );
            hSocketMain2 = (HB_SOCKET_T)-1;
            return 1;
         }
      }
   }
   return 0;
}

static void runHandler( void )
{
   if( s_pSymHandler )
   {
      unsigned long ulms = milliSec();
      _writelog( pLogFile, 0, "run_handler_1\r\n" );
      hb_vmPushDynSym( s_pSymHandler );
      hb_vmPushNil();
      hb_vmDo( 0 );
      _writelog( pLogFile, 0, "run_handler_2 at: %lu\r\n", ulms );
   }
}

static int sockIn_Check( void )
{

   iSockIn_Check ++;
   if( iIpActive && hSocketIn != (HB_SOCKET_T)-1 && gs_ipDataReady( hSocketIn,2 ) != 0 )
   {
      _writelog( pLogFile, 0, "check-1 %d\r\n", iSockIn_Check );
      if( sockIn_Recv( TIMEOUT ) > 0 )
      {
         runHandler();
         iSockIn_Check --;
         return 1;
      }
      else if( gs_iperrorcode() )
      {
         gs_ip_rfd_clr( hSocketIn );
         gs_ipclose( hSocketIn );
         hSocketIn = (HB_SOCKET_T)-1;
         bSocketError = 1;
         _writelog( pLogFile, 0, "socket error!\r\n" );
      }
   }
   iSockIn_Check --;
   return 0;

}

HB_FUNC( GS_PROTO_VERSION )
{
   hb_retc( PROTOCOL_VER );
}

HB_FUNC( GS_SLEEP_NS )
{
   sleep_ns( hb_parni(1) );
}

HB_FUNC( GS_MILLISEC )
{
   hb_retnl( milliSec() );
}

HB_FUNC( GS_GETRECVBUFFER )
{

   return_buffer( 0 );
}

HB_FUNC( GS_GETRECVBUFFEROUT )
{

   return_buffer( 1 );
}

HB_FUNC( GS_SETLOGFILE )
{
   pLogFile = hb_parc(1);
}

HB_FUNC( GS_SETPREFIX )
{
   strcpy( szPrefix, hb_parc(1) );
}

HB_FUNC( GS_SETVERSION )
{
   strcpy( szVersion, hb_parc(1) );
}

HB_FUNC( GS_SETHANDLER )
{
   s_pSymHandler = hb_dynsymGetCase( hb_parc(1) );
   if( hb_dynsymIsFunction( s_pSymHandler ) )
      hb_retl(1);
   else
      hb_retl(0);
}

HB_FUNC( GS_CHECKSOCKERROR )
{
   hb_retl( bSocketError );
}

HB_FUNC( GS_SEND2SOCKETIN )
{
   const char *szBuf = hb_parc(1);

   if( iIpActive && hSocketIn != (HB_SOCKET_T)-1 )
   {
      socket_Send( hSocketIn, szBuf, strlen(szBuf) );
      _writelog( pLogFile, 0, "sendIn: %s \r\n", szBuf );
   }
}

HB_FUNC( GS_SEND2SOCKETOUT )
{
   const char *szBuf = hb_parc(1);
   unsigned long ulms = milliSec();

   _writelog( pLogFile, 0, "sendOut: %s \r\n", szBuf );
   if( iIpActive && hSocketOut != (HB_SOCKET_T)-1 )
   {
      _writelog( pLogFile, 0, "sendOut2\r\n" );
      socket_Send( hSocketOut, szBuf, strlen(szBuf) );
      while( iIpActive )
      {
         sockIn_Check();
         if( bSocketError )
            return;
         if( gs_ipDataReady( hSocketOut,2 ) != 0 )
         {
            if( sockOut_Recv( TIMEOUT ) > 0 )
            {
               return_buffer( 1 );
            }
            break;
         }
         else if( gs_iperrorcode() || TIMEOUT < (int)(milliSec()-ulms) )
         {
            _writelog( pLogFile, 0, "sendOut2a started: %lu\r\n", ulms );
            return;
         }
      }
      _writelog( pLogFile, 0, "sendOut3 %d started: %lu\r\n", lLastRcvOut, ulms );
   }        
}

HB_FUNC( GS_CONNECTSOCKET )
{
   HB_SOCKET_T hSocket;
   const char * szAddr = hb_parc(1);
   int iPort = hb_parni(2);
   unsigned long ulms = milliSec();

   //_writelog( pLogFile, 0, "conn: %d %d\r\n", iPort, htons( iPort ) );
   //hSocket = gs_ipConnect( szAddr, htons( iPort ), TIMEOUT );
   hSocket = gs_ipConnect( szAddr, iPort, TIMEOUT*2 );
   if( !gs_iperrorcode() )    
   {
      hSocketOut = hSocket;
      _writelog( pLogFile, 0, "conn1 Ok %d %lu\r\n", hSocket, ulms );
      while( iIpActive )
      {
         if( gs_ipDataReady( hSocketOut,2 ) != 0 )
         {
            if( sockOut_Recv( TIMEOUT ) > 0 )
            {
               hSocket = gs_ipConnect( szAddr, iPort+1, TIMEOUT*2 );
               if( !gs_iperrorcode() )    
               {
                  hSocketIn = hSocket;
                  gs_ip_rfd_set( hSocketIn );
                  _writelog( pLogFile, 0, "conn2 Ok %d %lu\r\n", hSocket, ulms );
                  while( iIpActive )
                  {
                     if( gs_ipDataReady( hSocketIn,2 ) != 0 )
                     {
                        if( sockIn_Recv( TIMEOUT ) > 0 )
                        {
                           return_buffer( 1 );
                           return;
                        }
                     }
                  }
               }
            }
            break;
         }
         else if( gs_iperrorcode() || TIMEOUT < (int)(milliSec()-ulms) )
         {
            _writelog( pLogFile, 0, "connSock2a:\r\n" );
            return;
         }
      }
   }
   hb_ret();
}

HB_FUNC( GS_CREATESOCKET )
{

   gs_ip_rfd_zero();

   iServerPort = hb_parni(1);

   _writelog( pLogFile, 0, "crsocket: %d\r\n", iServerPort );
   if( ( hSocketMain1 = gs_ipServer( iServerPort, NULL, 10 ) ) == (HB_SOCKET_T)-1 )
      return;
   gs_ip_rfd_set( hSocketMain1 );
   _writelog( pLogFile, 0, "main socket1: %lu\r\n", hSocketMain1 );
   if( ( hSocketMain2 = gs_ipServer( iServerPort+1, NULL, 10 ) ) == (HB_SOCKET_T)-1 )
      return;
   _writelog( pLogFile, 0, "main socket2: %lu\r\n", hSocketMain2 );
}

HB_FUNC( GS_LISTENSOCKET )
{
   if( hSocketMain1 != ((HB_SOCKET_T)-1) || hSocketMain2 != ((HB_SOCKET_T)-1) )
      hb_retl( sock_Listen() );
   else
      hb_retl( 1 );
}

HB_FUNC( GS_CHECKSOCKET )
{
   hb_retl( sockIn_Check() );
}

HB_FUNC( GS_GETLOCALIP )
{
   char szIP[24];

   *szIP = '\0';
   if( iIpActive && hSocketIn != (HB_SOCKET_T)-1  )
   {
      hb_getLocalIP( hSocketIn, szIP );
   }
   hb_retc( szIP );
}

HB_FUNC( GS_IPINIT )
{
   if( !iIpActive )
   {
      gs_ipInit();
      lBufferInLen = lBufferOutLen = HB_SENDRECV_BUFFER_SIZE;
      pBufferIn = (char*) malloc(lBufferInLen);
      pBufferOut = (char*) malloc(lBufferOutLen);
      iIpActive = 1;
   }

}

HB_FUNC( GS_IPEXIT )
{
   if( iIpActive )
   {
      if( hSocketMain1 != (HB_SOCKET_T)-1 ) {
         gs_ipclose( hSocketMain1 );
         hSocketMain1 = (HB_SOCKET_T)-1;
      }
      if( hSocketMain2 != (HB_SOCKET_T)-1 ) {
         gs_ipclose( hSocketMain2 );
         hSocketMain2 = (HB_SOCKET_T)-1;
      }
      if( hSocketIn != (HB_SOCKET_T)-1 ) {
         gs_ipclose( hSocketIn );
         hSocketIn = (HB_SOCKET_T)-1;
      }
      if( hSocketOut != (HB_SOCKET_T)-1 ) {
         gs_ipclose( hSocketOut );
         hSocketOut = (HB_SOCKET_T)-1;
      }
      gs_ipCleanup();
      lBufferInLen = lBufferOutLen = 0;
      if( pBufferIn ) {
         free( pBufferIn );
         pBufferIn = NULL;
      }
      if( pBufferOut ) {
         free( pBufferOut );
         pBufferOut = NULL;
      }
 
      iIpActive = 0;
   }
}
