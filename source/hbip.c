/*
 * xHarbour Project source code:
 *    The internet protocol / TCP support
 *
 * Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
 *                Ron Pinkas [Ron@RonPinkas.com]
 *                Marcelo Lombardo [marcelo.lombardo@newage-software.com.br]
 * www - http://www.xharbour.org
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    updated and ported to Harbour
 * www - http://www.harbour-project.org
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 * updated for Leto db server
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#define HB_LEGACY_LEVEL4
#define HB_LEGACY_TYPES_ON

#include "hbdefs.h"

#if defined( __XHARBOUR__ ) || (defined( __HARBOUR__ ) && __HARBOUR__ - 0 < 0x000100)
   #define HB_SIZE ULONG
#endif

/* Compile in Unix mode under Cygwin */
#ifdef OS_UNIX_COMPATIBLE
  #undef HB_OS_WIN_32
  #undef HB_OS_WIN
#endif

/* HB_INET_H_ */
#include <string.h>

#ifndef HB_ERR_FUNCNAME
   #define HB_ERR_FUNCNAME &hb_errFuncName
#endif

#if defined( HB_OS_WIN_32 ) || defined( HB_OS_WIN ) 
   #define _WINSOCKAPI_  /* Prevents inclusion of Winsock.h in Windows.h */
   #define HB_SOCKET_T SOCKET
   #include <winsock2.h>
   #include <windows.h>
   #include <process.h>

   #define HB_IP_CLOSE( x )    closesocket( x )
   #define socklen_t int
#else

   #define _MULTI_THREADED
   #include <pthread.h>

   #define HB_SOCKET_T int
   #include <unistd.h>
   #include <sys/types.h>
   #include <sys/socket.h>
   #include <netdb.h>
   #include <netinet/in.h>
   #include <netinet/tcp.h>
   #include <arpa/inet.h>

   #if defined(__WATCOMC__)
      #define h_errno errno
   #else
      extern int h_errno;
   #endif
   #define HB_IP_CLOSE( x )    close( x )
   #include <errno.h>
#endif

#define HB_SOCKET_ZERO_ERROR()   \
            do { errorCode = 0; errorDesc = ""; } while( 0 )

#if defined( HB_OS_WIN_32 ) || defined( HB_OS_WIN )
    #define HB_SOCKET_SET_ERROR()   \
            do { \
               errorCode = WSAGetLastError(); \
               errorDesc = strerror( errorCode );\
               WSASetLastError( 0 ); \
            } while( 0 )

#else
    #define HB_SOCKET_SET_ERROR()      \
            do { errorCode = errno; errorDesc = strerror( errno ); } while( 0 )
#endif

#define HB_SOCKET_SET_ERROR1( code )   \
            do { errorCode = code; errorDesc = strerror( code ); } while( 0 )
#define HB_SOCKET_SET_ERROR2( code, desc )   \
            do { errorCode = code; errorDesc = desc; } while( 0 )

#ifndef MSG_NOSIGNAL
   #define MSG_NOSIGNAL 0
#endif
#ifndef MSG_DONTWAIT
   #define MSG_DONTWAIT 0
#endif
#ifndef MSG_WAITALL
   #define MSG_WAITALL  0
#endif

#if !defined( HB_WINCE )
   #include <fcntl.h>
   #include <errno.h>
#endif

#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE ) || defined( HB_OS_BSD ) || defined(HB_OS_OS2)
   #include <sys/time.h>
#endif

#ifdef HB_OS_LINUX
#include <signal.h>
#define HB_IP_LINUX_INTERRUPT     SIGUSR1+90
static void gs_ipLinuxSigusrHandle( int sig )
{
   /* nothing to do */
   HB_SYMBOL_UNUSED( sig );
}
#endif

#if defined(_MSC_VER)
   #define SOCKOPT4  char*
#else
   #define SOCKOPT4  void*
#endif

static volatile int s_iSessions = 0;
static const char *errorDesc;
static int errorCode;
static fd_set rd_fds, active_fds;
static HB_SOCKET_T rd_maxfd;

int gs_iperrorcode( void )
{
   return errorCode;
}

void gs_ipInit( void )
{
   if( s_iSessions )
   {
      s_iSessions++;
   }
   else
   {
      #if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
         WSADATA wsadata;
         WSAStartup( MAKEWORD(1,1), &wsadata );
      #elif defined( HB_OS_LINUX )
         signal( HB_IP_LINUX_INTERRUPT, gs_ipLinuxSigusrHandle );
      #endif
      s_iSessions = 1;
   }
}

void gs_ipCleanup( void )
{
   if( --s_iSessions == 0 )
   {
      #if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
         WSACleanup();
      #endif
   }
}

void gs_ipSetBufSize( HB_SOCKET_T hSocket, long iBufSend, long iBufRecv )
{
   long value;
   int len = 4;//sizeof(value);

   if( iBufSend && !getsockopt( (unsigned) hSocket, (int) SOL_SOCKET, (int) SO_SNDBUF, (char*) (SOCKOPT4) &value, (socklen_t*) &len ) )
   {
       if( value < iBufSend )
       {
           value = iBufSend;
           setsockopt( (unsigned) hSocket, (int) SOL_SOCKET, (int) SO_SNDBUF, (char const *) (SOCKOPT4) &value, (socklen_t) sizeof( value ) );
       }
   }

   if( iBufRecv && !getsockopt( (unsigned) hSocket, (int) SOL_SOCKET, (int) SO_RCVBUF, (char*) (SOCKOPT4) &value, (socklen_t*) &len ) )
   {
       if( value < iBufRecv )
       {
           value = iBufRecv;
           setsockopt( (unsigned) hSocket, (int) SOL_SOCKET, (int) SO_RCVBUF, (char const *) (SOCKOPT4) &value, (socklen_t) sizeof( value ) );
       }
   }
}

int gs_ipDataReady( HB_SOCKET_T hSocket, int timeout )
{
   fd_set set;
   struct timeval tv;

   HB_SOCKET_ZERO_ERROR();

   FD_ZERO( &set );
   FD_SET( hSocket, &set);

   if( timeout == -1 )
   {
      if( select( hSocket+1, &set, NULL, NULL, NULL ) < 0 )
      {
         HB_SOCKET_SET_ERROR();
         return 0;
      }
   }
   else
   {
      tv.tv_sec = timeout/ 1000;
      tv.tv_usec = (timeout % 1000) * 1000;
      if( select( hSocket+1, &set, NULL, NULL, &tv ) < 0 )
      {
         HB_SOCKET_SET_ERROR();
         return 0;
      }
   }

   return FD_ISSET( hSocket, &set );
}

static int hb_selectWriteSocket( HB_SOCKET_T hSocket, int timeout )
{
   fd_set set;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET( hSocket, &set );

   if( timeout == -1 )
   {
      if( select( hSocket + 1, NULL, &set, NULL, NULL ) < 0 )
         return 0;
   }
   else
   {
      tv.tv_sec = timeout/ 1000;
      tv.tv_usec = (timeout % 1000) * 1000;
      if( select( hSocket + 1, NULL, &set, NULL, &tv ) < 0 )
         return 0;
   }

   return FD_ISSET( hSocket, &set );
}

#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
static int hb_selectWriteExceptSocket( HB_SOCKET_T hSocket, int timeout )
{
   fd_set set, eset;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET( hSocket, &set );
   FD_ZERO( &eset );
   FD_SET( hSocket, &eset );

   if( timeout == -1 )
   {
      if( select( hSocket + 1, NULL, &set, &eset, NULL ) < 0 )
         return 2;
   }
   else
   {
      tv.tv_sec = timeout/ 1000;
      tv.tv_usec = (timeout % 1000) * 1000;
      if( select( hSocket + 1, NULL, &set, &eset, &tv) < 0 )
         return 2;
   }

   if( FD_ISSET( hSocket, &eset) )
   {
      return 2;
   }

   if( FD_ISSET( hSocket, &set ) )
   {
      return 1;
   }
   return 0;
}
#endif

static ULONG hb_getAddr( const char * name )
{
   ULONG ulAddr = inet_addr( name );

   if( ulAddr == INADDR_NONE )
   {
      struct hostent *Host = gethostbyname( name );

      if( Host )
         return (*(UINT *)Host->h_addr_list[0]);
      else
      {
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
         HB_SOCKET_SET_ERROR2( WSAGetLastError() , "Generic error in GetHostByName()" );
         WSASetLastError( 0 );
#elif defined(HB_OS_OS2) || defined(HB_OS_HPUX) || defined(__WATCOMC__)
         HB_SOCKET_SET_ERROR2( h_errno, "Generic error in GetHostByName()" );
#else
         HB_SOCKET_SET_ERROR2( h_errno, (char *) hstrerror( h_errno ) );
#endif
         return INADDR_NONE;
      }

   }

   return ulAddr;
}

static void hb_socketSetNonBlocking( HB_SOCKET_T hSocket )
{
#if defined (HB_OS_WIN_32) || defined( HB_OS_WIN )
   ULONG mode = 1;
   ioctlsocket( hSocket, FIONBIO, &mode );

#else
   int flags = fcntl( hSocket, F_GETFL, 0 );
   if( flags != -1 )
   {
      flags |= O_NONBLOCK;
      fcntl( hSocket, F_SETFL, (LONG) flags );
   }
#endif
}

static void hb_socketSetBlocking( HB_SOCKET_T hSocket )
{
#if defined (HB_OS_WIN_32) || defined( HB_OS_WIN )
   ULONG mode = 0;
   ioctlsocket( hSocket, FIONBIO, &mode );
#else
   int flags = fcntl( hSocket, F_GETFL, 0 );
   if( flags != -1 )
   {
      flags &= ~O_NONBLOCK;
      fcntl( hSocket, F_SETFL, ( long ) flags );
   }
#endif
}

static int hb_socketConnect( HB_SOCKET_T hSocket, struct sockaddr_in *remote, int timeout )
{
   int iErr1;
   #if ! defined(HB_OS_WIN_32) && !defined( HB_OS_WIN )
      int iErrval;
      socklen_t iErrvalLen;
   #endif
   int iOpt;

   /* we'll be using a nonblocking function */
   hb_socketSetNonBlocking( hSocket );

   iErr1 = connect( hSocket, (struct sockaddr *) remote, sizeof(*remote) );
   if( iErr1 != 0 )
   {
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
      if( WSAGetLastError() != WSAEWOULDBLOCK )
#else
      if( errno != EINPROGRESS )
#endif
      {
         HB_SOCKET_SET_ERROR();
      }
      else
      {
         /* Now we wait for socket connection or timeout */
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
         iErr1 = hb_selectWriteExceptSocket( hSocket,timeout );
         if( iErr1 == 2 )
         {
            HB_SOCKET_SET_ERROR2( 2, "Connection failed" );
         }
         else if( iErr1 == 1 )
         {
            /* success */
         }
#else
         if( hb_selectWriteSocket( hSocket,timeout ) )
         {
            /* Connection has been completed with a failure or a success */
            iErrvalLen = sizeof( iErrval );
            iErr1 = getsockopt( hSocket, SOL_SOCKET, SO_ERROR, 
                           (SOCKOPT4) &iErrval, &iErrvalLen );

            if( iErr1 )
            {
               HB_SOCKET_SET_ERROR1( iErr1 );
            }
            else if( iErrval )
            {
               HB_SOCKET_SET_ERROR1( iErrval );
            }
            /* Success! */
         }
#endif
         /* Timed out */
         else
         {
            HB_SOCKET_SET_ERROR2( -1, "Timeout" );
         }
      }
   }

   iOpt = 1;
   setsockopt( hSocket, SOL_SOCKET, SO_KEEPALIVE, (const char *) &iOpt , sizeof( iOpt ) );
   iOpt = 1;
   setsockopt( hSocket, IPPROTO_TCP, TCP_NODELAY, (const char *) &iOpt, sizeof( iOpt ) );

   hb_socketSetBlocking( hSocket );

   return ( errorCode == 0 );
}

int gs_ipRecv( HB_SOCKET_T hSocket, char * szBuffer, int iBufferLen )
{
   int iLen;
   
   HB_SOCKET_ZERO_ERROR();

   iLen = recv( hSocket, szBuffer, iBufferLen, MSG_NOSIGNAL );

   if( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( -2, "Connection closed" );
   }
   else if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR();
   }

   return iLen ;
}

int gs_ipSend( HB_SOCKET_T hSocket, const char *szBuffer, int iSend, int timeout )
{
   int iSent, iBufferLen;
   int iBufferMax;
   int iLen = sizeof( iBufferMax );

   getsockopt( (unsigned) hSocket, (int) SOL_SOCKET, (int) SO_SNDBUF, (char*) (SOCKOPT4) &iBufferMax, (socklen_t*) &iLen );
   /* iBufferMax = iBufferMax / (int) 2; */
   
   iSent = iLen = 0;

   HB_SOCKET_ZERO_ERROR();

   while( iSent < iSend )
   {
      iBufferLen = ( iBufferMax > iSend - iSent )? iSend - iSent : iBufferMax;

      iLen = 0;
      if( hb_selectWriteSocket( hSocket,timeout ) )
         iLen = send( hSocket, szBuffer + iSent, iBufferLen, MSG_NOSIGNAL );

      if( iLen > 0 )
      {
         iSent += iLen;
      }
      else if( iLen == 0 )
      {
         HB_SOCKET_SET_ERROR2( -1 , "Timeout" );
         break;
      }
      else
      {
         HB_SOCKET_SET_ERROR();
         break;
      }
   }
   if( iLen > 0 )
   {
      return iSent;
   }
   else
   {
      return -1;
   }
}

HB_SOCKET_T gs_ipConnect( const char * szHost, int iPort, int timeout )
{
   HB_SOCKET_T hSocket = -1;
   ULONG ulAddr;
   struct sockaddr_in remote;

   HB_SOCKET_ZERO_ERROR();

   ulAddr = hb_getAddr( szHost );

   /* error had been set by get hosts */
   if( ulAddr != INADDR_NONE )
   {
      /* Creates comm socket */
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
      hSocket = socket( AF_INET, SOCK_STREAM, 0);
#else
      hSocket = socket( PF_INET, SOCK_STREAM, 0);
#endif

      if( hSocket == ( HB_SOCKET_T ) -1 )
      {
         HB_SOCKET_SET_ERROR();
      }
      else
      {
         remote.sin_family = AF_INET;
         remote.sin_port= htons( iPort );
         remote.sin_addr.s_addr = ulAddr;


         /* Set internal socket send buffer to 64k,
         * this should fix the speed problems some users have reported
         */
         gs_ipSetBufSize( hSocket, 0xFFFFFF, 0xFFFFFF );

         hb_socketConnect( hSocket, &remote, timeout );       
      }
   }
   return hSocket;
}

HB_SOCKET_T gs_ipServer( int iPort, const char * szAddress, int iListen )
{
   HB_SOCKET_T hSocket;
   int iOpt;
   struct sockaddr_in remote;

   HB_SOCKET_ZERO_ERROR();

   /* Creates comm socket */
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
   hSocket = socket( AF_INET, SOCK_STREAM, 0 );
#else
   hSocket = socket( PF_INET, SOCK_STREAM, 0 );
#endif

   if( hSocket == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR();
   }

   /* Reusable socket; under unix, do not wait it is unused */
   iOpt = 1;
   setsockopt( hSocket, SOL_SOCKET, SO_REUSEADDR, (const char *) &iOpt, sizeof( iOpt ));

   remote.sin_family = AF_INET;
   remote.sin_port = htons( iPort );

   remote.sin_addr.s_addr = szAddress ? inet_addr( szAddress ) : INADDR_ANY;

   if( bind( hSocket, (struct sockaddr *) &remote, sizeof(remote) ) )
   {
      HB_SOCKET_SET_ERROR();
      HB_IP_CLOSE( hSocket );
      return -1;
   }
   else if( listen( hSocket, iListen ) )
   {
      HB_SOCKET_SET_ERROR();
      HB_IP_CLOSE( hSocket );
      return -1;
   }
   else
      return hSocket;
}

HB_SOCKET_T gs_ipAccept( HB_SOCKET_T hSocket, int timeout, char * szAddr, long int * lPort )
{
#if !defined(EAGAIN)
#define EAGAIN -1
#endif
   HB_SOCKET_T incoming = 0;
   int iError = EAGAIN;
   struct sockaddr_in si_remote;
#if defined(_XOPEN_SOURCE_EXTENDED)
   socklen_t Len;
#elif defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
   int Len;
#else
   unsigned int Len;
#endif

   Len = sizeof( struct sockaddr_in );

   /*
   * Accept can (and should) be asynchronously stopped by closing the
   * accepting socket. this will make the wait to terminate, and the
   * calling program will be notivfied through the status of the
   * returned socket.
   */

   HB_SOCKET_ZERO_ERROR();

   /* Connection incoming */
   while( iError == EAGAIN )
   {
      if( gs_ipDataReady( hSocket,timeout ) )
      {
         /* On error (e.g. async connection closed) , com will be -1 and
            errno == 22 (invalid argument ) */
         incoming = accept( hSocket, (struct sockaddr *) &si_remote, &Len );
         if( incoming == ( HB_SOCKET_T ) -1 )
         {
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
            iError = WSAGetLastError();
#else
            iError = errno;
#endif
         }
         else
            iError = 0;
      }
      /* Timeout expired */
      else
         iError = -1;
   }

   if( iError == -1 )
   {
      HB_SOCKET_SET_ERROR2( -1, "Timeout" );
      return -1;
   }
   else if( incoming == ( HB_SOCKET_T ) -1 )
   {
      if( !iError )
         HB_SOCKET_SET_ERROR2( -2, "Unknown" );
      else
         HB_SOCKET_SET_ERROR1( iError );
      return -1;
   }
   else
   {
      // char * ptr = inet_ntoa( si_remote.sin_addr );
      ULONG u = ntohl( si_remote.sin_addr.s_addr );
      int iOpt;

      // memcpy( szAddr, ptr, strlen(ptr) );
      // szAddr[strlen(ptr)] = '\0';
      sprintf( szAddr, "%hd.%hd.%hd.%hd", HB_UHBYTE( u ), HB_ULBYTE( u ), 
            HB_HIBYTE( u ), HB_LOBYTE( u ) );

      *lPort = ntohs( si_remote.sin_port );
      iOpt = 1;
      setsockopt( incoming, SOL_SOCKET, SO_KEEPALIVE, (const char *) &iOpt , sizeof( iOpt ));

      /* Set internal socket send buffer to 64k,
      * this should fix the speed problems some users have reported
      */
      gs_ipSetBufSize( incoming, 0xFFFFFF, 0xFFFFFF );
      return incoming;
   }
}

int gs_ipclose( HB_SOCKET_T hSocket )
{
   int iRet;

   #if defined( HB_OS_WIN_32 ) || defined( HB_OS_WIN )
      shutdown( hSocket, SD_BOTH );
   #elif defined(HB_OS_OS2)
      shutdown( hSocket, SO_RCV_SHUTDOWN + SO_SND_SHUTDOWN );
   #elif !defined(__WATCOMC__)
      shutdown( hSocket, SHUT_RDWR );
   #endif

   iRet = HB_IP_CLOSE( hSocket );

   #ifdef HB_OS_LINUX
      kill( 0, HB_IP_LINUX_INTERRUPT );
   #endif
   return iRet;
}

void hb_getLocalIP( HB_SOCKET_T hSocket, char * szIP )
{
   struct sockaddr_in localAddr;
   socklen_t sin_size = sizeof( localAddr );
   int nRetVal = getsockname( hSocket, (struct sockaddr *)&localAddr, &sin_size );

   if(nRetVal == -1)
   {
      *szIP = '\0';
   }
   else
      strcpy( szIP, inet_ntoa(localAddr.sin_addr) );
}


int gs_ip_rfd_isset( HB_SOCKET_T hSocket )
{
   return FD_ISSET( hSocket,&active_fds );
}

void gs_ip_rfd_set( HB_SOCKET_T hSocket )
{
   HB_SOCKET_ZERO_ERROR();

   FD_SET( hSocket, &rd_fds );
   rd_maxfd = ( rd_maxfd > hSocket )? rd_maxfd : hSocket;
}

void gs_ip_rfd_clr( HB_SOCKET_T hSocket )
{
   if( !hSocket )
      return;
   FD_CLR( hSocket, &rd_fds );
}

void gs_ip_rfd_zero( void )
{
   FD_ZERO( &rd_fds );
   rd_maxfd = 0;
}

int gs_ip_rfd_select( int iTimeOut )
{
   struct timeval tv = {0,0};

   if( iTimeOut != -1 )
   {
      tv.tv_sec = iTimeOut / 1000;
      tv.tv_usec = (iTimeOut % 1000) * 1000;
   }
   active_fds = rd_fds;

   iTimeOut = select( rd_maxfd + 1, &active_fds, NULL, NULL, &tv);

   return iTimeOut;
}

