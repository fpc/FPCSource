{
  Socket level and socket options for Linux on SPARC and SPARC64 (arch/sparc/include/uapi/asm/socket.h).
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
         SOL_SOCKET  =   $FFFF;
         SO_DEBUG    =   $0001;
         SO_REUSEADDR=   $0004;
         SO_TYPE     =   $1008;
         SO_ERROR    =   $1007;
         SO_DONTROUTE=   $0010;
         SO_BROADCAST=   $0020;
         SO_SNDBUF   =   $1001;
         SO_RCVBUF   =   $1002;
         SO_KEEPALIVE=   $0008;
         SO_OOBINLINE=   $0100;
         SO_NO_CHECK =   $000B;
         SO_PRIORITY =   $000C;
         SO_LINGER   =   $0080;
         SO_BSDCOMPAT=   $0400;
         SO_REUSEPORT=   $0200;
         SO_PASSCRED=    $0002;
         SO_PEERCRED=    $0040;
         SO_RCVLOWAT=    $0800;
         SO_SNDLOWAT=    $1000;
         SO_RCVTIMEO=    $2000;
         SO_SNDTIMEO=    $4000;

         SO_SECURITY_AUTHENTICATION      =   $5001;
         SO_SECURITY_ENCRYPTION_TRANSPORT=   $5002;
         SO_SECURITY_ENCRYPTION_NETWORK  =   $5004;

         SO_BINDTODEVICE=   $000D;

         SO_ATTACH_FILTER=  $001A;
         SO_DETACH_FILTER=  $001B;
         SO_PEERNAME     =  $001C;
         SO_TIMESTAMP    =  $001D;
         SCM_TIMESTAMP   = SO_TIMESTAMP;
         SO_ACCEPTCONN   = $8000;
