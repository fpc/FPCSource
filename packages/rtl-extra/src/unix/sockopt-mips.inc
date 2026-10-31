{
  Socket level and socket options for Linux on MIPS (arch/mips/include/uapi/asm/socket.h).
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
         SO_NO_CHECK =   11;
         SO_PRIORITY =   12;
         SO_LINGER   =   $0080;
         SO_BSDCOMPAT=   14;
         SO_REUSEPORT=   $0200;
         SO_PASSCRED=    17;
         SO_PEERCRED=    18;
         SO_RCVLOWAT=    $1004;
         SO_SNDLOWAT=    $1003;
         SO_RCVTIMEO=    $1006;
         SO_SNDTIMEO=    $1005;

         SO_SECURITY_AUTHENTICATION      =   22;
         SO_SECURITY_ENCRYPTION_TRANSPORT=   23;
         SO_SECURITY_ENCRYPTION_NETWORK  =   24;

         SO_BINDTODEVICE=   25;

         SO_ATTACH_FILTER=  26;
         SO_DETACH_FILTER=  27;
         SO_PEERNAME     =  28;
         SO_TIMESTAMP    =  29;
         SCM_TIMESTAMP   = SO_TIMESTAMP;
         SO_ACCEPTCONN   = $1009;
