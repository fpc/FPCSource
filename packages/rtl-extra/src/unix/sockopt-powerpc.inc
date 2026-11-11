{
  Socket level and socket options for Linux on PowerPC and PowerPC64 (arch/powerpc/include/uapi/asm/socket.h).
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
         SOL_SOCKET  =   1;
         SO_DEBUG    =   1;
         SO_REUSEADDR=   2;
         SO_TYPE     =   3;
         SO_ERROR    =   4;
         SO_DONTROUTE=   5;
         SO_BROADCAST=   6;
         SO_SNDBUF   =   7;
         SO_RCVBUF   =   8;
         SO_KEEPALIVE=   9;
         SO_OOBINLINE=   10;
         SO_NO_CHECK =   11;
         SO_PRIORITY =   12;
         SO_LINGER   =   13;
         SO_BSDCOMPAT=   14;
         SO_REUSEPORT=   15;
         SO_RCVLOWAT=    16;
         SO_SNDLOWAT=    17;
         SO_RCVTIMEO=    18;
         SO_SNDTIMEO=    19;
         SO_PASSCRED=    20;
         SO_PEERCRED=    21;

         SO_SECURITY_AUTHENTICATION      =   22;
         SO_SECURITY_ENCRYPTION_TRANSPORT=   23;
         SO_SECURITY_ENCRYPTION_NETWORK  =   24;

         SO_BINDTODEVICE=   25;

         SO_ATTACH_FILTER=  26;
         SO_DETACH_FILTER=  27;
         SO_PEERNAME     =  28;
         SO_TIMESTAMP    = 29;
         SCM_TIMESTAMP   = SO_TIMESTAMP;
         SO_ACCEPTCONN   = 30;
