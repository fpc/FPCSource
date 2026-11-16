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
         SO_SNDBUFFORCE           = $100a;
         SO_RCVBUFFORCE           = $100b;
         SO_PROTOCOL              = $1028;
         SO_DOMAIN                = $1029;
         SO_PEERSEC               = $001e;
         SO_PASSSEC               = $001f;
         SO_TIMESTAMPNS           = $0021;
         SCM_TIMESTAMPNS          = SO_TIMESTAMPNS;
         SO_MARK                  = $0022;
         SO_TIMESTAMPING          = $0023;
         SCM_TIMESTAMPING         = SO_TIMESTAMPING;
         SO_RXQ_OVFL              = $0024;
         SO_WIFI_STATUS           = $0025;
         SO_PEEK_OFF              = $0026;
         SO_BUSY_POLL             = $0030;
         SO_MAX_PACING_RATE       = $0031;
         SO_ATTACH_BPF            = $0034;
         SO_ATTACH_REUSEPORT_EBPF = $0036;
         SO_COOKIE                = $003b;
         SO_PEERGROUPS            = $003d;
         SO_ZEROCOPY              = $003e;
         SO_TXTIME                = $003f;
         SO_BINDTOIFINDEX         = $0041;
         SO_DETACH_REUSEPORT_BPF  = $0047;
         SO_PREFER_BUSY_POLL      = $0048;
         SO_NETNS_COOKIE          = $0050;
         SO_BUF_LOCK              = $0051;
         SO_RESERVE_MEM           = $0052;
         SO_RCVMARK               = $0054;
         SO_PASSPIDFD             = $0055;
         SO_PEERPIDFD             = $0056;
