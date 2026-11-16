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
         SO_SNDBUFFORCE           = 32;
         SO_RCVBUFFORCE           = 33;
         SO_PROTOCOL              = 38;
         SO_DOMAIN                = 39;
         SO_PEERSEC               = 31;
         SO_PASSSEC               = 34;
         SO_TIMESTAMPNS           = 35;
         SCM_TIMESTAMPNS          = SO_TIMESTAMPNS;
         SO_MARK                  = 36;
         SO_TIMESTAMPING          = 37;
         SCM_TIMESTAMPING         = SO_TIMESTAMPING;
         SO_RXQ_OVFL              = 40;
         SO_WIFI_STATUS           = 41;
         SO_PEEK_OFF              = 42;
         SO_BUSY_POLL             = 46;
         SO_MAX_PACING_RATE       = 47;
         SO_ATTACH_BPF            = 50;
         SO_ATTACH_REUSEPORT_EBPF = 52;
         SO_COOKIE                = 57;
         SO_PEERGROUPS            = 59;
         SO_ZEROCOPY              = 60;
         SO_TXTIME                = 61;
         SO_BINDTOIFINDEX         = 62;
         SO_DETACH_REUSEPORT_BPF  = 68;
         SO_PREFER_BUSY_POLL      = 69;
         SO_NETNS_COOKIE          = 71;
         SO_BUF_LOCK              = 72;
         SO_RESERVE_MEM           = 73;
         SO_RCVMARK               = 75;
         SO_PASSPIDFD             = 76;
         SO_PEERPIDFD             = 77;
