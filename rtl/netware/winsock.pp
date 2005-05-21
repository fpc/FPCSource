{
    This file is part of the Free Pascal run time library.
    This unit contains the declarations for the WinSock2
    Socket Library for Netware and Win32

    Copyright (c) 1999-2003 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ************************************************************************
 For NetWare 4.11 you have to install winsock-support (i.e. nw4wsock.exe)
 NetWare >= 5.0 contains winsock support by default
 ************************************************************************}

{$PACKRECORDS 1}
{$R-}

unit winsock;

{$mode objfpc}

  interface

    const
       {
         Default maximium number of sockets.
         this does not mean that the underlying netware
         Sockets implementation has to support that many!
       }
       FD_SETSIZE = 64;

    type
       tOS_INT  = LongInt;
       tOS_UINT = DWord;
       ptOS_INT = ^tOS_INT;
       ptOS_UINT = ^tOS_UINT;

       u_char = char;
       u_short = word;
       u_int = tOS_UINT;
       u_long = dword;
       pu_long = ^u_long;
       pu_short = ^u_short;
       plongint = ^longint;
       TSocket = longint;
       BOOL    = boolean;
       LPINT   = ^integer;
       LPDWORD = ^dword;
       {$ifdef netware}
       OVERLAPPED = record
          Internal    : DWORD;
          InternalHigh: DWORD;
          Offset      : DWORD;
          OffsetHigh  : DWORD;
          hEvent      : THandle;
       end;
       LPOVERLAPPED = ^OVERLAPPED;
       TOVERLAPPED  = OVERLAPPED;
       POVERLAPPED  = ^OVERLAPPED;
       {$endif}
       PHandle = ^THandle;
       TWSAOVERLAPPED= OVERLAPPED;
       PWSAOVERLAPPED= ^OVERLAPPED;
       TWSAEVENT = THandle;
       PWSAEVENT = ^THandle;

       { there is already a procedure called FD_SET, so this
         record was renamed (FK) }
       fdset = record
          fd_count : u_int;
          fd_array : array[0..(FD_SETSIZE)-1] of TSocket;
       end;

       TFDSet = fdset;
       PFDSet = ^fdset;

       timeval = record
          tv_sec : longint;
          tv_usec : longint;
       end;

       TTimeVal = timeval;
       PTimeVal = ^TTimeVal;

    const
       IOCPARM_MASK = $7f;
       IOC_VOID = $20000000;
       IOC_OUT = $40000000;
       IOC_IN = $80000000;
       IOC_INOUT = IOC_IN or IOC_OUT;
       FIONREAD = IOC_OUT or
         ((4 and IOCPARM_MASK) shl 16) or
         (102 shl 8) or 127;
       FIONBIO = IOC_IN or
         ((4 and IOCPARM_MASK) shl 16) or
         (102 shl 8) or 126;
       FIOASYNC     = IOC_IN or
         ((4 and IOCPARM_MASK) shl 16) or
         (102 shl 8) or 125;
       {
         Structures returned by network data base library, taken from the
         BSD file netdb.h.  All addresses are supplied in host order, and
         returned in network order (suitable for use in system calls).
         Slight modifications for differences between Linux and winsock.h
      }
    type
       hostent = record
          { official name of host  }
          h_name: pchar;
          { alias list  }
          h_aliases: ^pchar;
          { host address type  }
          h_addrtype: SmallInt;
          { length of address  }
          h_length: SmallInt;
          { list of addresses  }
          case byte of
             0: (h_addr_list: ^pchar);
             1: (h_addr: ^pchar)
       end;
       THostEnt = hostent;
       PHostEnt = ^THostEnt;

       {
         Assumption here is that a network number
         fits in an unsigned long -- someday that won't be true!
       }
       netent = record
          n_name : ^char;        // official name of net
          n_aliases : ^pchar;    // alias list
          n_addrtype : SmallInt; // net address type
          n_pad1 : SmallInt;     // ensure right packaging
          n_net : u_long;        // network #
       end;
       TNetEnt = netent;
       PNetEnt = ^TNetEnt;

       servent = record
          { official service name  }
          s_name : ^char;
          { alias list  }
          s_aliases : ^pchar;
          { port #  }
          s_port : SmallInt;
          n_pad1 : SmallInt;    { ensure right packaging }
          { protocol to use  }
          s_proto : ^char;
       end;
       TServEnt = servent;
       PServEnt = ^TServEnt;

       protoent = record
          { official protocol name  }
          p_name : ^char;
          { alias list  }
          p_aliases : ^pchar;
          { protocol #  }
          p_proto : SmallInt;
          p_pad1 : SmallInt;    { ensure packaging }
       end;
       TProtoEnt = protoent;
       PProtoEnt = ^TProtoEnt;

    const
       {
         Standard well-known IP protocols.
         For some reason there are differences between Linx and winsock.h
       }
       IPPROTO_IP = 0;
       IPPROTO_ICMP = 1;
       IPPROTO_IGMP = 2;
       IPPROTO_GGP = 3;
       IPPROTO_TCP = 6;
       IPPORT_ECHO = 7;
       IPPORT_DISCARD = 9;
       IPPORT_SYSTAT = 11;
       IPPROTO_PUP = 12;
       IPPORT_DAYTIME = 13;
       IPPORT_NETSTAT = 15;
       IPPROTO_UDP = 17;
       IPPROTO_IDP = 22;
       IPPROTO_ND = 77;
       IPPROTO_RAW = 255;
       IPPROTO_MAX = 256;
       IPPORT_FTP = 21;
       IPPORT_TELNET = 23;
       IPPORT_SMTP = 25;
       IPPORT_TIMESERVER = 37;
       IPPORT_NAMESERVER = 42;
       IPPORT_WHOIS = 43;
       IPPORT_MTP = 57;
       IPPORT_TFTP = 69;
       IPPORT_RJE = 77;
       IPPORT_FINGER = 79;
       IPPORT_TTYLINK = 87;
       IPPORT_SUPDUP = 95;
       IPPORT_EXECSERVER = 512;
       IPPORT_LOGINSERVER = 513;
       IPPORT_CMDSERVER = 514;
       IPPORT_EFSSERVER = 520;
       IPPORT_BIFFUDP = 512;
       IPPORT_WHOSERVER = 513;
       IPPORT_ROUTESERVER = 520;
       IPPORT_RESERVED = 1024;

    const
       IMPLINK_IP = 155;
       IMPLINK_LOWEXPER = 156;
       IMPLINK_HIGHEXPER = 158;

    type
       SunB = packed record
          s_b1,s_b2,s_b3,s_b4 : u_char;
       end;

       SunW = packed record
         s_w1,s_w2 : u_short;
       end;

       in_addr = record
          case integer of
             0 : (S_un_b : SunB);
             1 : (S_un_w : SunW);
             2 : (S_addr : u_long);
       end;
       TInAddr = in_addr;
       PInAddr = ^TInAddr;

       sockaddr_in = record
          case integer of
             0 : ( (* equals to sockaddr_in, size is 16 byte *)
                  sin_family : SmallInt;                      (* 2 byte *)
                  sin_port : u_short;                         (* 2 byte *)
                  sin_addr : TInAddr;                         (* 4 byte *)
                  sin_zero : array[0..7] of char;             (* 8 byte *)
                 );
             1 : ( (* equals to sockaddr, size is 16 byte *)
                  sa_family : SmallInt;                       (* 2 byte *)
                  sa_data : array[0..13] of char;             (* 14 byte *)
                 );

         end;

       TSockAddrIn = sockaddr_in;
       PSockAddrIn = ^TSockAddrIn;
       TSockAddr = sockaddr_in;
       PSockAddr = ^TSockAddr;

    const
       INADDR_ANY = $00000000;
       INADDR_LOOPBACK = $7F000001;
       INADDR_BROADCAST = $FFFFFFFF;

       IN_CLASSA_NET = $ff000000;
       IN_CLASSA_NSHIFT = 24;
       IN_CLASSA_HOST = $00ffffff;
       IN_CLASSA_MAX = 128;
       IN_CLASSB_NET = $ffff0000;
       IN_CLASSB_NSHIFT = 16;
       IN_CLASSB_HOST = $0000ffff;
       IN_CLASSB_MAX = 65536;
       IN_CLASSC_NET = $ffffff00;
       IN_CLASSC_NSHIFT = 8;
       IN_CLASSC_HOST = $000000ff;
       INADDR_NONE = $ffffffff;

       WSADESCRIPTION_LEN = 256;
       WSASYS_STATUS_LEN = 128;

    type
       WSADATA = record
          wVersion : WORD;              { 2 byte, ofs 0 }
          wHighVersion : WORD;          { 2 byte, ofs 2 }
          szDescription : array[0..(WSADESCRIPTION_LEN+1)-1] of char; { 257 byte, ofs 4 }
          szSystemStatus : array[0..(WSASYS_STATUS_LEN+1)-1] of char; { 129 byte, ofs 261 }
          iMaxSockets : word;           { 2 byte, ofs 390 }
          iMaxUdpDg : word;             { 2 byte, ofs 392 }
          pad1 : SmallInt;              { 2 byte, ofs 394 } { ensure right packaging }
          lpVendorInfo : pchar;         { 4 byte, ofs 396 }
       end;                             { total size 400 }
       TWSAData = WSADATA;
       PWSAData = TWSAData;

    const
       IP_OPTIONS = 1;
       IP_MULTICAST_IF = 2;
       IP_MULTICAST_TTL = 3;
       IP_MULTICAST_LOOP = 4;
       IP_ADD_MEMBERSHIP = 5;
       IP_DROP_MEMBERSHIP = 6;
       IP_DEFAULT_MULTICAST_TTL = 1;
       IP_DEFAULT_MULTICAST_LOOP = 1;
       IP_MAX_MEMBERSHIPS = 20;

    type
       ip_mreq = record
            imr_multiaddr : in_addr;
            imr_interface : in_addr;
         end;

    {
       Definitions related to sockets: types, address families, options,
       taken from the BSD file sys/socket.h.
    }
    const
       INVALID_SOCKET = longint(not(1));
       SOCKET_ERROR = -1;
       SOCK_STREAM = 1;
       SOCK_DGRAM = 2;
       SOCK_RAW = 3;
       SOCK_RDM = 4;
       SOCK_SEQPACKET = 5;

      { For setsockoptions(2)  }
       SO_DEBUG = $0001;
       SO_ACCEPTCONN = $0002;
       SO_REUSEADDR = $0004;
       SO_KEEPALIVE = $0008;
       SO_DONTROUTE = $0010;
       SO_BROADCAST = $0020;
       SO_USELOOPBACK = $0040;
       SO_LINGER = $0080;
       SO_OOBINLINE = $0100;
       {
         Additional options.
       }
       { send buffer size  }
       SO_SNDBUF = $1001;
       { receive buffer size  }
       SO_RCVBUF = $1002;
       { send low-water mark  }
       SO_SNDLOWAT = $1003;
       { receive low-water mark  }
       SO_RCVLOWAT = $1004;
       { send timeout  }
       SO_SNDTIMEO = $1005;
       { receive timeout  }
       SO_RCVTIMEO = $1006;
       { get error status and clear  }
       SO_ERROR = $1007;
       { get socket type  }
       SO_TYPE = $1008;

       { WinSock 2 extension -- new option }
       SO_GROUP_ID       = $2001;      { ID of a socket group }
       SO_GROUP_PRIORITY = $2002;      { the relative priority within a group }
       SO_MAX_MSG_SIZE   = $2003;      { maximum message size }
       SO_PROTOCOL_INFOA = $2004;      { WSAPROTOCOL_INFOA structure }
       SO_PROTOCOL_INFOW = $2005;      { WSAPROTOCOL_INFOW structure }
       SO_PROTOCOL_INFO  = SO_PROTOCOL_INFOW;
       PVD_CONFIG        = $3001;      { configuration info for service provider }


       {
         Options for connect and disconnect data and options.  Used only by
         non-TCP/IP transports such as DECNet, OSI TP4, etc.
       }
       SO_CONNDATA = $7000;
       SO_CONNOPT = $7001;
       SO_DISCDATA = $7002;
       SO_DISCOPT = $7003;
       SO_CONNDATALEN = $7004;
       SO_CONNOPTLEN = $7005;
       SO_DISCDATALEN = $7006;
       SO_DISCOPTLEN = $7007;

       {
         Option for opening sockets for synchronous access.
       }
       SO_OPENTYPE = $7008;
       SO_SYNCHRONOUS_ALERT = $10;
       SO_SYNCHRONOUS_NONALERT = $20;

       {
         Other NT-specific options.
       }
       SO_MAXDG = $7009;
       SO_MAXPATHDG = $700A;
       SO_UPDATE_ACCEPT_CONTEXT = $700B;
       SO_CONNECT_TIME = $700C;

       {
         TCP options.
       }
       TCP_NODELAY = $0001;
       TCP_BSDURGENT = $7000;

       { Address families. }
       { unspecified  }
       AF_UNSPEC = 0;
       {
        * Although  AF_UNSPEC  is  defined for backwards compatibility, using
        * AF_UNSPEC for the "af" parameter when creating a socket is STRONGLY
        * DISCOURAGED.    The  interpretation  of  the  "protocol"  parameter
        * depends  on the actual address family chosen.  As environments grow
        * to  include  more  and  more  address families that use overlapping
        * protocol  values  there  is  more  and  more  chance of choosing an
        * undesired address family when AF_UNSPEC is used. }

       { local to host (pipes, portals)  }
       AF_UNIX = 1;
       { internetwork: UDP, TCP, etc.  }
       AF_INET = 2;
       { arpanet imp addresses  }
       AF_IMPLINK = 3;
       { pup protocols: e.g. BSP  }
       AF_PUP = 4;
       { mit CHAOS protocols  }
       AF_CHAOS = 5;
       { IPX and SPX  }
       AF_IPX = 6;
       { XEROX NS protocols  }
       AF_NS = 6;
       { ISO protocols  }
       AF_ISO = 7;
       { OSI is ISO  }
       AF_OSI = AF_ISO;
       { european computer manufacturers  }
       AF_ECMA = 8;
       { datakit protocols  }
       AF_DATAKIT = 9;
       { CCITT protocols, X.25 etc  }
       AF_CCITT = 10;
       { IBM SNA  }
       AF_SNA = 11;
       { DECnet  }
       AF_DECnet = 12;
       { Direct data link interface  }
       AF_DLI = 13;
       { LAT  }
       AF_LAT = 14;
       { NSC Hyperchannel  }
       AF_HYLINK = 15;
       { AppleTalk  }
       AF_APPLETALK = 16;
       { NetBios-style addresses  }
       AF_NETBIOS = 17;
       { VoiceView }
       AF_VOICEVIEW = 18;
       { FireFox }
       AF_FIREFOX = 19;
       { Somebody is using this! }
       AF_UNKNOWN1 = 20;
       { Banyan }
       AF_BAN = 21;

       AF_ATM      = 22;
       AF_INET6    = 23;
       AF_CLUSTER  = 24;
       AF_12844    = 25;
       AF_IRDA     = 26;

       AF_MAX      = 27;

    type
       {
         Structure used by kernel to pass protocol
         information in raw sockets.
       }
       sockproto = record
          sp_family : u_short;
          sp_protocol : u_short;
       end;
       TSockProto = sockproto;
       PSockProto = ^TSockProto;

    const
       {
         Protocol families, same as address families for now.
       }
       PF_UNSPEC = AF_UNSPEC;
       PF_UNIX = AF_UNIX;
       PF_INET = AF_INET;
       PF_IMPLINK = AF_IMPLINK;
       PF_PUP = AF_PUP;
       PF_CHAOS = AF_CHAOS;
       PF_NS = AF_NS;
       PF_IPX = AF_IPX;
       PF_ISO = AF_ISO;
       PF_OSI = AF_OSI;
       PF_ECMA = AF_ECMA;
       PF_DATAKIT = AF_DATAKIT;
       PF_CCITT = AF_CCITT;
       PF_SNA = AF_SNA;
       PF_DECnet = AF_DECnet;
       PF_DLI = AF_DLI;
       PF_LAT = AF_LAT;
       PF_HYLINK = AF_HYLINK;
       PF_APPLETALK = AF_APPLETALK;
       PF_VOICEVIEW = AF_VOICEVIEW;
       PF_FIREFOX = AF_FIREFOX;
       PF_UNKNOWN1 = AF_UNKNOWN1;
       PF_BAN = AF_BAN;
       PF_ATM = AF_ATM;
       PF_INET6 = AF_INET6;
       PF_MAX = AF_MAX;

    type
       {
         Structure used for manipulating linger option.
       }
       linger = record
          l_onoff : u_short;
          l_linger : u_short;
       end;
       TLinger = linger;
       PLinger = ^TLinger;

    const
       {
         Level number for (get/set)sockopt() to apply to socket itself.
       }
       { options for socket level  }
       SOL_SOCKET = $ffff;
       {
         Maximum queue length specifiable by listen.
       }
       SOMAXCONN = $7fffffff;
       { process out-of-band data  }
       MSG_OOB = $1;
       { peek at incoming message  }
       MSG_PEEK = $2;
       { send without using routing tables  }
       MSG_DONTROUTE = $4;
       MSG_MAXIOVLEN = 16;
       { partial send or recv for message xport  }
       MSG_PARTIAL = $8000;

       {
         Define constant based on rfc883, used by gethostbyxxxx() calls.
       }
       MAXGETHOSTSTRUCT = 1024;
       MAXHOSTNAMELEN = MAXGETHOSTSTRUCT;

       { Winsock2 extension -- new flags for WSASend, WSASendTo, WSARecv
         and WSARecvFrom }
       MSG_INTERRUPT   = $10;  { send/recv in the interrupt context }

       { Define flags to be used with the WSAAsyncSelect() call. }
       FD_READ = $01;
       FD_WRITE = $02;
       FD_OOB = $04;
       FD_ACCEPT = $08;
       FD_CONNECT = $10;
       FD_CLOSE = $20;
       FD_QOS   = $40;
       FD_GROUP_QOS = $80;
       FD_ROUTING_INTERFACE_CHANGE = $100;
       FD_ADDRESS_LIST_CHANGE = $200;
       FD_MAX_EVENTS = 10;
       FD_ALL_EVENTS = $4ff;

       { All Windows Sockets error constants are biased by WSABASEERR from
         the "normal" }
       WSABASEERR = 10000;

       { Windows Sockets definitions of regular Microsoft C error constants }
       WSAEINTR = WSABASEERR + 4;
       WSAEBADF = WSABASEERR + 9;
       WSAEACCES = WSABASEERR + 13;
       WSAEFAULT = WSABASEERR + 14;
       WSAEINVAL = WSABASEERR + 22;
       WSAEMFILE = WSABASEERR + 24;

       { Windows Sockets definitions of regular Berkeley error constants }
       WSAEWOULDBLOCK = WSABASEERR + 35;
       WSAEINPROGRESS = WSABASEERR + 36;
       WSAEALREADY = WSABASEERR + 37;
       WSAENOTSOCK = WSABASEERR + 38;
       WSAEDESTADDRREQ = WSABASEERR + 39;
       WSAEMSGSIZE = WSABASEERR + 40;
       WSAEPROTOTYPE = WSABASEERR + 41;
       WSAENOPROTOOPT = WSABASEERR + 42;
       WSAEPROTONOSUPPORT = WSABASEERR + 43;
       WSAESOCKTNOSUPPORT = WSABASEERR + 44;
       WSAEOPNOTSUPP = WSABASEERR + 45;
       WSAEPFNOSUPPORT = WSABASEERR + 46;
       WSAEAFNOSUPPORT = WSABASEERR + 47;
       WSAEADDRINUSE = WSABASEERR + 48;
       WSAEADDRNOTAVAIL = WSABASEERR + 49;
       WSAENETDOWN = WSABASEERR + 50;
       WSAENETUNREACH = WSABASEERR + 51;
       WSAENETRESET = WSABASEERR + 52;
       WSAECONNABORTED = WSABASEERR + 53;
       WSAECONNRESET = WSABASEERR + 54;
       WSAENOBUFS = WSABASEERR + 55;
       WSAEISCONN = WSABASEERR + 56;
       WSAENOTCONN = WSABASEERR + 57;
       WSAESHUTDOWN = WSABASEERR + 58;
       WSAETOOMANYREFS = WSABASEERR + 59;
       WSAETIMEDOUT = WSABASEERR + 60;
       WSAECONNREFUSED = WSABASEERR + 61;
       WSAELOOP = WSABASEERR + 62;
       WSAENAMETOOLONG = WSABASEERR + 63;
       WSAEHOSTDOWN = WSABASEERR + 64;
       WSAEHOSTUNREACH = WSABASEERR + 65;
       WSAENOTEMPTY = WSABASEERR + 66;
       WSAEPROCLIM = WSABASEERR + 67;
       WSAEUSERS = WSABASEERR + 68;
       WSAEDQUOT = WSABASEERR + 69;
       WSAESTALE = WSABASEERR + 70;
       WSAEREMOTE = WSABASEERR + 71;

       { Extended Windows Sockets error constant definitions }
       WSASYSNOTREADY = WSABASEERR + 91;
       WSAVERNOTSUPPORTED = WSABASEERR + 92;
       WSANOTINITIALISED = WSABASEERR + 93;
       WSAEDISCON = WSABASEERR + 101;
       WSAENOMORE              = WSABASEERR+102;
       WSAECANCELLED           = WSABASEERR+103;
       WSAEINVALIDPROCTABLE    = WSABASEERR+104;
       WSAEINVALIDPROVIDER     = WSABASEERR+105;
       WSAEPROVIDERFAILEDINIT  = WSABASEERR+106;
       WSASYSCALLFAILURE       = WSABASEERR+107;
       WSASERVICE_NOT_FOUND    = WSABASEERR+108;
       WSATYPE_NOT_FOUND       = WSABASEERR+109;
       WSA_E_NO_MORE           = WSABASEERR+110;
       WSA_E_CANCELLED         = WSABASEERR+111;
       WSAEREFUSED             = WSABASEERR+112;

       {
         Error return codes from gethostbyname() and gethostbyaddr()
         (when using the resolver). Note that these errors are
         retrieved via WSAGetLastError() and must therefore follow
         the rules for avoiding clashes with error numbers from
         specific implementations or language run-time systems.
         For this reason the codes are based at WSABASEERR+1001.
         Note also that [WSA]NO_ADDRESS is defined only for
         compatibility purposes.
        }
       WSAHOST_NOT_FOUND = WSABASEERR + 1001;
       HOST_NOT_FOUND = WSAHOST_NOT_FOUND;
       { Non-Authoritative: Host not found, or SERVERFAIL  }
       WSATRY_AGAIN = WSABASEERR + 1002;
       TRY_AGAIN = WSATRY_AGAIN;

       { Non recoverable errors, FORMERR, REFUSED, NOTIMP  }
       WSANO_RECOVERY = WSABASEERR + 1003;
       NO_RECOVERY = WSANO_RECOVERY;

       { Valid name, no data record of requested type  }
       WSANO_DATA = WSABASEERR + 1004;
       NO_DATA = WSANO_DATA;

       { no address, look for MX record  }
       WSANO_ADDRESS = WSANO_DATA;
       NO_ADDRESS = WSANO_ADDRESS;

       { Define QOS related error return codes }
       WSA_QOS_RECEIVERS               = WSABASEERR + 1005;
       { at least one Reserve has arrived }
       WSA_QOS_SENDERS                 = WSABASEERR + 1006;
       { at least one Path has arrived }
       WSA_QOS_NO_SENDERS              = WSABASEERR + 1007;
       { there are no senders }
       WSA_QOS_NO_RECEIVERS            = WSABASEERR + 1008;
       { there are no receivers }
       WSA_QOS_REQUEST_CONFIRMED       = WSABASEERR + 1009;
       { Reserve has been confirmed }
       WSA_QOS_ADMISSION_FAILURE       = WSABASEERR + 1010;

       WSA_QOS_POLICY_FAILURE          = WSABASEERR + 1011;
       { rejected for administrative reasons - bad credentials }
       WSA_QOS_BAD_STYLE               = WSABASEERR + 1012;
       { unknown or conflicting style }
       WSA_QOS_BAD_OBJECT              = WSABASEERR + 1013;
       {* problem with some part of the filterspec or providerspecific
        * buffer in general }
       WSA_QOS_TRAFFIC_CTRL_ERROR      = WSABASEERR + 1014;
       { problem with some part of the flowspec }
       WSA_QOS_GENERIC_ERROR           = WSABASEERR + 1015;
       { general error }

    const
       {
         Windows Sockets errors redefined as regular Berkeley error constants.
       }
       EWOULDBLOCK = WSAEWOULDBLOCK;
       EINPROGRESS = WSAEINPROGRESS;
       EALREADY = WSAEALREADY;
       ENOTSOCK = WSAENOTSOCK;
       EDESTADDRREQ = WSAEDESTADDRREQ;
       EMSGSIZE = WSAEMSGSIZE;
       EPROTOTYPE = WSAEPROTOTYPE;
       ENOPROTOOPT = WSAENOPROTOOPT;
       EPROTONOSUPPORT = WSAEPROTONOSUPPORT;
       ESOCKTNOSUPPORT = WSAESOCKTNOSUPPORT;
       EOPNOTSUPP = WSAEOPNOTSUPP;
       EPFNOSUPPORT = WSAEPFNOSUPPORT;
       EAFNOSUPPORT = WSAEAFNOSUPPORT;
       EADDRINUSE = WSAEADDRINUSE;
       EADDRNOTAVAIL = WSAEADDRNOTAVAIL;
       ENETDOWN = WSAENETDOWN;
       ENETUNREACH = WSAENETUNREACH;
       ENETRESET = WSAENETRESET;
       ECONNABORTED = WSAECONNABORTED;
       ECONNRESET = WSAECONNRESET;
       ENOBUFS = WSAENOBUFS;
       EISCONN = WSAEISCONN;
       ENOTCONN = WSAENOTCONN;
       ESHUTDOWN = WSAESHUTDOWN;
       ETOOMANYREFS = WSAETOOMANYREFS;
       ETIMEDOUT = WSAETIMEDOUT;
       ECONNREFUSED = WSAECONNREFUSED;
       ELOOP = WSAELOOP;
       ENAMETOOLONG = WSAENAMETOOLONG;
       EHOSTDOWN = WSAEHOSTDOWN;
       EHOSTUNREACH = WSAEHOSTUNREACH;
       ENOTEMPTY = WSAENOTEMPTY;
       EPROCLIM = WSAEPROCLIM;
       EUSERS = WSAEUSERS;
       EDQUOT = WSAEDQUOT;
       ESTALE = WSAESTALE;
       EREMOTE = WSAEREMOTE;

       TF_DISCONNECT = $01;
       TF_REUSE_SOCKET = $02;
       TF_WRITE_BEHIND = $04;

       {
         Options for use with [gs]etsockopt at the IP level.
       }
       IP_TTL = 7;
       IP_TOS = 8;
       IP_DONTFRAGMENT = 9;

    type
       _TRANSMIT_FILE_BUFFERS = record
          Head : Pointer;
          HeadLength : dword;
          Tail : Pointer;
          TailLength : dword;
       end;
       TRANSMIT_FILE_BUFFERS = _TRANSMIT_FILE_BUFFERS;
       TTransmitFileBuffers = _TRANSMIT_FILE_BUFFERS;
       PTransmitFileBuffers = ^TTransmitFileBuffers;

    {  WinSock 2 extension -- WSABUF and QOS struct, include qos.h
       to pull in FLOWSPEC and related definitions  }
    { the length of the buffer  }
    (* far ignored *)
    { the pointer to the buffer  }
    (* far ignored *)

    TWSABUF = record
            len : u_long;
            buf : ^char;
         end;
    _WSABUF = TWSABUF;
    LPWSABUF = ^TWSABUF;
    PWSABUF  = ^TWSABUF;

    {$i qos.inc }

    { the flow spec for data sending  }
    { the flow spec for data receiving  }
    { additional provider specific stuff  }

    type

       TQualityOfService = record
            SendingFlowspec   : TFLOWSPEC;
            ReceivingFlowspec : TFLOWSPEC;
            ProviderSpecific  : TWSABUF;
         end;
       TQOS = TQualityOfService;
       PQOS = ^TQOS;

    { WinSock 2 extension -- manifest constants for return values of the condition function }

    const
       CF_ACCEPT  = $0000;
       CF_REJECT  = $0001;
       CF_DEFER   = $0002;

       { WinSock 2 extension -- manifest constants for shutdown() }
       SD_RECEIVE = $00;
       SD_SEND    = $01;
       SD_BOTH    = $02;

    { WinSock 2 extension -- data type and manifest constants for socket groups }

    type
       TGROUP = dword;
       PGROUP = ^TGROUP;

    const
       SG_UNCONSTRAINED_GROUP = $01;
       SG_CONSTRAINED_GROUP   = $02;

    { WinSock 2 extension -- data type for WSAEnumNetworkEvents() }

    type

       TWSANETWORKEVENTS = record
         lNetworkEvents : longint;
         iErrorCode     : array[0..(FD_MAX_EVENTS)-1] of longint;
       end;
       LPWSANETWORKEVENTS = ^TWSANETWORKEVENTS;
       PWSANETWORKEVENTS  = ^TWSANETWORKEVENTS;

    {  WinSock 2 extension -- WSAPROTOCOL_INFO structure and associated
       manifest constants  }

    type

       TGUID = record
            Data1 : dword;
            Data2 : word;
            Data3 : word;
            Data4 : array[0..7] of byte;
         end;
       PGUID  = ^TGUID;
       LPGUID = PGUID;

    const
       MAX_PROTOCOL_CHAIN = 7;
       BASE_PROTOCOL = 1;
       LAYERED_PROTOCOL = 0;
    { the length of the chain,      }
    { length = 0 means layered protocol,  }
    { length = 1 means base protocol,  }
    { length > 1 means protocol chain  }
    { a list of dwCatalogEntryIds  }

    type

       TWSAPROTOCOLCHAIN = record
            ChainLen : longint;
            ChainEntries : array[0..(MAX_PROTOCOL_CHAIN)-1] of DWORD;
         end;
       LPWSAPROTOCOLCHAIN = ^TWSAPROTOCOLCHAIN;
       PWSAPROTOCOLCHAIN  = ^TWSAPROTOCOLCHAIN;

    const
       WSAPROTOCOL_LEN = 255;
    (* far ignored *)

    type

       TWSAPROTOCOL_INFOA = record
            dwServiceFlags1 : DWORD;
            dwServiceFlags2 : DWORD;
            dwServiceFlags3 : DWORD;
            dwServiceFlags4 : DWORD;
            dwProviderFlags : DWORD;
            ProviderId      : TGUID;
            dwCatalogEntryId: DWORD;
            ProtocolChain   : TWSAPROTOCOLCHAIN;
            iVersion        : longint;
            iAddressFamily  : longint;
            iMaxSockAddr    : longint;
            iMinSockAddr    : longint;
            iSocketType     : longint;
            iProtocol          : longint;
            iProtocolMaxOffset : longint;
            iNetworkByteOrder  : longint;
            iSecurityScheme    : longint;
            dwMessageSize      : DWORD;
            dwProviderReserved : DWORD;
            szProtocol         : array[0..(WSAPROTOCOL_LEN + 1)-1] of CHAR;
         end;
       LPWSAPROTOCOL_INFOA = ^TWSAPROTOCOL_INFOA;
       PWSAPROTOCOL_INFOA  = ^TWSAPROTOCOL_INFOA;


       TWSAPROTOCOL_INFOW = record
            dwServiceFlags1 : DWORD;
            dwServiceFlags2 : DWORD;
            dwServiceFlags3 : DWORD;
            dwServiceFlags4 : DWORD;
            dwProviderFlags : DWORD;
            ProviderId      : TGUID;
            dwCatalogEntryId: DWORD;
            ProtocolChain   : TWSAPROTOCOLCHAIN;
            iVersion        : longint;
            iAddressFamily  : longint;
            iMaxSockAddr    : longint;
            iMinSockAddr    : longint;
            iSocketType     : longint;
            iProtocol       : longint;
            iProtocolMaxOffset : longint;
            iNetworkByteOrder  : longint;
            iSecurityScheme    : longint;
            dwMessageSize      : DWORD;
            dwProviderReserved : DWORD;
            szProtocol : array[0..(WSAPROTOCOL_LEN + 1)-1] of WCHAR;
         end;
       LPWSAPROTOCOL_INFOW = ^TWSAPROTOCOL_INFOW;
       PWSAPROTOCOL_INFOW  = ^TWSAPROTOCOL_INFOW;

{$ifdef UNICODE}
    type
       TWSAPROTOCOL_INFO = TWSAPROTOCOL_INFOW;
       LPWSAPROTOCOL_INFO = LPWSAPROTOCOL_INFOW;
       PWSAPROTOCOL_INFO  = PWSAPROTOCOL_INFOW;
{$else}
    type
       TWSAPROTOCOL_INFO = TWSAPROTOCOL_INFOA;
       LPWSAPROTOCOL_INFO = LPWSAPROTOCOL_INFOA;
       PWSAPROTOCOL_INFO  = PWSAPROTOCOL_INFOA;
{$endif}

    { Flag bit definitions for dwProviderFlags  }

    const
       PFL_MULTIPLE_PROTO_ENTRIES = $00000001;
       PFL_RECOMMENDED_PROTO_ENTRY = $00000002;
       PFL_HIDDEN = $00000004;
       PFL_MATCHES_PROTOCOL_ZERO = $00000008;
    { Flag bit definitions for dwServiceFlags1  }
       XP1_CONNECTIONLESS = $00000001;
       XP1_GUARANTEED_DELIVERY = $00000002;
       XP1_GUARANTEED_ORDER = $00000004;
       XP1_MESSAGE_ORIENTED = $00000008;
       XP1_PSEUDO_STREAM = $00000010;
       XP1_GRACEFUL_CLOSE = $00000020;
       XP1_EXPEDITED_DATA = $00000040;
       XP1_CONNECT_DATA = $00000080;
       XP1_DISCONNECT_DATA = $00000100;
       XP1_SUPPORT_BROADCAST = $00000200;
       XP1_SUPPORT_MULTIPOINT = $00000400;
       XP1_MULTIPOINT_CONTROL_PLANE = $00000800;
       XP1_MULTIPOINT_DATA_PLANE = $00001000;
       XP1_QOS_SUPPORTED = $00002000;
       XP1_INTERRUPT = $00004000;
       XP1_UNI_SEND = $00008000;
       XP1_UNI_RECV = $00010000;
       XP1_IFS_HANDLES = $00020000;
       XP1_PARTIAL_MESSAGE = $00040000;
       BIGENDIAN = $0000;
       LITTLEENDIAN = $0001;
       SECURITY_PROTOCOL_NONE = $0000;

    { WinSock 2 extension -- manifest constants for WSAJoinLeaf() }
       JL_SENDER_ONLY = $01;
       JL_RECEIVER_ONLY = $02;
       JL_BOTH = $04;

    { WinSock 2 extension -- manifest constants for WSASocket() }
       WSA_FLAG_OVERLAPPED = $01;
       WSA_FLAG_MULTIPOINT_C_ROOT = $02;
       WSA_FLAG_MULTIPOINT_C_LEAF = $04;
       WSA_FLAG_MULTIPOINT_D_ROOT = $08;
       WSA_FLAG_MULTIPOINT_D_LEAF = $10;


    { WinSock 2 extension -- manifest constants for WSAIoctl()  }
       IOC_UNIX = $00000000;
       IOC_WS2 = $08000000;
       IOC_PROTOCOL = $10000000;
       IOC_VENDOR = $18000000;

    { WinSock 2 extension -- manifest constants for SIO_TRANSLATE_HANDLE ioctl }

    const
       TH_NETDEV = $00000001;
       TH_TAPI   = $00000002;

    {  Microsoft Windows Extended data types required for the functions to
       convert   back  and  forth  between  binary  and  string  forms  of
       addresses.  }

    type
       //PSOCKADDR = sockaddr;
       LPSOCKADDR = PSockAddr;

    {  Manifest constants and type definitions related to name resolution and
       registration (RNR) API  }
    type
       TBLOB = record
         cbSize : u_long;
         pBlobData:ARRAY[0..0] OF POINTER;  {???}
       end;
       PBLOB = ^TBLOB;

    { Service Install Flags }

    const
       SERVICE_MULTIPLE = $00000001;

    { & Name Spaces  }
       NS_ALL = 0;
       NS_SAP = 1;
       NS_NDS = 2;
       NS_PEER_BROWSE = 3;
       NS_TCPIP_LOCAL = 10;
       NS_TCPIP_HOSTS = 11;
       NS_DNS = 12;
       NS_NETBT = 13;
       NS_WINS = 14;
       NS_NBP = 20;
       NS_MS = 30;
       NS_STDA = 31;
       NS_NTDS = 32;
       NS_X500 = 40;
       NS_NIS = 41;
       NS_NISPLUS = 42;
       NS_WRQ = 50;

    {  Resolution flags for WSAGetAddressByName().
       Note these are also used by the 1.1 API GetAddressByName, so
       leave them around. }

       RES_UNUSED_1 = $00000001;
       RES_FLUSH_CACHE = $00000002;

       RES_SERVICE = $00000004;

    { RES_SERVICE  }
    { Well known value names for Service Types }

    const
       SERVICE_TYPE_VALUE_IPXPORTA = 'IpxSocket';
(* error #define SERVICE_TYPE_VALUE_IPXPORTW     L"IpxSocket" *)
       SERVICE_TYPE_VALUE_SAPIDA = 'SapId';
(* error #define SERVICE_TYPE_VALUE_SAPIDW       L"SapId" *)
       SERVICE_TYPE_VALUE_TCPPORTA = 'TcpPort';
(* error #define SERVICE_TYPE_VALUE_TCPPORTW     L"TcpPort" *)
       SERVICE_TYPE_VALUE_UDPPORTA = 'UdpPort';
(* error #define SERVICE_TYPE_VALUE_UDPPORTW     L"UdpPort" *)
       SERVICE_TYPE_VALUE_OBJECTIDA = 'ObjectId';
(* error #define SERVICE_TYPE_VALUE_OBJECTIDW    L"ObjectId" *)

{$ifdef UNICODE}
    const
       SERVICE_TYPE_VALUE_SAPID = SERVICE_TYPE_VALUE_SAPIDW;
       SERVICE_TYPE_VALUE_TCPPORT = SERVICE_TYPE_VALUE_TCPPORTW;
       SERVICE_TYPE_VALUE_UDPPORT = SERVICE_TYPE_VALUE_UDPPORTW;
       SERVICE_TYPE_VALUE_OBJECTID = SERVICE_TYPE_VALUE_OBJECTIDW;
{$else}
    { not UNICODE  }
    const
       SERVICE_TYPE_VALUE_SAPID = SERVICE_TYPE_VALUE_SAPIDA;
       SERVICE_TYPE_VALUE_TCPPORT = SERVICE_TYPE_VALUE_TCPPORTA;
       SERVICE_TYPE_VALUE_UDPPORT = SERVICE_TYPE_VALUE_UDPPORTA;
       SERVICE_TYPE_VALUE_OBJECTID = SERVICE_TYPE_VALUE_OBJECTIDA;
{$endif}

    { SockAddr Information }
    type

       TSOCKET_ADDRESS = record
            lpSockaddr      : PSockAddr;
            iSockaddrLength : tOS_INT;
         end;
       PSOCKET_ADDRESS = ^TSOCKET_ADDRESS;
       LPSOCKET_ADDRESS = ^TSOCKET_ADDRESS;

    { CSAddr Information }

       TCSADDR_INFO = record
            LocalAddr   : TSOCKET_ADDRESS;
            RemoteAddr  : TSOCKET_ADDRESS;
            iSocketType : tOS_INT;
            iProtocol   : tOS_INT;
         end;
       PCSADDR_INFO  = ^TCSADDR_INFO;
       LPCSADDR_INFO = ^TCSADDR_INFO;

    { Address list returned via SIO_ADDRESS_LIST_QUERY }

    type

       TSOCKET_ADDRESS_LIST = record
            iAddressCount : tOS_INT;
            Address       : array[0..0] of TSOCKET_ADDRESS;
         end;
       LPSOCKET_ADDRESS_LIST = ^TSOCKET_ADDRESS_LIST;
       PSOCKET_ADDRESS_LIST = ^TSOCKET_ADDRESS_LIST;

    { Address Family/Protocol Tuples }

       TAFPROTOCOLS = record
            iAddressFamily : tOS_INT;
            iProtocol      : tOS_INT;
         end;
       PAFPROTOCOLS = ^TAFPROTOCOLS;
       LPAFPROTOCOLS = ^TAFPROTOCOLS;

    { Client Query API Typedefs }
    { The comparators }

       TWSAEcomparator = (COMP_EQUAL := 0,COMP_NOTLESS);
       PWSAECOMPARATOR = ^TWSAEcomparator;
       LPWSAECOMPARATOR = ^TWSAEcomparator;

       TWSAVersion = record
            dwVersion : DWORD;
            ecHow     : TWSAECOMPARATOR;
         end;
       PWSAVERSION = ^TWSAVersion;
       LPWSAVERSION = ^TWSAVersion;

       TWSAQuerySetA = record
            dwSize : DWORD;
            lpszServiceInstanceName : PChar;
            lpServiceClassId : LPGUID;
            lpVersion : LPWSAVERSION;
            lpszComment : PChar;
            dwNameSpace : DWORD;
            lpNSProviderId : LPGUID;
            lpszContext : PChar;
            dwNumberOfProtocols : DWORD;
            lpafpProtocols : LPAFPROTOCOLS;
            lpszQueryString : PChar;
            dwNumberOfCsAddrs : DWORD;
            lpcsaBuffer : LPCSADDR_INFO;
            dwOutputFlags : DWORD;
            lpBlob : PBLOB;
         end;
       PWSAQUERYSETA = ^TWSAQuerySetA;
       LPWSAQUERYSETA = ^TWSAQuerySetA;

       TWSAQuerySetW = record
            dwSize : DWORD;
            lpszServiceInstanceName : PWideChar;
            lpServiceClassId : LPGUID;
            lpVersion : LPWSAVERSION;
            lpszComment : PWideChar;
            dwNameSpace : DWORD;
            lpNSProviderId : LPGUID;
            lpszContext : PWideChar;
            dwNumberOfProtocols : DWORD;
            lpafpProtocols : LPAFPROTOCOLS;
            lpszQueryString : PWideChar;
            dwNumberOfCsAddrs : DWORD;
            lpcsaBuffer : LPCSADDR_INFO;
            dwOutputFlags : DWORD;
            lpBlob : PBLOB;
         end;
       PWSAQUERYSETW = ^TWSAQuerySetW;
       LPWSAQUERYSETW = ^TWSAQuerySetW;
{$ifdef UNICODE}

    type
       TWSAQUERYSET = TWSAQUERYSETW;
       PWSAQUERYSET = PWSAQUERYSETW;
       LPWSAQUERYSET = LPWSAQUERYSETW;
{$else}
    type
       TWSAQUERYSET = TWSAQUERYSETA;
       PWSAQUERYSET = PWSAQUERYSETA;
       LPWSAQUERYSET = LPWSAQUERYSETA;
{$endif}

    const
       LUP_DEEP = $0001;
       LUP_CONTAINERS = $0002;
       LUP_NOCONTAINERS = $0004;
       LUP_NEAREST = $0008;
       LUP_RETURN_NAME = $0010;
       LUP_RETURN_TYPE = $0020;
       LUP_RETURN_VERSION = $0040;
       LUP_RETURN_COMMENT = $0080;
       LUP_RETURN_ADDR = $0100;
       LUP_RETURN_BLOB = $0200;
       LUP_RETURN_ALIASES = $0400;
       LUP_RETURN_QUERY_STRING = $0800;
       LUP_RETURN_ALL = $0FF0;
       LUP_RES_SERVICE = $8000;
       LUP_FLUSHCACHE = $1000;
       LUP_FLUSHPREVIOUS = $2000;

    { Return flags }

       RESULT_IS_ALIAS = $0001;

    { Service Address Registration and Deregistration Data Types. }

    type

       TWSAESETSERVICEOP =
          (RNRSERVICE_REGISTER := 0,RNRSERVICE_DEREGISTER, RNRSERVICE_DELETE);
       PWSAESETSERVICEOP = ^TWSAESETSERVICEOP;
       LPWSAESETSERVICEOP = ^TWSAESETSERVICEOP;

    { Service Installation/Removal Data Types. }

       TWSANSClassInfoA = record
            lpszName    : PChar;
            dwNameSpace : DWORD;
            dwValueType : DWORD;
            dwValueSize : DWORD;
            lpValue     : Pointer;
         end;
       PWSANSCLASSINFOA = ^TWSANSClassInfoA;
       LPWSANSCLASSINFOA = ^TWSANSClassInfoA;

       TWSANSClassInfoW = record
            lpszName    : PWideChar;
            dwNameSpace : DWORD;
            dwValueType : DWORD;
            dwValueSize : DWORD;
            lpValue     : Pointer;
         end;
       PWSANSCLASSINFOW = ^TWSANSClassInfoW;
       LPWSANSCLASSINFOW = ^TWSANSClassInfoW;
{$ifdef UNICODE}
    type
       TWSANSCLASSINFO = TWSANSCLASSINFOW;
       PWSANSCLASSINFO = PWSANSCLASSINFOW;
       LPWSANSCLASSINFO = LPWSANSCLASSINFOW;
{$else}
    type
       TWSANSCLASSINFO = TWSANSCLASSINFOA;
       PWSANSCLASSINFO = PWSANSCLASSINFOA;
       LPWSANSCLASSINFO = LPWSANSCLASSINFOA;
{$endif}

    type
       TWSAServiceClassInfoA = record
            lpServiceClassId     : LPGUID;
            lpszServiceClassName : PChar;
            dwCount              : DWORD;
            lpClassInfos         : LPWSANSCLASSINFOA;
         end;
       PWSASERVICECLASSINFOA = ^TWSAServiceClassInfoA;
       LPWSASERVICECLASSINFOA = ^TWSAServiceClassInfoA;

       TWSAServiceClassInfoW = record
            lpServiceClassId : LPGUID;
            lpszServiceClassName : PWideChar;
            dwCount : DWORD;
            lpClassInfos : LPWSANSCLASSINFOW;
         end;
       PWSASERVICECLASSINFOW = ^TWSAServiceClassInfoW;
       LPWSASERVICECLASSINFOW = ^TWSAServiceClassInfoW;

{$ifdef UNICODE}
    type
       TWSASERVICECLASSINFO = TWSASERVICECLASSINFOW;
       PWSASERVICECLASSINFO = PWSASERVICECLASSINFOW;
       LPWSASERVICECLASSINFO = LPWSASERVICECLASSINFOW;
{$else}
    type
       TWSASERVICECLASSINFO = TWSASERVICECLASSINFOA;
       PWSASERVICECLASSINFO = PWSASERVICECLASSINFOA;
       LPWSASERVICECLASSINFO = LPWSASERVICECLASSINFOA;
{$endif}

    type
       TWSANAMESPACE_INFOA = record
            NSProviderId  : TGUID;
            dwNameSpace   : DWORD;
            fActive       : BOOL;
            dwVersion     : DWORD;
            lpszIdentifier: PChar;
         end;
       PWSANAMESPACE_INFOA = ^TWSANAMESPACE_INFOA;
       LPWSANAMESPACE_INFOA = ^TWSANAMESPACE_INFOA;

       TWSANAMESPACE_INFOW = record
            NSProviderId  : TGUID;
            dwNameSpace   : DWORD;
            fActive       : BOOL;
            dwVersion     : DWORD;
            lpszIdentifier: PWideChar;
         end;
       PWSANAMESPACE_INFOW = ^TWSANAMESPACE_INFOW;
       LPWSANAMESPACE_INFOW = ^TWSANAMESPACE_INFOW;
{$ifdef UNICODE}
    type
       TWSANAMESPACE_INFO = TWSANAMESPACE_INFOW;
       PWSANAMESPACE_INFO = PWSANAMESPACE_INFOW;
       LPWSANAMESPACE_INFO = LPWSANAMESPACE_INFOW;
{$else}
    type
       TWSANAMESPACE_INFO = TWSANAMESPACE_INFOA;
       PWSANAMESPACE_INFO = PWSANAMESPACE_INFOA;
       LPWSANAMESPACE_INFO = LPWSANAMESPACE_INFOA;
{$endif}

    { WinSock 2 extensions -- data types for the condition function in
      WSAAccept() and overlapped I/O completion routine. }

    type
      TWSAOVERLAPPED_COMPLETION_ROUTINE =
        function  (dwError,cbTransferred : longint;
                   lpOverlapped          : PWSAOVERLAPPED;
                   dwFlags               : longint) : longint;
            {$ifdef netware} cdecl; {$else} stdcall; {$endif}

      TCONDITIONPROC =
        function (lpCallerId, lpCallerData : PWSABUF;
                  lpSQOS, lpGQOS           : PQOS;
                  lpCalleeId, lpCalleeData : PWSABUF;
                  g                        : PGROUP;
                  dwCallbackData           : dword) : longint;
            {$ifdef netware} cdecl; {$else} stdcall; {$endif}

{--------------------------------------------------------------------}
{netware extensions from ws2nlm.h}
{$ifdef netware}
  { New Address Types }

  const
     AF_INET_ACP = 25;
     AF_IPX_ACP = 26;
     AF_ACP = 27;

  { NetWare SSL Ioctls }

  const
     SECURITY_PROTOCOL_SSL = 1;
     SECURITY_PROTOCOL_TLS = 2;

  { There are three interesting authentication types }
  { CLIENT -    Client initiates a SSL connection.  }
  { SERVER - Listener set up to listen for incoming SSL conns, (Server sends it's cert during auth) }
  { MUTUAL is SSL Server requesting client authentication, (Server asks for client cert inaddition to sending it's cert) }
  { optval is a DWORD defined as }

  const
     MUTUAL = $00000002;

  { System flags not defined in NetWare }

  const
     INFINITE = $FFFFFFFF;
     WAIT_OBJECT_0 = 0;

  {Various Types that may not be defined }
  { }
  { }
  { Predefined Value Types. }
  { }
  { No value type }

  const
     REG_NONE = 0;
     REG_SZ = 1;
     REG_EXPAND_SZ = 2;
     REG_BINARY = 3;
     REG_DWORD = 4;
     REG_DWORD_LITTLE_ENDIAN = 4;
     REG_DWORD_BIG_ENDIAN = 5;
     REG_LINK = 6;
     REG_MULTI_SZ = 7;
     REG_RESOURCE_LIST = 8;
     REG_FULL_RESOURCE_DESCRIPTOR = 9;
     REG_RESOURCE_REQUIREMENTS_LIST = 10;

     ERROR_INVALID_HANDLE = 6;
     ERROR_NOT_ENOUGH_MEMORY = 8;
     ERROR_INVALID_PARAMETER = 87;
     ERROR_IO_PENDING = 997;
     ERROR_OPERATION_ABORTED = 995;
     ERROR_IO_INCOMPLETE = 996;

  { connect timeout  }

  const
     SO_CONNTIMEO = $1009;
  { NetWare Fast Accept and Recv option structures }
  { Fast Recv also has a cleanup routine returned. }

  type

     LPFASTACCEPT_COMPLETION_ROUTINE = longint;
     LPFASTRECV_COMPLETION_ROUTINE = longint;

     TFASTACCEPT_OP = record
          acceptHandler : LPFASTACCEPT_COMPLETION_ROUTINE;
          arg : pointer;
       end;
     LPFAST_ACCEPT_OPT = ^TFASTACCEPT_OP;
     PFAST_ACCEPT_OPT = ^TFASTACCEPT_OP;

     TFASTRECV_OP = record
          recvHandler : LPFASTRECV_COMPLETION_ROUTINE;
          Arg : pointer;
       end;
     LPFAST_RECV_OPT = ^TFASTRECV_OP;
     PFAST_RECV_OPT = ^TFASTRECV_OP;

  { Winsock 2 applications that want to use SSL need to define WS_SSL }
  type
     time_t = dword;

  { Secure Sockets Layer - needed until Winsock SDK supplies ssl header file. }
  { Taken from Winsock 2 protocol Annex for SSL Security Protocol. Unsupported }
  { options are labeled "not supported". }
  { This value is the SSL protocol tag and WSAIoctl dwIoControlCode
     "T" value.  }

  const
     _SO_SSL = (2 shl 27) or ($73 shl 16);
  {
     These values are used to form the WSAIoctl dwIoControlCode
     "Code" value.
   }
     _SO_SSL_FLAGS = $01;
     _SO_SSL_CIPHERS = $02;
     _SO_SSL_SERVER = $04;
  { not supported }
     _SO_SSL_AUTH_CERT_HOOK = $08;
  { not supported }
     _SO_SSL_RSA_ENCRYPT_HOOK = $10;
  { not supported }
     _SO_SSL_RSA_DECRYPT_HOOK = $20;
  { _SO_SSL_CLIENT has been changed from 0x03 to 0x80 to avoid bitwise  }
  { conflicts with _SO_SSL_CIPHERS _SO_SSL_FLAGS.  }
     _SO_SSL_CLIENT = $80;
  { Actual SSL Ioctl commands }
     SO_SSL_GET_FLAGS = (IOC_IN or _SO_SSL) or _SO_SSL_FLAGS;
     SO_SSL_SET_FLAGS = (IOC_OUT or _SO_SSL) or _SO_SSL_FLAGS;
     SO_SSL_GET_CIPHERS = (IOC_IN or _SO_SSL) or _SO_SSL_CIPHERS;
  {not supported }
     SO_SSL_SET_CIPHERS = (IOC_OUT or _SO_SSL) or _SO_SSL_CIPHERS;
     SO_SSL_GET_CLIENT = (IOC_IN or _SO_SSL) or _SO_SSL_CLIENT;
     SO_SSL_SET_CLIENT = (IOC_OUT or _SO_SSL) or _SO_SSL_CLIENT;
     SO_SSL_GET_SERVER = (IOC_IN or _SO_SSL) or _SO_SSL_SERVER;
     SO_SSL_SET_SERVER = (IOC_OUT or _SO_SSL) or _SO_SSL_SERVER;
  {not supported }
     SO_SSL_GET_AUTH_CERT_HOOK = (IOC_IN or _SO_SSL) or _SO_SSL_AUTH_CERT_HOOK;
  {not supported }
     SO_SSL_SET_AUTH_CERT_HOOK = (IOC_OUT or _SO_SSL) or _SO_SSL_AUTH_CERT_HOOK;
  {not supported }
     SO_SSL_GET_RSA_ENCRYPT_HOOK = (IOC_IN or _SO_SSL) or _SO_SSL_RSA_ENCRYPT_HOOK;
  {not supported }
     SO_SSL_SET_RSA_ENCRYPT_HOOK = (IOC_OUT or _SO_SSL) or _SO_SSL_RSA_ENCRYPT_HOOK;
  {not supported }
     SO_SSL_GET_RSA_DECRYPT_HOOK = (IOC_IN or _SO_SSL) or _SO_SSL_RSA_DECRYPT_HOOK;
  {not supported }
     SO_SSL_SET_RSA_DECRYPT_HOOK = (IOC_OUT or _SO_SSL) or _SO_SSL_RSA_DECRYPT_HOOK;
     SO_SSL_ENABLE = $001;
     SO_SSL_SERVER = $002;
     SO_SSL_AUTH_CLIENT = $004;
  {not supported }
     SO_SSL_ACCEPT_WEAK = $008;

  type

     Tsslcipheropts = record
          n : longint;
          specs : array[0..2] of char;
       end;

     Tsslclientopts = record
          cert : PChar;
          certlen : longint;
          sidtimeout : time_t;
          sidentries : longint;
          siddir : PChar;
       end;

     Tsslserveropts = record
          cert : PChar;
          certlen : longint;
          sidtimeout : time_t;
          sidentries : longint;
          siddir : PChar;
       end;

  {not suppported }
     {Tsslauthcertopts = record
          _type : longint;
          func : function (arg:pointer; cert:Pchar; len:longint):longint;cdecl;
          arg : pointer;
       end;}

  {not supported  }

  const
     SSL_ACK_OK = 1;
  {not supported  }
     SSL_ACH_WEAK_OK = 2;
  {not supported  }
     SSL_ACH_LONG_DATA = 3;
  {not supported  }
     SSL_ACH_BAD_DATA = 4;
  {not supported  }
     SSL_ACH_BAD_SIG = 5;
  {not supported  }
     SSL_ACH_CERT_EXPIRED = 6;
  {not suppported }

  type
     sslrsaencrypthook = record
          func : function (arg:pointer; blockType:longint; dest:Pchar; destlen:Plongint; src:Pchar;
                       srclen:longint):longint; cdecl;
          arg : pointer;
       end;

  {not supported  }

  const
     SSL_REH_OK = 0;
  {not supported  }
     SSL_REH_BAD_TYPE = 1;
  {not supported  }
     SSL_REH_BAD_LEN = 2;
  {not suppported }

  type
     Tsslrsadecrypthook = record
          func : function (arg:pointer; blockType:longint; dest:Pchar; destlen:Plongint; src:Pchar;
                       srclen:longint):longint; cdecl;
          arg : pointer;
       end;
     Psslrsadecrypthook = ^Tsslrsadecrypthook;

  {not supported  }

  const
     SSL_RDH_OK = 0;
  {not supported  }
     SSL_RDH_BAD_TYPE = 1;
  {not supported  }
     SSL_RDH_BAD_LEN = 2;
  { TLS options }
  { Secure Sockets Layer - needed until Winsock SDK supplies ssl header file. }
  { Taken from Winsock 2 protocol Annex for SSL Security Protocol. Unsupported }
  { options are labeled "not supported". }
  {
     This value is the SSL protocol tag and WSAIoctl dwIoControlCode
     "T" value. This value is unique to distinguish a TLS Ioctl from an SSL
     Ioctl due to different structure definitions.
   }
     _SO_TLS = (2 shl 27) or ($74 shl 16);
  {
     These values are used to form the WSAIoctl dwIoControlCode
     "Code" value.
   }
     _SO_TLS_FLAGS = $01;
     _SO_TLS_CIPHERS = $02;
     _SO_TLS_SERVER = $04;
  { not supported }
     _SO_TLS_AUTH_CERT_HOOK = $08;
  { not supported }
     _SO_TLS_RSA_ENCRYPT_HOOK = $10;
  { not supported }
     _SO_TLS_RSA_DECRYPT_HOOK = $20;
     _SO_TLS_CERT = $40;
  { _SO_TLS_CLIENT has been changed from 0x03 to 0x80 to avoid bitwise  }
  { conflicts with _SO_TLS_CIPHERS _SO_TLS_FLAGS.  }
     _SO_TLS_CLIENT = $80;
  {
     Actual TLS Ioctl commands
   }
     SO_TLS_GET_FLAGS = (IOC_IN or _SO_TLS) or _SO_TLS_FLAGS;
     SO_TLS_SET_FLAGS = (IOC_OUT or _SO_TLS) or _SO_TLS_FLAGS;
     SO_TLS_GET_CIPHERS = (IOC_IN or _SO_TLS) or _SO_TLS_CIPHERS;
  {not supported }
     SO_TLS_SET_CIPHERS = (IOC_OUT or _SO_TLS) or _SO_TLS_CIPHERS;
     SO_TLS_GET_CLIENT = (IOC_IN or _SO_TLS) or _SO_TLS_CLIENT;
     SO_TLS_SET_CLIENT = (IOC_OUT or _SO_TLS) or _SO_TLS_CLIENT;
     SO_TLS_GET_SERVER = (IOC_IN or _SO_TLS) or _SO_TLS_SERVER;
     SO_TLS_SET_SERVER = (IOC_OUT or _SO_TLS) or _SO_TLS_SERVER;
     SO_TLS_GET_CERT = (IOC_IN or _SO_TLS) or _SO_TLS_CERT;
  {not supported }
     SO_TLS_GET_AUTH_CERT_HOOK = (IOC_IN or _SO_TLS) or _SO_TLS_AUTH_CERT_HOOK;
  {not supported }
     SO_TLS_SET_AUTH_CERT_HOOK = (IOC_OUT or _SO_TLS) or _SO_TLS_AUTH_CERT_HOOK;
  {not supported }
     SO_TLS_GET_RSA_ENCRYPT_HOOK = (IOC_IN or _SO_TLS) or _SO_TLS_RSA_ENCRYPT_HOOK;
  {not supported }
     SO_TLS_SET_RSA_ENCRYPT_HOOK = (IOC_OUT or _SO_TLS) or _SO_TLS_RSA_ENCRYPT_HOOK;
  {not supported }
     SO_TLS_GET_RSA_DECRYPT_HOOK = (IOC_IN or _SO_TLS) or _SO_TLS_RSA_DECRYPT_HOOK;
  {not supported }
     SO_TLS_SET_RSA_DECRYPT_HOOK = (IOC_OUT or _SO_TLS) or _SO_TLS_RSA_DECRYPT_HOOK;
     SO_TLS_ENABLE = $0001;
     SO_TLS_SERVER = $0002;
     SO_TLS_AUTH_CLIENT = $0004;
  {not supported }
     SO_TLS_ACCEPT_WEAK = $0008;
     SO_TLS_MAP_DISABLE = $0010;
     SO_TLS_MAP_IDENTITY = $0020;
     SO_TLS_BLIND_ACCEPT = $0040;
     SO_TLS_INTERACTIVE_ACCEPT = $0080;

  type
     Ttlscipheropts = record
          n : longint;
          specs : array[0..2] of char;
       end;
     Ptlscipheropts = ^Ttlscipheropts;

     Ttlsclientopts = record
          wallet : PWideChar;  // ^unicode;
          walletlen : longint;
          sidtimeout : time_t;
          sidentries : longint;
          siddir : PChar;  // ^char;
          options : pointer;
       end;
     Ptlsclientopts = ^Ttlsclientopts;

     Ttlsserveropts = record
          wallet : PWideChar;  // ^unicode;
          walletlen : longint;
          sidtimeout : time_t;
          sidentries : longint;
          siddir : PChar;  // ^char;
          options : pointer;
       end;
     Ptlsserveropts = ^Ttlsserveropts;

  {wallet content provider e.g. PFX, KMO, DER. }
  {alias for private key in wallet to be used }
  {  not used for anything but pfx wallet provider }
  {number of elements in the array }
  {array of trusted root names }
  {number of elements in the array }
  {reserved to set ciphers }
  {reserved for CRL }
  {reserved for CRL len. }
     Tnwtlsopts = record
          walletProvider : PWideChar;  //^unicode;
          keysList : PPWideChar;      // ^^unicode;
          numElementsInKeyList : longint;
          TrustedRootList : PPWideChar;  // ^^unicode;
          numElementsInTRList : longint;
          reservedforfutureuse : pointer;
          reservedforfutureCRL : pointer;
          reservedforfutureCRLLen : longint;
          reserved1 : pointer;
          reserved2 : pointer;
          reserved3 : pointer;
       end;
     Pnwtlsopts=^Tnwtlsopts;

  {not suppported }
     {tlsauthcertopts = record
          _type : longint;
          func : function (arg:pointer; cert:Pchar; len:longint):longint; cdecl;
          arg : pointer;
       end;}

  {not supported  }

  const
     TLS_ACK_OK = 1;
  {not supported  }
     TLS_ACH_WEAK_OK = 2;
  {not supported  }
     TLS_ACH_LONG_DATA = 3;
  {not supported  }
     TLS_ACH_BAD_DATA = 4;
  {not supported  }
     TLS_ACH_BAD_SIG = 5;
  {not supported  }
     TLS_ACH_CERT_EXPIRED = 6;
  {not suppported }

  type
     Ttlsrsaencrypthook = record
          func : function (arg:pointer; blockType:longint; dest:Pchar; destlen:Plongint; src:Pchar;
                       srclen:longint):longint; cdecl;
          arg : pointer;
       end;
     Ptlsrsaencrypthook=^Ttlsrsaencrypthook;

  {not supported  }

  const
     TLS_REH_OK = 0;
  {not supported  }
     TLS_REH_BAD_TYPE = 1;
  {not supported  }
     TLS_REH_BAD_LEN = 2;
  {not suppported }

  type
     Ttlsrsadecrypthook = record
          func : function (arg:pointer; blockType:longint; dest:Pchar; destlen:Plongint; src:Pchar;
                       srclen:longint):longint; cdecl;
          arg : pointer;
       end;
     Ptlsrsadecrypthook=^Ttlsrsadecrypthook;
  {not supported  }

  const
     TLS_RDH_OK = 0;
  {not supported  }
     TLS_RDH_BAD_TYPE = 1;
  {not supported  }
     TLS_RDH_BAD_LEN = 2;

  type
     Ttlscert = record
          cert : PChar;
          certlen : longint;
       end;
     Ptlscert = ^Ttlscert;

  const SIO_RAWCALLBACKS       = $1ADD0002;
        SIO_WORKTODOCALLBACKS  = $1ADD0004;
        SIO_FASTACCEPTCALLBACK = $1ADD0008;
        SIO_FASTRECVCALLBACK   = $1ADD000C;
        SIO_RCVFULLMSG         = $1ADD0010;
        SIO_SSL_CRYPTFILE      = $1ADD0010;
        SIO_SSL_AUTHTYPE       = $1ADD0020;
        SIO_SSL_CONVERT        = $1ADD0040;
        SKTS_RAWCALLBACKS      = $40000000;
        SKTS_WORKTODOCALLBACKS = $20000000;
        WAIT_ABANDONED         = $00000080;
        WAIT_TIMEOUT           = $00000102;
        WAIT_FAILED            = $FFFFFFFF;
        MAXIMUM_WAIT_OBJECTS   = 64;
        WAIT_IO_COMPLETION     = $000000C0;

  { This file contains proposed extensions to the Winsock 2 specification to }
  { support Novell's implementation of namespace providers. }

  {___[ Manifest constants ]________________________________________________________________________ }
  { Proposed output flag for deregistered services }
  const
     RESULT_IS_DEREGISTERED = $0002;
  { Proposed output flag for containers }
     RESULT_IS_CONTAINER = $0004;

  { Values used to indicate an attribute list in the blob }
  { blob contains ASCII strings }

     WS_ATTRLIST_ASCII = $b10bea1a;
  { blob contains UNICODE strings }
     WS_ATTRLIST_UNICODE = $b10bea10;
  {     Name Spaces }
  { Extends definitions in WINSOCK2.H }
     NS_BINDERY = 4;
     NS_SLP = 5;
  { Predefined BLOB Value Types }
  { Extends Predefined Value Types in winnt.h }

     REG_BOOL    = 11;  // Boolean value; TRUE or FALSE }
     REG_KEYWORD = 12;  // Keyword with no value

  {___[ Type definitions ]__________________________________________________________________________ }

    type

       TWSAATTRINFO = TWSANSCLASSINFO;
       LPWSAATTRINFO = ^TWSAATTRINFO;
       PWSAATTRINFO = ^TWSAATTRINFO;
    { Structure of a blob containing an attribute list }
    { Identifies the blob as an attribute list }
    { Number of attributes present }
    { Pointer to attribute array }

       TWSABlobAttrList = record
            dwSignature : DWORD;
            dwAttrCount : DWORD;
            lpAttributes : PWSAATTRINFO;
         end;
       LPWSABLOBATTRLIST = ^TWSABlobAttrList;
       PWSABLOBATTRLIST = ^TWSABlobAttrList;
{$endif netware}

{--------------------------------------------------------------------}

    { Socket function prototypes  }
   const
    {$ifndef netware}
    winsockdll      = 'ws2_32.dll';
    _fn_bind        = 'bind';
    _fn_closesocket = 'closesocket';
    _fn_ioctlsocket = 'ioctlsocket';
    _fn_getpeername = 'getpeername';
    _fn_getsockopt  = 'getsockopt';
    _fn_htonl       = 'htonl';
    _fn_htons       = 'htons';
    _fn_inet_addr   = 'inet_addr';
    _fn_inet_ntoa   = 'inet_ntoa';
    _fn_listen      = 'listen';
    _fn_recv        = 'recv';
    _fn_recvfrom    = 'recvfrom';
    _fn_select      = 'select';
    _fn_send        = 'send';
    _fn_sendto      = 'sendto';
    _fn_setsockopt  = 'setsockopt';
    _fn_shutdown    = 'shutdown';
    _fn_socket      = 'socket';
    _fn_gethostbyaddr    = 'gethostbyaddr';
    _fn_gethostbyname    = 'gethostbyname';
    _fn_gethostname      = 'gethostname';
    _fn_getservbyport    = 'getservbyport';
    _fn_getservbyname    = 'getservbyname';
    _fn_getprotobynumber = 'getprotobynumber';
    _fn_getprotobyname   = 'getprotobyname';
    {$else}
    winsockdll      = 'ws2_32.nlm';
    {for netware the function names for the non WSA-functions are
     different because the names are already present from bsd-sockets}
    _fn_bind        = 'WS2_32_bind';
    _fn_closesocket = 'WS2_32_closesocket';
    _fn_ioctlsocket = 'WS2_32_ioctlsocket';
    _fn_getpeername = 'WS2_32_getpeername';
    _fn_getsockopt  = 'WS2_32_getsockopt';
    _fn_htonl       = 'WS2_32_htonl';
    _fn_htons       = 'WS2_32_htons';
    _fn_inet_addr   = 'WS2_32_inet_addr';
    _fn_inet_ntoa   = 'WS2_32_inet_ntoa';
    _fn_listen      = 'WS2_32_listen';
    _fn_recv        = 'WS2_32_recv';
    _fn_recvfrom    = 'WS2_32_recvfrom';
    _fn_select      = 'WS2_32_select';
    _fn_send        = 'WS2_32_send';
    _fn_sendto      = 'WS2_32_sendto';
    _fn_setsockopt  = 'WS2_32_setsockopt';
    _fn_shutdown    = 'WS2_32_shutdown';
    _fn_socket      = 'WS2_32_socket';
    _fn_gethostbyaddr    = 'WS2_32_gethostbyaddr';
    _fn_gethostbyname    = 'WS2_32_gethostbyname';
    _fn_gethostname      = 'WS2_32_gethostname';
    _fn_getservbyport    = 'WS2_32_getservbyport';
    _fn_getservbyname    = 'WS2_32_getservbyname';
    _fn_getprotobynumber = 'WS2_32_getprotobynumber';
    _fn_getprotobyname   = 'WS2_32_getprotobyname';
    {$endif}

{
Winsock types all buffers as pchar (char *), modern POSIX does it the ANSI
C way with pointer (void *). If the pointer overloaded version doesn't exist,
a "pointer" will be passed to the "var" version. (bug 3142).
So if there are var/const versions:
- To keep ported unix code working, there must be "pointer" variants (ANSI)
- To keep Delphi/ported C Winsock code working there must be pchar variants
        (K&R)
IOW, there _must_ be 3 versions then: var/const, pchar and pointer}

    {$ifdef netware}
    function accept(s:TSocket; addr: PSockAddr; addrlen : ptOS_INT) : TSocket;
    function accept(s:TSocket; addr: PSockAddr; var addrlen : tOS_INT) : TSocket;
    {$else}
    function accept(s:TSocket; addr: PSockAddr; addrlen : ptOS_INT) : TSocket;stdcall;external winsockdll name 'accept';
    function accept(s:TSocket; addr: PSockAddr; var addrlen : tOS_INT) : TSocket;stdcall;external winsockdll name 'accept';
    {$endif}
    function bind(s:TSocket; addr: PSockaddr;namelen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_Bind;
    function bind(s:TSocket; const addr: TSockaddr;namelen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_Bind;
    function closesocket(s:TSocket):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_closesocket;
    {$ifdef netware}
    function connect(s:TSocket; addr:PSockAddr; namelen:tOS_INT):tOS_INT;
    function connect(s:TSocket; Const name:TSockAddr; namelen:tOS_INT):tOS_INT;
    {$else}
    function connect(s:TSocket; addr:PSockAddr; namelen:tOS_INT):tOS_INT; stdcall;external winsockdll name 'connect';
    function connect(s:TSocket; Const name:TSockAddr; namelen:tOS_INT):tOS_INT; stdcall;external winsockdll name 'connect';
    {$endif}

    function ioctlsocket(s:TSocket; cmd:longint; var arg:u_long):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_ioctlsocket;
    function ioctlsocket(s:TSocket; cmd:longint; var arg:longint):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_ioctlsocket;
    function ioctlsocket(s:TSocket; cmd:longint; argp:pu_long):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_ioctlsocket;
    function getpeername(s:TSocket; var name:TSockAddr;var namelen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_getpeername;
    function getsockname(s:TSocket; var name:TSockAddr;var namelen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_getpeername;
    function getsockopt(s:TSocket; level:tOS_INT; optname:tOS_INT; optval:pchar;var optlen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_getsockopt;
    function getsockopt(s:TSocket; level:tOS_INT; optname:tOS_INT; optval:pointer;var optlen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_getsockopt;
    function getsockopt(s:TSocket; level:tOS_INT; optname:tOS_INT;var optval;var optlen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_getsockopt;
    function htonl(hostlong:u_long):u_long;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_htonl;
    function htons(hostshort:u_short):u_short;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_htons;
    function inet_addr(cp:pchar):cardinal;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_inet_addr;
    function inet_ntoa(i : TInAddr):pchar;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_inet_ntoa;
    function listen(s:TSocket; backlog:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_listen;
    { are ntohl and ntohs macros or bsd-functions (for netware) ?? }
    {$ifndef netware}
    function ntohl(netlong:u_long):u_long;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'ntohl';
    function ntohs(netshort:u_short):u_short;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'ntohs';
    {$endif}

    function recv(s:TSocket;buf:pchar; len:tOS_INT; flags:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_recv;
    function recv(s:TSocket;buf:pointer; len:tOS_INT; flags:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_recv;
    function recv(s:TSocket;var buf; len:tOS_INT; flags:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_recv;
    function recvfrom(s:TSocket;buf:pchar; len:tOS_INT; flags:tOS_INT;from:PSockAddr; fromlen:ptOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_recvfrom;
    function recvfrom(s:TSocket;buf:pointer; len:tOS_INT; flags:tOS_INT;from:PSockAddr; fromlen:ptOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_recvfrom;
    function recvfrom(s:TSocket;var buf; len:tOS_INT; flags:tOS_INT;Const from:TSockAddr; var fromlen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_recvfrom;
    function select(nfds:tOS_INT; readfds,writefds,exceptfds : PFDSet;timeout: PTimeVal):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_select;
    function send(s:TSocket;const buf; len:tOS_INT; flags:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_send;
    function send(s:TSocket;buf : pchar; len:tOS_INT; flags:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_send;
    function send(s:TSocket;buf : pointer; len:tOS_INT; flags:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_send;
    function sendto(s:TSocket; buf:pchar; len:tOS_INT; flags:tOS_INT;toaddr:PSockAddr; tolen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_sendto;
    function sendto(s:TSocket; buf:pointer; len:tOS_INT; flags:tOS_INT;toaddr:PSockAddr; tolen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_sendto;
    function sendto(s:TSocket; const buf; len:tOS_INT; flags:tOS_INT;Const toaddr:TSockAddr; tolen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}
      external winsockdll name _fn_sendto;

    function setsockopt(s:TSocket; level:tOS_INT; optname:tOS_INT; optval:pchar; optlen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_setsockopt;
    function setsockopt(s:TSocket; level:tOS_INT; optname:tOS_INT; Const optval; optlen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_setsockopt;
    function setsockopt(s:TSocket; level:tOS_INT; optname:tOS_INT; optval:pointer; optlen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_setsockopt;
    function shutdown(s:TSocket; how:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_shutdown;
    function socket(af:tOS_INT; t:tOS_INT; protocol:tOS_INT):TSocket;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name _fn_socket;

    { Database function prototypes  }
    function gethostbyaddr(addr:pchar; len:tOS_INT; t:tOS_INT): PHostEnt;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_gethostbyaddr;
    function gethostbyname(name:pchar):PHostEnt;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_gethostbyname;
    function gethostname(name:pchar; namelen:tOS_INT):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_gethostname;
    function getservbyport(port:tOS_INT; proto:pchar):PServEnt;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_getservbyport;
    function getservbyname(name:pchar; proto:pchar):PServEnt;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_getservbyname;
    function getprotobynumber(proto:tOS_INT):PProtoEnt;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_getprotobynumber;
    function getprotobyname(name:pchar):PProtoEnt;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name _fn_getprotobyname;

    { Microsoft Windows Extension function prototypes  }
    function WSAStartup(wVersionRequired:word;var WSAData:TWSADATA):tOS_INT;
    function WSACleanup:tOS_INT;
    procedure WSASetLastError(iError:tOS_INT);{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSASetLastError';
    function WSAGetLastError:tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAGetLastError';
    {$ifndef netware}
    //function WSAIsBlocking:BOOL;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAIsBlocking';
    // function WSAUnhookBlockingHook:tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAUnhookBlockingHook';
    // function WSASetBlockingHook(lpBlockFunc:TFarProc):TFarProc;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSASetBlockingHook';
    {$endif}
    function WSACancelBlockingCall:tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSACancelBlockingCall';
    {$ifndef netware}
    function WSAAsyncGetServByName(hWnd:HWND; wMsg:u_int; name:pchar; proto:pchar; buf:pchar;
                                   buflen:tOS_INT):THandle;stdcall;external winsockdll name 'WSAAsyncGetServByName';
    function WSAAsyncGetServByPort(hWnd:HWND; wMsg:u_int; port:tOS_INT; proto:pchar; buf:pchar;
                                   buflen:tOS_INT):THandle;stdcall;external winsockdll name 'WSAAsyncGetServByPort';
    function WSAAsyncGetProtoByName(hWnd:HWND; wMsg:u_int; name:pchar; buf:pchar; buflen:tOS_INT):THandle;stdcall;
      external winsockdll name 'WSAAsyncGetProtoByName';
    function WSAAsyncGetProtoByNumber(hWnd:HWND; wMsg:u_int; number:tOS_INT; buf:pchar; buflen:tOS_INT):THandle;stdcall;
      external winsockdll name 'WSAAsyncGetProtoByNumber';
    function WSAAsyncGetHostByName(hWnd:HWND; wMsg:u_int; name:pchar; buf:pchar; buflen:tOS_INT):THandle;stdcall;
      external winsockdll name 'WSAAsyncGetHostByName';
    function WSAAsyncGetHostByAddr(hWnd:HWND; wMsg:u_int; addr:pchar; len:tOS_INT; t:tOS_INT;
                                   buf:pchar; buflen:tOS_INT):THandle;stdcall;
                                   external winsockdll name 'WSAAsyncGetHostByAddr';
    function WSACancelAsyncRequest(hAsyncTaskHandle:THandle):tOS_INT;stdcall;
      external winsockdll name 'WSACancelAsyncRequest';
    function WSAAsyncSelect(s:TSocket; hWnd:HWND; wMsg:u_int; lEvent:longint):tOS_INT; stdcall;
      external winsockdll name 'WSAAsyncSelect';
    function WSARecvEx(s:TSocket;var buf; len:tOS_INT; flags:ptOS_INT):tOS_INT;stdcall;
      external winsockdll name 'WSARecvEx';
    {$endif}
    function __WSAFDIsSet(s:TSocket; var FDSet:TFDSet):Bool;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name '__WSAFDIsSet';
    function __WSAFDIsSet_(s:TSocket; var FDSet:TFDSet):tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name '__WSAFDIsSet';

    {$ifndef netware}
    function TransmitFile(hSocket:TSocket; hFile:THandle; nNumberOfBytesToWrite:dword;
                          nNumberOfBytesPerSend:DWORD; lpOverlapped:POverlapped;
                          lpTransmitBuffers:PTransmitFileBuffers; dwReserved:dword):Bool;stdcall;
                          external winsockdll name 'TransmitFile';

    function AcceptEx(sListenSocket,sAcceptSocket:TSocket;
                      lpOutputBuffer:Pointer; dwReceiveDataLength,dwLocalAddressLength,
                      dwRemoteAddressLength:dword; var lpdwBytesReceived:dword;
                      lpOverlapped:POverlapped):Bool;stdcall;
                      external winsockdll name 'AcceptEx';

    procedure GetAcceptExSockaddrs(lpOutputBuffer:Pointer;
                                   dwReceiveDataLength,dwLocalAddressLength,dwRemoteAddressLength:dword;
                                   var LocalSockaddr:TSockAddr; var LocalSockaddrLength:tOS_INT;
                                   var RemoteSockaddr:TSockAddr; var RemoteSockaddrLength:tOS_INT);stdcall;
                                   external winsockdll name 'GetAcceptExSockaddrs';
    {$endif}

    function WSAMakeSyncReply(Buflen,Error:Word):dword;
    function WSAMakeSelectReply(Event,Error:Word):dword;
    function WSAGetAsyncBuflen(Param:dword):Word;
    function WSAGetAsyncError(Param:dword):Word;
    function WSAGetSelectEvent(Param:dword):Word;
    function WSAGetSelectError(Param:dword):Word;
    procedure FD_CLR(Socket:TSocket; var FDSet:TFDSet);
    function FD_ISSET(Socket:TSocket; var FDSet:TFDSet):Boolean;
    procedure FD_SET(Socket:TSocket; var FDSet:TFDSet);
    procedure FD_ZERO(var FDSet:TFDSet);

    function MAKELONG(a,b : longint) : LONGINT;
    function MAKEWORD(a,b : longint) : WORD;

    { WinSock 2 API new function prototypes }

    function WSAAccept(s: TSocket; addr:PSockAddr; addrlen : ptOS_INT;
                       lpfnCondition : TCONDITIONPROC;
                       dwCallbackData: dword) : longint;
      {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAAccept';

    function WSAAccept(s: TSocket; addr:PSockAddr; var addrlen:longint;
                       lpfnCondition : TCONDITIONPROC;
                       dwCallbackData: dword) : longint;
      {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAAccept';

    function WSACloseEvent (hEvent : TWSAEVENT) : longint;
      {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSACloseEvent';

    function WSAconnect (s:TSocket; Const name:TSockAddr;
                         namelen: tOS_INT;
                         lpCallerData, lpCaleeData : PWSABUF;
                         lpSQOS, lpGQOS : PQOS) : tOS_INT;
      {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAConnect';

    function WSAconnect (s:TSocket; name:PSockAddr;
                         namelen: tOS_INT;
                         lpCallerData, lpCaleeData : PWSABUF;
                         lpSQOS, lpGQOS : PQOS) : tOS_INT;
      {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAConnect';

    {$ifndef netware}
    function WSADuplicateSocket (s:TSocket; dwProcessId:dword; lpProtoInfo: PWSAPROTOCOL_INFOA) : longint;
      {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSADuplicateSocketA';
    {$endif}

    function WSAEnumNetworkEvents(s:TSocket;hEventObject:TWSAEVENT;lpNetworkEvents:PWSANETWORKEVENTS): longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAEnumNetworkEvents';

    function WSAEnumProtocols (lpiProtocols:LPINT;
                               lpProtocolBuffer:PWSAPROTOCOL_INFOA;
                               var lpdwBufferLength : dword) : longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAEnumProtocolsA';

    function WSAEventSelect(s:TSocket; hEventObject: TWSAEvent;lNetworkEvents:longint):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAEventSelect';

    function WSAGetOverlappedResult (s:TSocket;
                                     lpOverlapped:PWSAOVERLAPPED;
                                     lpcbTransfer : LPDWORD;
                                     fWait        : BOOL;
                                     lpdwFlags    : LPDWORD) : longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAGetOverlappedResult';

    function WSAGetQOSByName(s:TSocket; lpQOSName: LPWSABUF; lpQOS:PQOS) : longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAGetQOSByName';

    function WSAHtonl(s:TSocket; hostlong:u_long;lpnetlong:pu_long):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAHtonl';

    function WSAHtonl(s:TSocket; hostlong:u_long;var lpnetlong:u_long):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAHtonl';

    function WSAHtons(s:TSocket; hostshort:u_short;lpnetshort:pu_short):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAHtons';

    function WSAHtons(s:TSocket; hostshort:u_short;var lpnetshort:u_short):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAHtons';

    function WSAIoctl(s:TSocket;dwIoControlCode:dword;
                      lpvInBuffer:pointer; cbInBuffer:dword;
                      lpvOutBuffer:pointer; cbOutBuffer:dword;
                      lpcbBytesReturned:LPDWORD;
                      lpOverlapped:PWSAOVERLAPPED;
                      lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAIoctl';

    function WSAIoctl(s:TSocket;dwIoControlCode:dword;
                      var lpvInBuffer; cbInBuffer:dword;
                      var lpvOutBuffer; cbOutBuffer:dword;
                      var lpcbBytesReturned:DWORD;
                      lpOverlapped:PWSAOVERLAPPED;
                      lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAIoctl';

    function WSAJoinLeaf(s:TSocket; name: PSockAddr; namelen:longint;
                         lpCallerData,lpCalleeData:PWSABUF;
                         lpSQOS, lpGQOS : PQOS; dwFlags:dword):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAJoinLeaf';

    function WSANtohl(s:TSocket;netlong:u_long;lphostlong:pu_long):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSANtohl';

    function WSANtohl(s:TSocket;netlong:u_long;var hostlong:u_long):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSANtohl';

    function WSANtohs(s:TSocket;netshort:u_short;lphostshort:pu_short):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSANtohs';

    function WSANtohs(s:TSocket;netshort:u_short;var hostshort:u_short):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSANtohs';

    function WSARecv(s:TSocket;buf:pchar; dwBufferCount:dword;
                     lpNumberOfBytesRecvd,lpFlags : LPDWORD;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSARecv';

    function WSARecv(s:TSocket;buf:pointer; dwBufferCount:dword;
                     lpNumberOfBytesRecvd,lpFlags : LPDWORD;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSARecv';

    function WSARecv(s:TSocket;var buf; dwBufferCount:dword;
                     var lpNumberOfBytesRecvd,lpFlags : DWORD;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSARecv';

    function WSARecvDisconnect(s:TSocket;lpInboundDisconnectData:PWSABUF):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSARecvDisconnect';

    function WSARecvDisconnect(s:TSocket;var InboundDisconnectData:TWSABUF):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSARecvDisconnect';

    function WSARecvFrom(s:TSocket;buf:pchar; dwBufferCount:dword;
                     lpNumberOfBytesRecvd,lpFlags : LPDWORD;
                     lpFrom: PSockaddr;
                     lpFromlen: PDWORD;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSARecvFrom';

    function WSARecvFrom(s:TSocket;buf:pointer; dwBufferCount:dword;
                     lpNumberOfBytesRecvd,lpFlags : LPDWORD;
                     lpFrom: PSockaddr;
                     lpFromlen: PDWORD;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSARecvFrom';

    function WSARecvFrom(s:TSocket;var buf; dwBufferCount:dword;
                     var lpNumberOfBytesRecvd,lpFlags : DWORD;
                     var lpFrom: TSockaddr;
                     var lpFromlen: DWORD;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSARecvFrom';

    function WSAResetEvent(hEvent:TWSAEVENT):BOOL;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAResetEvent';

    function WSASend(s:TSocket;buf:pchar;len:dword;
                     NumberOfBytesSent:PDWORD; Flags:dword;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASend';

    function WSASend(s:TSocket;buf:pointer;len:dword;
                     NumberOfBytesSent:PDWORD; Flags:dword;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASend';

    function WSASend(s:TSocket;var buf;len:dword;
                     var NumberOfBytesSent: DWORD; Flags:dword;
                     lpOverlapped:PWSAOVERLAPPED;
                     lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASend';

    function WSASendDisconnect(s:TSocket;lpOutboundDisconnectData:PWSABUF):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASendDisconnect';

    function WSASendTo(s:TSocket;buf:pchar;len:dword;
                       NumberOfBytesSent:LPDWORD;
                       Flags:dword;
                       lpTo: PSockaddr;
                       iToLen:dword;
                       lpOverlapped:PWSAOVERLAPPED;
                       lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASendTo';

    function WSASendTo(s:TSocket;buf:pointer;len:dword;
                       NumberOfBytesSent:LPDWORD;
                       Flags:dword;
                       lpTo: PSockaddr;
                       iToLen:dword;
                       lpOverlapped:PWSAOVERLAPPED;
                       lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASendTo';

    function WSASendTo(s:TSocket;var buf;len:dword;
                       var NumberOfBytesSent:DWORD;
                       Flags:dword;
                       var lpTo: TSockaddr;
                       iToLen:dword;
                       lpOverlapped:PWSAOVERLAPPED;
                       lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASendTo';

    function WSASetEvent(hEvent:TWSAEVENT):BOOL;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASetEvent';

    function WSASocket(af,typ,proto:tOS_INT;
                       lpProtocolInfo:PWSAPROTOCOL_INFO;
                       g : TGROUP; Flags:dword):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSASocketA';

    function WSAWaitForMultipleEvents(cEvents:dword;
                                      lphEvents:pointer; {IN const WSAEVENT FAR * lphEvents,}
                                      fWaitAll:BOOL; dwTimeout:dword; fAlertable:BOOL):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAWaitForMultipleEvents';

    function WSAAddressToString(addr:PSockAddr; len:dword;
                               ProtocolInfo:PWSAPROTOCOL_INFO;
                               lpszAddressString:pchar;
                               lpdwAddressStringLength:lpdword):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAAddressToStringA';

    function WSAAddressToString(var addr:TSockAddr; len:dword;
                               ProtocolInfo:PWSAPROTOCOL_INFO;
                               lpszAddressString:pchar;
                               var lpdwAddressStringLength:dword):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAAddressToStringA';

    function WSAStringToAddress (AddressString  : pchar;
                                 AddressFamily  : longint;
                                 lpProtocolInfo : PWSAPROTOCOL_INFOA;
                             VAR lpAddress      : TSOCKADDR;
                             VAR lpAddressLength: LONGINT) : longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif} external winsockdll name 'WSAStringToAddressA';

    function WSAStringToAddress (AddressString  : pchar;
                                 AddressFamily  : longint;
                                 lpProtocolInfo : PWSAPROTOCOL_INFOA;
                                 lpAddress      : PSOCKADDR;
                             VAR lpAddressLength: LONGINT) : longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAStringToAddressA';

    function WSALookupServiceBegin(lpqsRestrictions:PWSAQUERYSET;
                                    ControlFlags:dword;lphLookup:PHandle):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSALookupServiceBeginA';

    function WSALookupServiceBegin(var Restrictions:TWSAQUERYSET;
                                    ControlFlags:dword;var hLookup:THandle):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSALookupServiceBeginA';

    function WSALookupServiceNext(hLookup:THandle;ControlFlags:dword;
                                   lpdwBufferLength:LPDWORD;
                                   lpqsResults:PWSAQUERYSET):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSALookupServiceNextA';

    function WSAInstallServiceClass(lpServiceClassInfo:PWSASERVICECLASSINFO):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAInstallServiceClassA';

    function WSARemoveServiceClass(lpServiceClassId:PGUID):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSARemoveServiceClass';

    function WSAGetServiceClassInfo(lpProviderId,lpServiceClassId:PGUID;
                                     lpdwBufSize:LPDWORD;
                                     lpServiceClassInfo:PWSASERVICECLASSINFO):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAGetServiceClassInfoA';

    function WSAGetServiceClassInfo(var ProviderId,ServiceClassId:TGUID;
                                    var BufSize:DWORD;
                                    var ServiceClassInfo:TWSASERVICECLASSINFO):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAGetServiceClassInfoA';

    function WSAEnumNameSpaceProviders(lpdwBufferLength:LPDWORD;lpnspBuffer:PWSANAMESPACE_INFO):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAEnumNameSpaceProvidersA';

    function WSAEnumNameSpaceProviders(var BufferLength:DWORD;var Buffer:TWSANAMESPACE_INFO):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAEnumNameSpaceProvidersA';

    function WSAGetServiceClassNameByClassId(lpServiceClassId:PGUID;lpszServiceClassName:pchar;buflen:PDWORD):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAGetServiceClassNameByClassIdA';

    function WSAGetServiceClassNameByClassId(var lpServiceClassId:TGUID;lpszServiceClassName:pchar;var buflen:DWORD):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAGetServiceClassNameByClassIdA';

    function WSASetService(lpqsRegInfo:PWSAQUERYSET;essoperation:TWSAESETSERVICEOP;flags:dword):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSASetServiceA';

   {$ifndef Netware}
    function WSAProviderConfigChange(lpNotificationHandle:LPHANDLE;
                       lpOverlapped:PWSAOVERLAPPED;
                       lpCompletionRoutine:TWSAOVERLAPPED_COMPLETION_ROUTINE):longint;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSAProviderConfigChange';
   {$endif}



  implementation

    { was #define dname(params) def_expr }
    { argument types are unknown }
    function MAKELONG(a,b : longint) : LONGINT;
    begin
       MAKELONG:=LONGINT((WORD(a)) or ((DWORD(WORD(b))) shl 16));
    end;

    function MAKEWORD(a,b : longint) : WORD;
    begin
       MAKEWORD:=WORD((BYTE(a)) or ((WORD(BYTE(b))) shl 8));
    end;

    {
      Implementation of the helper routines
    }
    function WSAMakeSyncReply(Buflen,Error:Word):dword;

      begin
         WSAMakeSyncReply:=MakeLong(Buflen, Error);
      end;

    function WSAMakeSelectReply(Event,Error:Word):dword;

      begin
         WSAMakeSelectReply:=MakeLong(Event,Error);
      end;

    function WSAGetAsyncBuflen(Param:dword):Word;

      begin
         WSAGetAsyncBuflen:=lo(Param);
      end;

    function WSAGetAsyncError(Param:dword):Word;

      begin
         WSAGetAsyncError:=hi(Param);
      end;

    function WSAGetSelectEvent(Param:dword):Word;

      begin
         WSAGetSelectEvent:=lo(Param);
      end;

    function WSAGetSelectError(Param:dword):Word;

      begin
         WSAGetSelectError:=hi(Param);
      end;

    procedure FD_CLR(Socket:TSocket; var FDSet:TFDSet);

      var
         i : u_int;

      begin
         i:=0;
         while i<FDSet.fd_count do
           begin
              if FDSet.fd_array[i]=Socket then
                begin
                   while i<FDSet.fd_count-1 do
                     begin
                        FDSet.fd_array[i]:=FDSet.fd_array[i+1];
                        inc(i);
                     end;
                   dec(FDSet.fd_count);
                   break;
                end;
              inc(i);
           end;
      end;

    function FD_ISSET(Socket:TSocket; var FDSet:TFDSet):Boolean;
    begin
       FD_ISSET:=__WSAFDIsSet(Socket,FDSet);
    end;

    procedure FD_SET(Socket:TSocket; var FDSet:TFDSet);
    var i : integer;
    begin
      if FDSet.fd_count > FD_SETSIZE then
        FDSet.fd_count := FD_SETSIZE;
      for i := 1 to FDSet.fd_count do
        if FDSet.fd_array[i-1] = Socket then exit;  {this is what the c macro FD_SET does}
      if FDSet.fd_count<FD_SETSIZE then
      begin
        FDSet.fd_array[FDSet.fd_count]:=Socket;
        Inc(FDSet.fd_count);
      end;
    end;

    procedure FD_ZERO(var FDSet:TFDSet);
    begin
      fillchar(FDSet,sizeof(FDSet),0);
      {FDSet.fd_count:=0;}
    end;

    {$ifdef netware}
      {windows has connect and accept in ws2_32.dll, netware has not, they
       are defined as macros in ws2nlm.h }

    function connect(s:TSocket; addr:PSockAddr; namelen:tOS_INT):tOS_INT;
    begin
      connect := WSAConnect (s,addr,namelen,nil,nil,nil,nil);
    end;

    function connect(s:TSocket; Const name:TSockAddr; namelen:tOS_INT):tOS_INT; //cdecl;external winsockdll name 'WSAConnect';
    begin
      connect := WSAConnect (s,@name,namelen,nil,nil,nil,nil);
    end;

    function accept(s:TSocket; addr: PSockAddr; addrlen : ptOS_INT) : TSocket;
    begin
      accept := WSAAccept (s,addr,addrlen,nil,0);
    end;

    function accept(s:TSocket; addr: PSockAddr; var addrlen : tOS_INT) : TSocket;
    begin
      accept := WSAAccept (s,addr,@addrlen,nil,0);
    end;


    {$endif}

    {AD 2003/03/25: Special for netware
     if WSAStartup is called more than once, bad thinks will happen
     on netware. This is not a problem under windows.
     This happens with fcl because the unit initialization of SSockets and
     resolve both calls WSAStartup, for the second startup we simply
     return success without calling the WS2_32 WSAStartup }

    function __WSAStartup(wVersionRequired:word;var WSAData:TWSADATA):tOS_INT;
    {$ifdef Netware}cdecl;{$else}stdcall;{$endif}
      external winsockdll name 'WSAStartup';

    function __WSACleanup:tOS_INT;{$ifdef Netware}cdecl;{$else}stdcall;{$endif}external winsockdll name 'WSACleanup';

    var WSAstartupData : TWSADATA;

    function WSACleanup:tOS_INT;
    begin
      if WSAstartupData.wVersion <> $ffff then
      begin
        Result := __WSACleanup;
        if Result = 0 then WSAstartupData.wVersion := $ffff;
      end else Result := WSANOTINITIALISED;
    end;

    function WSAStartup(wVersionRequired:word;var WSAData:TWSADATA):tOS_INT;
    begin
      if WSAstartupData.wVersion = $ffff then
      begin
        Result := __WSAStartup(wVersionRequired,WSAData);
        if Result = 0 then WSAstartupData := WSAData;
        {Writeln (stderr,'WSAStartup called');}
      end else
      begin
        result := 0;
        {Writeln (stderr,'WSAStartup should be called only once !');}
      end;
    end;

var
  oldUnloadProc : pointer;

    procedure exitProc;
    begin
      {$ifdef DEBUG_MT}
      ConsolePrintf (#13'winsock.exitProc called'#13#10);
      {$endif}
      NetwareUnloadProc := oldUnloadProc;
      WSACleanup;
    end;



initialization
  WSAstartupData.wVersion := $ffff;
  oldUnloadProc := NetwareUnloadProc;
  NetwareUnloadProc := @exitProc;
finalization
  WSACleanUp;
end.
