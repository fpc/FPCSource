{****************************************************************************

    $Id$

    This file is part of the Free Pascal run time library.
    Copyrigth (c) 2003 by Yuri Prokushev (prokushev@freemail.ru)

    This file corresponds to version 1.1 of the Windows Sockets
    specification.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}
unit pmwsock;

Interface

{$MACRO ON}

Uses OS2Def;

// The new type to be used in all instances which refer to sockets.
type
  TSocket=Cardinal;

// Select uses arrays of TSockets.  These macros manipulate such
// arrays.  FD_SETSIZE may be defined by the user before including
// this file, but the default here should be >= 64.
//
// CAVEAT IMPLEMENTOR and USER: THESE MACROS AND TYPES MUST BE
// INCLUDED IN WINSOCK.H EXACTLY AS SHOWN HERE.
const
  FD_SETSIZE = 64;

//#pragma pack(4)
type
  fd_set=record
    fd_count: Word;                             // how many are SET?
    fd_array: Array[0..FD_SETSIZE-1] of TSocket; // an array of TSockets
  end;
//#pragma pack()

Function __WSAFDIsSet(a: TSocket;var b: fd_set): Longint; cdecl;
    external 'PMWSock' name '__WSAFDIsSet';

Function FD_ISSET(a: TSocket;var b: fd_set): Longint; cdecl;
    external 'PMWSock' name '__WSAFDIsSet';

Procedure FD_CLR(ASocket: TSocket; var aset: fd_set);
Procedure FDSET(ASocket: TSocket; var FDSet: fd_set);

// Structure used in select() call, taken from the BSD file sys/time.h.
type
  timeval=record
    tv_sec: LongInt;    // seconds
    tv_usec: LongInt;   // and microseconds
  end;

// Operations on timevals.
// * NB: timercmp does not work for >= or <=.

Function timerisset(tvp: timeval): Boolean;
//Function timercmp(tvp, uvp, cmp);
Procedure timerclear(var tvp: timeval);

// Commands for ioctlsocket(),  taken from the BSD file fcntl.h.
//
// Ioctl's have the command encoded in the lower word,
// and the size of any in or out parameters in the upper
// word.  The high 2 bits of the upper word are used
// to encode the in/out status of the parameter; for now
// we restrict parameters to at most 128 bytes.

const
  IOCPARM_MASK    = $7f;               // parameters must be < 128 bytes
  IOC_VOID        = $20000000;         // no parameters
  IOC_OUT         = $40000000;         // copy out parameters
  IOC_IN          = $80000000;         // copy in parameters
  IOC_INOUT       = IOC_IN or IOC_OUT; // 0x20000000 distinguishes new &
                                       // old ioctl's
{$define _IOR(x,y,t):=(IOC_OUT or ((Longint (sizeof(t)) and IOCPARM_MASK) shl 16) or (x shl 8) or y)}

{$define _IOW(x,y,t):=(IOC_IN or ((Longint(sizeof(t)) and IOCPARM_MASK) shl 16) or (x shl 8) or y)}

const
  // get # bytes to read
  FIONREAD=(IOC_OUT or ((Longint (sizeof(Cardinal)) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 127);
  // set/clear non-blocking i/o
  FIONBIO=(IOC_IN or ((Longint(sizeof(Cardinal)) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 126);
  // set/clear async i/o
  FIOASYNC=(IOC_IN or ((Longint(sizeof(Cardinal)) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 125);

// Socket I/O Controls
const
  // set high watermark
  SIOCSHIWAT=(IOC_IN or ((Longint(sizeof(Cardinal)) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 0);
  // get high watermark
  SIOCGHIWAT=(IOC_OUT or ((Longint (sizeof(Cardinal)) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 1);
  // set low watermark
  SIOCSLOWAT=(IOC_IN or ((Longint(sizeof(Cardinal)) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 2);
  // get low watermark
  SIOCGLOWAT=(IOC_OUT or ((Longint (sizeof(Cardinal)) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 3);
  // at oob mark?
  SIOCATMARK=(IOC_OUT or ((Longint (sizeof(Cardinal)) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 7);

// Structures returned by network data base library, taken from the
// BSD file netdb.h.  All addresses are supplied in host order, and
// returned in network order (suitable for use in system calls).

type
  hostent=record
    h_name: PChar;             // official name of host
    h_aliases: PPChar;         // alias list
    h_addrtype: LongInt;       // host address type
    h_length: LongInt;         // length of address
    h_addr_list: PPChar;       // list of addresses from name server
{$define h_addr:=h_addr_list[0]}   // address, for backward compatiblity
  end;
  phostent=^hostent;

// It is assumed here that a network number
// fits in 32 bits.
type
  netent=record
    n_name: PChar;                       // official name of net
    n_aliases: PPChar;                   // alias list
    n_addrtype: Longint;                 // net address type
    n_net: Cardinal;                     // network #
  End;
  pnetent=^netent;

  servent=record
    s_name: PChar;                      // official service name
    s_aliases: PPChar;                  // alias list
    s_port: LongInt;                    // port #
    s_proto: PChar;                     // protocol to use
  end;
  pservent=^servent;

  protoent=record
    p_name: PChar;                      // official protocol name
    p_aliases: PPChar;                  // alias list
    p_proto: LongInt;                   // protocol #
  end;
  pprotoent=^protoent;

// Constants and structures defined by the internet system,
// Per RFC 790, September 1981, taken from the BSD file netinet/in.h.

// Protocols
const
  IPPROTO_IP              =0;               // dummy for IP
  IPPROTO_ICMP            =1;               // control message protocol
  IPPROTO_GGP             =2;               // gateway^2 (deprecated)
  IPPROTO_TCP             =6;               // tcp
  IPPROTO_PUP             =12;              // pup
  IPPROTO_UDP             =17;              // user datagram protocol
  IPPROTO_IDP             =22;              // xns idp
  IPPROTO_ND              =77;              // UNOFFICIAL net disk proto
  IPPROTO_RAW             =255;             // raw IP packet
  IPPROTO_MAX             =256;

// Port/socket numbers: network standard functions

  IPPORT_ECHO             =7;
  IPPORT_DISCARD          =9;
  IPPORT_SYSTAT           =11;
  IPPORT_DAYTIME          =13;
  IPPORT_NETSTAT          =15;
  IPPORT_FTP              =21;
  IPPORT_TELNET           =23;
  IPPORT_SMTP             =25;
  IPPORT_TIMESERVER       =37;
  IPPORT_NAMESERVER       =42;
  IPPORT_WHOIS            =43;
  IPPORT_MTP              =57;

// Port/socket numbers: host specific functions

  IPPORT_TFTP             =69;
  IPPORT_RJE              =77;
  IPPORT_FINGER           =79;
  IPPORT_TTYLINK          =87;
  IPPORT_SUPDUP           =95;

// UNIX TCP sockets

  IPPORT_EXECSERVER       =512;
  IPPORT_LOGINSERVER      =513;
  IPPORT_CMDSERVER        =514;
  IPPORT_EFSSERVER        =520;

// UNIX UDP sockets

  IPPORT_BIFFUDP          =512;
  IPPORT_WHOSERVER        =513;
  IPPORT_ROUTESERVER      =520;
                                        // 520+1 also used

// Ports < IPPORT_RESERVED are reserved for
// privileged processes (e.g. root).

  IPPORT_RESERVED         =1024;

// Link numbers

  IMPLINK_IP              =155;
  IMPLINK_LOWEXPER        =156;
  IMPLINK_HIGHEXPER       =158;

// Internet address (old style... should be updated)

type
  in_addr=record
  case Integer of
    1:(S_un_b:record s_b1,s_b2,s_b3,s_b4: Byte; end;);
    2:(s_un_w:record s_w1,s_w2: Word; end;);
    3:(s_addr: Cardinal);
  end;

{$define s_addr:=in_addr.S_addr} // can be used for most tcp & ip code
{$define s_host:=in_addr.S_un_b.s_b2} // host on imp
{$define s_net:=in_addr.S_un_b.s_b1} // network
{$define s_imp:=in_addr.S_un_w.s_w2} // imp
{$define s_impno:=in_addr.S_un_b.s_b4} // imp #
{$define s_lh:=in_addr.S_un_b.s_b3} // logical host


// Definitions of bits in internet address integers.
// On subnets, the decomposition of addresses to host and net parts
// is done according to subnet mask, not the masks here.
const
{$define IN_CLASSA(i):=((Longint(i) and $80000000) = 0)}
  IN_CLASSA_NET           =$ff000000;
  IN_CLASSA_NSHIFT        =24;
  IN_CLASSA_HOST          =$00ffffff;
  IN_CLASSA_MAX           =128;

{$define IN_CLASSB(i):=((Longint(i) and $c0000000) = $80000000)}
  IN_CLASSB_NET           =$ffff0000;
  IN_CLASSB_NSHIFT        =16;
  IN_CLASSB_HOST          =$0000ffff;
  IN_CLASSB_MAX           =65536;

{$define IN_CLASSC(i):=((Longint(i) and $e0000000) = $c0000000)}
  IN_CLASSC_NET           =$ffffff00;
  IN_CLASSC_NSHIFT        =8;
  IN_CLASSC_HOST          =$000000ff;

  INADDR_ANY              =$00000000;
  INADDR_LOOPBACK         =$7f000001;
  INADDR_BROADCAST        =$ffffffff;
  INADDR_NONE             =$ffffffff;

// Socket address, internet style.

Type
    sockaddr_in=Record
                      sin_family:Integer;
                      sin_port:Word;
                      sin_addr:in_addr;
                      sin_zero:Array[0..7] of Char;
    End;

const
    WSADESCRIPTION_LEN      =256;
    WSASYS_STATUS_LEN       =128;

//#pragma pack(4)
Type
    WSAData=Record
               wVersion:Word;
               wHighVersion:Word;
               szDescription: array[0..WSADESCRIPTION_LEN] of Char;
               szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
               iMaxSockets:Word;
               iMaxUdpDg:Word;
               lpVendorInfo:PChar;
    End;
    PWSADATA=^WSAData;
    LPWSADATA=^WSAData;

//#pragma pack()


// Options for use with [gs]etsockopt at the IP level.

Const
    IP_OPTIONS      =1;  // set/get IP per-packet options

// Definitions related to sockets: types, address families, options,
// taken from the BSD file sys/socket.h.

// This is used instead of -1, since the
// TSocket type is unsigned.
Const
    INVALID_SOCKET  = -1;
    SOCKET_ERROR    = -1;

// Types

Const
    SOCK_STREAM     =1;               // stream socket
    SOCK_DGRAM      =2;               // datagram socket
    SOCK_RAW        =3;               // raw-protocol interface
    SOCK_RDM        =4;               // reliably-delivered message
    SOCK_SEQPACKET  =5;               // sequenced packet stream

// Option flags per-socket.

Const
    SO_DEBUG        =$0001;          // turn on debugging info recording
    SO_ACCEPTCONN   =$0002;          // socket has had listen()
    SO_REUSEADDR    =$0004;          // allow local address reuse
    SO_KEEPALIVE    =$0008;          // keep connections alive
    SO_DONTROUTE    =$0010;          // just use interface addresses
    SO_BROADCAST    =$0020;          // permit sending of broadcast msgs
    SO_USELOOPBACK  =$0040;          // bypass hardware when possible
    SO_LINGER       =$0080;          // linger on close if data present
    SO_OOBINLINE    =$0100;          // leave received OOB data in line
    SO_DONTLINGER   =NOT SO_LINGER;  // dont linger

// Additional options.

    SO_SNDBUF       =$1001;          // send buffer size
    SO_RCVBUF       =$1002;          // receive buffer size
    SO_SNDLOWAT     =$1003;          // send low-water mark
    SO_RCVLOWAT     =$1004;          // receive low-water mark
    SO_SNDTIMEO     =$1005;          // send timeout
    SO_RCVTIMEO     =$1006;          // receive timeout
    SO_ERROR        =$1007;          // get error status and clear
    SO_TYPE         =$1008;          // get socket type

// TCP options.

Const
    TCP_NODELAY     =$0001;

// Address families.

Const
    AF_UNSPEC       =0;               // unspecified
    AF_UNIX         =1;               // local to host (pipes, portals)
    AF_INET         =2;               // internetwork: UDP, TCP, etc.
    AF_IMPLINK      =3;               // arpanet imp addresses
    AF_PUP          =4;               // pup protocols: e.g. BSP
    AF_CHAOS        =5;               // mit CHAOS protocols
    AF_NS           =6;               // XEROX NS protocols
    AF_ISO          =7;               // ISO protocols
    AF_OSI          =AF_ISO;          // OSI is ISO
    AF_ECMA         =8;               // european computer manufacturers
    AF_DATAKIT      =9;               // datakit protocols
    AF_CCITT        =10;              // CCITT protocols, X.25 etc
    AF_SNA          =11;              // IBM SNA
    AF_DECnet       =12;              // DECnet
    AF_DLI          =13;              // Direct data link interface
    AF_LAT          =14;              // LAT
    AF_HYLINK       =15;              // NSC Hyperchannel
    AF_APPLETALK    =16;              // AppleTalk
    AF_NETBIOS      =17;              // NetBios-style addresses

    AF_MAX          =18;

// Structure used by kernel to store most
// addresses.

Type
    sockaddr=Record
        sa_family:Word;                 // address family
        sa_data:Array[0..13] of char;   // up to 14 bytes of direct address
    End;

// Structure used by kernel to pass protocol
// information in raw sockets.

    sockproto=Record
        sp_family:Word;                 // address family
        sp_protocol:Word;               // protocol
    End;


// Protocol families, same as address families for now.

Const
    PF_UNSPEC       =AF_UNSPEC;
    PF_UNIX         =AF_UNIX;
    PF_INET         =AF_INET;
    PF_IMPLINK      =AF_IMPLINK;
    PF_PUP          =AF_PUP;
    PF_CHAOS        =AF_CHAOS;
    PF_NS           =AF_NS;
    PF_ISO          =AF_ISO;
    PF_OSI          =AF_OSI;
    PF_ECMA         =AF_ECMA;
    PF_DATAKIT      =AF_DATAKIT;
    PF_CCITT        =AF_CCITT;
    PF_SNA          =AF_SNA;
    PF_DECnet       =AF_DECnet;
    PF_DLI          =AF_DLI;
    PF_LAT          =AF_LAT;
    PF_HYLINK       =AF_HYLINK;
    PF_APPLETALK    =AF_APPLETALK;

    PF_MAX          =AF_MAX;

// Structure used for manipulating linger option.

Type
    linger=Record
        l_onoff:LongInt;                // option on/off
        l_linger:LongInt;               // linger time
    End;

// Level number for (get/set)sockopt() to apply to socket itself.

Const
     SOL_SOCKET      =$ffff;          // options for socket level

// Maximum queue length specifiable by listen.

     SOMAXCONN       =5;

     MSG_OOB         =1;             // process out-of-band data
     MSG_PEEK        =2;             // peek at incoming message
     MSG_DONTROUTE   =4;             // send without using routing tables
     MSG_MAXIOVLEN   =16;

// Define constant based on rfc883, used by gethostbyxxxx() calls.

     MAXGETHOSTSTRUCT =1024;

// Define flags to be used with the WSAAsyncSelect() call.

     FD_READ         =$01;
     FD_WRITE        =$02;
     FD_OOB          =$04;
     FD_ACCEPT       =$08;
     FD_CONNECT      =$10;
     FD_CLOSE        =$20;

// All Windows Sockets error constants are biased by WSABASEERR from
// the "normal"

     WSABASEERR              =10000;


// Windows Sockets definitions of regular Microsoft C error constants

     WSAEINTR                =(WSABASEERR+4);
     WSAEBADF                =(WSABASEERR+9);
     WSAEACCES               =(WSABASEERR+13);
     WSAEFAULT               =(WSABASEERR+14);
     WSAEINVAL               =(WSABASEERR+22);
     WSAEMFILE               =(WSABASEERR+24);

// Windows Sockets definitions of regular Berkeley error constants

     WSAEWOULDBLOCK          =(WSABASEERR+35);
     WSAEINPROGRESS          =(WSABASEERR+36);
     WSAEALREADY             =(WSABASEERR+37);
     WSAENOTSOCK             =(WSABASEERR+38);
     WSAEDESTADDRREQ         =(WSABASEERR+39);
     WSAEMSGSIZE             =(WSABASEERR+40);
     WSAEPROTOTYPE           =(WSABASEERR+41);
     WSAENOPROTOOPT          =(WSABASEERR+42);
     WSAEPROTONOSUPPORT      =(WSABASEERR+43);
     WSAESOCKTNOSUPPORT      =(WSABASEERR+44);
     WSAEOPNOTSUPP           =(WSABASEERR+45);
     WSAEPFNOSUPPORT         =(WSABASEERR+46);
     WSAEAFNOSUPPORT         =(WSABASEERR+47);
     WSAEADDRINUSE           =(WSABASEERR+48);
     WSAEADDRNOTAVAIL        =(WSABASEERR+49);
     WSAENETDOWN             =(WSABASEERR+50);
     WSAENETUNREACH          =(WSABASEERR+51);
     WSAENETRESET            =(WSABASEERR+52);
     WSAECONNABORTED         =(WSABASEERR+53);
     WSAECONNRESET           =(WSABASEERR+54);
     WSAENOBUFS              =(WSABASEERR+55);
     WSAEISCONN              =(WSABASEERR+56);
     WSAENOTCONN             =(WSABASEERR+57);
     WSAESHUTDOWN            =(WSABASEERR+58);
     WSAETOOMANYREFS         =(WSABASEERR+59);
     WSAETIMEDOUT            =(WSABASEERR+60);
     WSAECONNREFUSED         =(WSABASEERR+61);
     WSAELOOP                =(WSABASEERR+62);
     WSAENAMETOOLONG         =(WSABASEERR+63);
     WSAEHOSTDOWN            =(WSABASEERR+64);
     WSAEHOSTUNREACH         =(WSABASEERR+65);
     WSAENOTEMPTY            =(WSABASEERR+66);
     WSAEPROCLIM             =(WSABASEERR+67);
     WSAEUSERS               =(WSABASEERR+68);
     WSAEDQUOT               =(WSABASEERR+69);
     WSAESTALE               =(WSABASEERR+70);
     WSAEREMOTE              =(WSABASEERR+71);

// Extended Windows Sockets error constant definitions

     WSASYSNOTREADY          =(WSABASEERR+91);
     WSAVERNOTSUPPORTED      =(WSABASEERR+92);
     WSANOTINITIALISED       =(WSABASEERR+93);

// Error return codes from gethostbyname() and gethostbyaddr()
// (when using the resolver). Note that these errors are
// retrieved via WSAGetLastError() and must therefore follow
// the rules for avoiding clashes with error numbers from
// specific implementations or language run-time systems.
// For this reason the codes are based at WSABASEERR+1001.
// Note also that [WSA]NO_ADDRESS is defined only for
// compatibility purposes.

{$define h_errno:=WSAGetLastError()}

// Authoritative Answer: Host not found

     WSAHOST_NOT_FOUND       =(WSABASEERR+1001);
     HOST_NOT_FOUND          =WSAHOST_NOT_FOUND;

// Non-Authoritative: Host not found, or SERVERFAIL

     WSATRY_AGAIN            =(WSABASEERR+1002);
     TRY_AGAIN               =WSATRY_AGAIN;

// Non recoverable errors, FORMERR, REFUSED, NOTIMP

     WSANO_RECOVERY          =(WSABASEERR+1003);
     NO_RECOVERY             =WSANO_RECOVERY;

// Valid name, no data record of requested type

     WSANO_DATA              =(WSABASEERR+1004);
     NO_DATA                 =WSANO_DATA;

// no address, look for MX record

     WSANO_ADDRESS           =WSANO_DATA;
     NO_ADDRESS              =WSANO_ADDRESS;

// Windows Sockets errors redefined as regular Berkeley error constants

Const
     EWOULDBLOCK             =WSAEWOULDBLOCK;
     EINPROGRESS             =WSAEINPROGRESS;
     EALREADY                =WSAEALREADY;
     ENOTSOCK                =WSAENOTSOCK;
     EDESTADDRREQ            =WSAEDESTADDRREQ;
     EMSGSIZE                =WSAEMSGSIZE;
     EPROTOTYPE              =WSAEPROTOTYPE;
     ENOPROTOOPT             =WSAENOPROTOOPT;
     EPROTONOSUPPORT         =WSAEPROTONOSUPPORT;
     ESOCKTNOSUPPORT         =WSAESOCKTNOSUPPORT;
     EOPNOTSUPP              =WSAEOPNOTSUPP;
     EPFNOSUPPORT            =WSAEPFNOSUPPORT;
     EAFNOSUPPORT            =WSAEAFNOSUPPORT;
     EADDRINUSE              =WSAEADDRINUSE;
     EADDRNOTAVAIL           =WSAEADDRNOTAVAIL;
     ENETDOWN                =WSAENETDOWN;
     ENETUNREACH             =WSAENETUNREACH;
     ENETRESET               =WSAENETRESET;
     ECONNABORTED            =WSAECONNABORTED;
     ECONNRESET              =WSAECONNRESET;
     ENOBUFS                 =WSAENOBUFS;
     EISCONN                 =WSAEISCONN;
     ENOTCONN                =WSAENOTCONN;
     ESHUTDOWN               =WSAESHUTDOWN;
     ETOOMANYREFS            =WSAETOOMANYREFS;
     ETIMEDOUT               =WSAETIMEDOUT;
     ECONNREFUSED            =WSAECONNREFUSED;
     ELOOP                   =WSAELOOP;
     ENAMETOOLONG            =WSAENAMETOOLONG;
     EHOSTDOWN               =WSAEHOSTDOWN;
     EHOSTUNREACH            =WSAEHOSTUNREACH;
     ENOTEMPTY               =WSAENOTEMPTY;
     EPROCLIM                =WSAEPROCLIM;
     EUSERS                  =WSAEUSERS;
     EDQUOT                  =WSAEDQUOT;
     ESTALE                  =WSAESTALE;
     EREMOTE                 =WSAEREMOTE;

// Socket function prototypes

Function accept(s: TSocket; Var addr; Var addrlen: LongInt): TSocket; cdecl;
    external 'PMWSock' name 'accept';

Function bind(s: TSocket; Const addr; namelen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'bind';

Function closesocket(s: TSocket): LongInt; cdecl;
    external 'PMWSock' name 'closesocket';

Function connect(s: TSocket; Const name: sockaddr; namelen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'connect';

Function ioctlsocket(s: TSocket; cmd: LongInt; Var argp: Cardinal): LongInt; cdecl;
    external 'PMWSock' name 'ioctlsocket';

Function getpeername(s: TSocket; Var name: sockaddr; Var nameLen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'getpeername';

Function getsockname(s: TSocket;Var name: sockaddr; Var namelen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'getsockname';

Function getsockopt(s: TSocket; level, optname: LongInt;Var optval; Var optlen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'getsockopt';

Function htonl(hostlong: Cardinal): Cardinal; cdecl;
    external 'PMWSock' name 'htonl';

Function htons(hostshort: Word): Word; cdecl;
    external 'PMWSock' name 'htons';

Function inet_addr(Const cp: PChar): Cardinal; cdecl;
    external 'PMWSock' name 'inet_addr';

Function inet_ntoa(Var _in: in_addr): PChar; cdecl;
    external 'PMWSock' name 'inet_ntoa';

Function listen(s: TSocket; backlog: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'listen';

Function ntohl(netlong: Cardinal): Cardinal; cdecl;
    external 'PMWSock' name 'ntohl';

Function ntohs(netshort: Word): Word; cdecl;
    external 'PMWSock' name 'ntohs';

Function recv(s: TSocket;Var Buf; len, flags: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'recv';

Function recvfrom(s: TSocket; Var Buf: PChar; len, flags:LongInt;
                         Var from: sockaddr; Var fromLen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'recvfrom';

Function select(nfds: LongInt; Var readfds, writefds, exceptfds: fd_set;
                       Const timeout: timeval): LongInt; cdecl;
    external 'PMWSock' name 'select';

Function send(s: TSocket; Const Buf: PChar; len, flags: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'send';

Function sendto(s: TSocket; Const Buf: PChar; len, flags: LongInt;
                    Const _to: sockaddr; tolen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'sendto';

Function setsockopt(s: TSocket; level, optname: LongInt;
                           Const optval: PChar; optlen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'setsockopt';

Function shutdown(s: TSocket; how: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'shutdown';

Function socket(af, typ, protocol: LongInt): TSocket; cdecl;
    external 'PMWSock' name 'socket';

// Database function prototypes

Function gethostbyaddr(Var addr: PChar; len, typ: LongInt): phostent; cdecl;
    external 'PMWSock' name 'gethostbyaddr';

Function gethostbyname(Const name: PChar): phostent; cdecl;
    external 'PMWSock' name 'gethostbyname';

Function gethostname(Const name: PChar; namelen: LongInt): LongInt; cdecl;
    external 'PMWSock' name 'gethostname';

Function getservbyport(port: LongInt;Const proto: PChar): pservent; cdecl;
    external 'PMWSock' name 'getservbyport';

Function getservbyname(Const name, proto: PChar): pservent; cdecl;
    external 'PMWSock' name 'getservbyname';

Function getprotobynumber(proto: LongInt): pprotoent; cdecl;
    external 'PMWSock' name 'getprotobynumber';

Function getprotobyname(Const name: PChar): pprotoent; cdecl;
    external 'PMWSock' name 'getprotobyname';

// Microsoft Windows Extension function prototypes

Function WSAStartup(wVersionRequired: Word;Var aWSAData: WSAData): LongInt; cdecl;
    external 'PMWSock' name 'WSAStartup';

Function WSACleanup: LongInt; cdecl;
    external 'PMWSock' name 'WSACleanup';

Procedure WSASetLastError(iError: LongInt); cdecl;
    external 'PMWSock' name 'WSASetLastError';

Function WSAGetLastError: LongInt; cdecl;
    external 'PMWSock' name 'WSAGetLastError';

Function WSAIsBlocking: Longbool; cdecl;
    external 'PMWSock' name 'WSAIsBlocking';

Function WSAUnhookBlockingHook: LongInt; cdecl;
    external 'PMWSock' name 'WSAUnhookBlockingHook';

Function WSASetBlockingHook(lpBlockFunc: Pointer): Pointer; cdecl;
    external 'PMWSock' name 'WSASetBlockingHook';

Function WSACancelBlockingCall: LongInt; cdecl;
    external 'PMWSock' name 'WSACancelBlockingCall';

Function WSAAsyncGetServByName(ahWnd: HWND; wMsg: Cardinal;
                                      Const name, proto: PChar;
                                      Var Buf: PChar;
                                      buflen: LongInt): Cardinal; cdecl;
    external 'PMWSock' name 'WSAAsyncGetServByName';

Function WSAAsyncGetServByPort(ahWnd: HWND; wMsg: Cardinal;
                                      port: LongInt;
                                      Const proto: PChar;
                                      Var Buf: PChar;
                                      buflen: LongInt): Cardinal; cdecl;
    external 'PMWSock' name 'WSAAsyncGetServByPort';

Function WSAAsyncGetProtoByName(ahWnd: HWND; wMsg: Cardinal;
                                       Const name: PChar;
                                       Var Buf: PChar;
                                       buflen: LongInt): Cardinal; cdecl;
    external 'PMWSock' name 'WSAAsyncGetProtoByName';

Function WSAAsyncGetProtoByNumber(ahWnd: HWND; wMsg: Cardinal;
                                         number:LongInt;
                                         Var Buf: PChar;
                                         buflen: LongInt): Cardinal; cdecl;
    external 'PMWSock' name 'WSAAsyncGetProtoByNumber';

Function WSAAsyncGetHostByName(ahWnd: HWND; wMsg: Cardinal;
                                      Const name: PChar;
                                      Var Buf: PChar; buflen: LongInt): Cardinal; cdecl;
    external 'PMWSock' name 'WSAAsyncGetHostByName';

Function WSAAsyncGetHostByAddr(ahWnd: HWND; wMsg: Cardinal;
                                      Const addr: PChar; len, typ: LongInt;
                                      Var Buf: PChar; buflen:LongInt): Cardinal; cdecl;
    external 'PMWSock' name 'WSAAsyncGetHostByAddr';

Function WSACancelAsyncRequest(hAsyncTaskHandle: Cardinal): LongInt; cdecl;
    external 'PMWSock' name 'WSACancelAsyncRequest';

Function WSAAsyncSelect(s: TSocket; ahWnd: HWND; wMsg: Cardinal; lEvent: LongInt): Cardinal; cdecl;
    external 'PMWSock' name 'WSAAsyncSelect';

// Windows message parameter composition and decomposition
// macros.
//
// WSAMAKEASYNCREPLY is intended for use by the Windows Sockets implementation
// when constructing the response to a WSAAsyncGetXByY() routine.

(*
#define WSAMAKEASYNCREPLY(buflen,error)     MAKELONG(buflen,error)
/*
 * WSAMAKESELECTREPLY is intended for use by the Windows Sockets implementation
 * when constructing the response to WSAAsyncSelect().
 */
#define WSAMAKESELECTREPLY(event,error)     MAKELONG(event,error)
/*
 * WSAGETASYNCBUFLEN is intended for use by the Windows Sockets application
 * to extract the buffer length from the lParam in the response
 * to a WSAGetXByY().
 */
#define WSAGETASYNCBUFLEN(lParam)           LOUSHORT(lParam)
/*
 * WSAGETASYNCERROR is intended for use by the Windows Sockets application
 * to extract the error code from the lParam in the response
 * to a WSAGetXByY().
 */
#define WSAGETASYNCERROR(lParam)            HIUSHORT(lParam)
/*
 * WSAGETSELECTEVENT is intended for use by the Windows Sockets application
 * to extract the event code from the lParam in the response
 * to a WSAAsyncSelect().
 */
#define WSAGETSELECTEVENT(lParam)           LOUSHORT(lParam)
/*
 * WSAGETSELECTERROR is intended for use by the Windows Sockets application
 * to extract the error code from the lParam in the response
 * to a WSAAsyncSelect().
 */
#define WSAGETSELECTERROR(lParam)           HIUSHORT(lParam)
*)

Procedure FD_ZERO(var aset: fd_set);

Implementation

Procedure FD_CLR(ASocket: TSocket; var aset: fd_set);
var
  I: Cardinal;
begin
  for I:=0 to aset.fd_count do
  begin
    if (aset.fd_array[i] = ASocket) then
    begin
      while (i < (aset.fd_count-1)) do
      begin
        aset.fd_array[I]:=aset.fd_array[i+1];
        Inc(I);
      end;
      Dec(aset.fd_count);
      break;
    end;
  end;
end;

Procedure FD_ZERO(var aset: fd_set);
Begin
  aset.fd_count:=0;
End;

procedure FDSET(ASocket: TSocket; var FDSet: fd_set);
begin
  if FDSet.fd_count < FD_SETSIZE then
  begin
    FDSet.fd_array[FDSet.fd_count] := ASocket;
    Inc(FDSet.fd_count);
  end;
end;

Function timerisset(tvp: timeval): Boolean;
Begin
  TimerIsSet:=Boolean(tvp.tv_sec or tvp.tv_usec);
End;

(*
Function timercmp(tvp, uvp, cmp): Boolean;
Begin
        ((tvp)->tv_sec cmp (uvp)->tv_sec || \
         (tvp)->tv_sec == (uvp)->tv_sec && (tvp)->tv_usec cmp (uvp)->tv_usec)
End;
*)

Procedure timerclear(var tvp: timeval);
begin
  tvp.tv_sec:=0;
  tvp.tv_usec:=0;
end;

end.

{
$Log$
Revision 1.1  2003-04-04 12:02:21  yuri
+ Initial import

}
