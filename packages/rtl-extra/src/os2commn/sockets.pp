{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 Yuri Prokushev
    Copyright (c) 2005 Soren Ager

    Sockets implementation for OS/2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE ObjFPC}
{ $DEFINE notUnix}      // To make ssockets.pp compile
{$ModeSwitch out}

unit Sockets;

interface

uses
  so32dll, ctypes;

const
  AF_UNSPEC      = so32dll.AF_UNSPEC;      // unspecified
  AF_LOCAL       = so32dll.AF_LOCAL;       // local to host (pipes, portals)
  AF_UNIX        = so32dll.AF_UNIX;
  AF_OS2         = so32dll.AF_OS2;
  AF_INET        = so32dll.AF_INET;        // internetwork: UDP, TCP, etc.
  AF_IMPLINK     = so32dll.AF_IMPLINK;     // arpanet imp addresses
  AF_PUP         = so32dll.AF_PUP;         // pup protocols: e.g. BSP
  AF_CHAOS       = so32dll.AF_CHAOS;       // mit CHAOS protocols
  AF_NS          = so32dll.AF_NS;          // XEROX NS protocols
  AF_ISO         = so32dll.AF_ISO;         // ISO protocols
  AF_OSI         = so32dll.AF_OSI;
  AF_ECMA        = so32dll.AF_ECMA;        // european computer manufacturers
  AF_DATAKIT     = so32dll.AF_DATAKIT;     // datakit protocols
  AF_CCITT       = so32dll.AF_CCITT;       // CCITT protocols, X.25 etc
  AF_SNA         = so32dll.AF_SNA;         // IBM SNA
  AF_DECnet      = so32dll.AF_DECnet;      // DECnet
  AF_DLI         = so32dll.AF_DLI;         // DEC Direct data link interface
  AF_LAT         = so32dll.AF_LAT;         // LAT
  AF_HYLINK      = so32dll.AF_HYLINK;      // NSC Hyperchannel
  AF_APPLETALK   = so32dll.AF_APPLETALK;   // Apple Talk
  AF_NB          = so32dll.AF_NB;          // Netbios
  AF_NETBIOS     = so32dll.AF_NETBIOS;     // Netbios
  AF_LINK        = so32dll.AF_LINK;        // Link layer interface
  pseudo_AF_XTP  = so32dll.pseudo_AF_XTP;  // eXpress Transfer Protocol (no AF)
  AF_COIP        = so32dll.AF_COIP;        // connection-oriented IP, aka ST II
  AF_CNT         = so32dll.AF_CNT;         // Computer Network Technology
  pseudo_AF_RTIP = so32dll.pseudo_AF_RTIP; // Help Identify RTIP packets
  AF_IPX         = so32dll.AF_IPX;         // Novell Internet Protocol
  AF_SIP         = so32dll.AF_SIP;         // Simple Internet Protocol
  AF_INET6       = so32dll.AF_INET6;
  pseudo_AF_PIP  = so32dll.pseudo_AF_PIP;  // Help Identify PIP packets
  AF_ROUTE       = so32dll.AF_ROUTE;       // Internal Routing Protocol
  AF_FWIP        = so32dll.AF_FWIP;        // firewall support
  AF_IPSEC       = so32dll.AF_IPSEC;       // IPSEC and encryption techniques
  AF_DES         = so32dll.AF_DES;         // DES
  AF_MD5         = so32dll.AF_MD5;
  AF_CDMF        = so32dll.AF_CDMF;

  AF_MAX         = so32dll.AF_MAX;

//  PF_LOCAL     = so32dll.PF_LOCAL;
  PF_OS2       = so32dll.PF_OS2;
  PF_IMPLINK   = so32dll.PF_IMPLINK;
  PF_PUP       = so32dll.PF_PUP;
  PF_CHAOS     = so32dll.PF_CHAOS;
  PF_NS        = so32dll.PF_NS;
  PF_ISO       = so32dll.PF_ISO;
  PF_OSI       = so32dll.PF_OSI;
  PF_ECMA      = so32dll.PF_ECMA;
  PF_DATAKIT   = so32dll.PF_DATAKIT;
  PF_CCITT     = so32dll.PF_CCITT;
  PF_SNA       = so32dll.PF_SNA;
  PF_DECnet    = so32dll.PF_DECnet;
  PF_DLI       = so32dll.PF_DLI;
  PF_LAT       = so32dll.PF_LAT;
  PF_HYLINK    = so32dll.PF_HYLINK;
  PF_APPLETALK = so32dll.PF_APPLETALK;
  PF_NETBIOS   = so32dll.PF_NB;
  PF_NB        = so32dll.PF_NB;
  PF_ROUTE     = so32dll.PF_ROUTE;
  PF_LINK      = so32dll.PF_LINK;
  PF_XTP       = so32dll.PF_XTP;  // really just proto family, no AF
  PF_COIP      = so32dll.PF_COIP;
  PF_CNT       = so32dll.PF_CNT;
  PF_SIP       = so32dll.PF_SIP;
  PF_INET6     = so32dll.PF_INET6;
  PF_IPX       = so32dll.PF_IPX;     // same format as AF_NS
  PF_RTIP      = so32dll.PF_RTIP;    // same format as AF_INET
  PF_PIP       = so32dll.PF_PIP;

  PF_MAX       = so32dll.PF_MAX;

  EsockEINTR  = SOCEINTR;
  EsockEBADF  = SOCEBADF;
  EsockEFAULT = SOCEFAULT;
  EsockEINVAL = SOCEINVAL;
  EsockEACCESS = SOCEACCES;
  EsockEMFILE  = SOCEMFILE;
  EsockEMSGSIZE = SOCEMSGSIZE;
  EsockENOBUFS = SOCENOBUFS;
  EsockENOTCONN = SOCENOTCONN;
  EsockENOTSOCK = SOCENOTSOCK;
  EsockEPROTONOSUPPORT = SOCEPROTONOSUPPORT;
  EsockEWOULDBLOCK = SOCEWOULDBLOCK;
  EsockADDRINUSE = SOCEADDRINUSE;


(***************************************************************************)
(*                                                                         *)
(*                            Option flags per-socket                      *)
(*                                                                         *)
(***************************************************************************)
const
  // turn on debugging info recording
  SO_DEBUG        = $0001;
  // socket has had listen()
  SO_ACCEPTCONN   = $0002;
  // allow local address reuse
  SO_REUSEADDR    = $0004;
  // keep connections alive
  SO_KEEPALIVE    = $0008;
  // just use interface addresses
  SO_DONTROUTE    = $0010;
  // permit sending of broadcast msgs
  SO_BROADCAST    = $0020;
  // bypass hardware when possible
  SO_USELOOPBACK  = $0040;
  // linger on close if data present
  SO_LINGER       = $0080;
  // leave received OOB data in line
  SO_OOBINLINE    = $0100;
  // limited broadcast sent on all IFs
  SO_L_BROADCAST  = $0200;
  // set if shut down called for rcv
  SO_RCV_SHUTDOWN = $0400;
  // set if shutdown called for send
  SO_SND_SHUTDOWN = $0800;
  // allow local address & port reuse
  SO_REUSEPORT    = $1000;
  // allow t/tcp on socket
  SO_TTCP         = $2000;
  // aliases so we are cross-platform
  SHUT_RD         = SO_RCV_SHUTDOWN;
  SHUT_WR         = SO_SND_SHUTDOWN;
  SHUT_RDWR       = SO_RCV_SHUTDOWN or SO_SND_SHUTDOWN;

(***************************************************************************)
(*                                                                         *)
(*                  Additional options, not kept in so_options             *)
(*                                                                         *)
(***************************************************************************)
  // send buffer size
  SO_SNDBUF   = $1001;
  // receive buffer size
  SO_RCVBUF   = $1002;
  // send low-water mark
  SO_SNDLOWAT = $1003;
  // receive low-water mark
  SO_RCVLOWAT = $1004;
  // send timeout
  SO_SNDTIMEO = $1005;
  // receive timeout
  SO_RCVTIMEO = $1006;
  // get error status and clear
  SO_ERROR    = $1007;
  // get socket type
  SO_TYPE     = $1008;
  // get socket options
  SO_OPTIONS  = $1010;


(***************************************************************************)
(*                                                                         *)
(*      Level number for (get/set)sockopt() to apply to socket itself      *)
(*                                                                         *)
(***************************************************************************)
  // options for socket level
  SOL_SOCKET = $ffff;


(***************************************************************************)
(*                                                                         *)
(*  Definitions for sysctl call. The sysctl call uses a hierarchical name  *)
(* for objects that can be examined or modified.  The name is expressed as *)
(* a sequence of integers.  Like a file path name, the meaning of each     *)
(* component depends on its place in the hierarchy. The top-level and kern *)
(* identifiers are defined here, and other identifiers are defined in the  *)
(* respective subsystem header files.                                      *)
(*                                                                         *)
(***************************************************************************)

// largest number of components supported
  CTL_MAXNAME    = 12;

  // name is a node
  CTLTYPE_NODE    =1;
  // name describes an integer
  CTLTYPE_INT     =2;
  // name describes a string
  CTLTYPE_STRING  =3;
  // name describes a 64-bit number
  CTLTYPE_QUAD    =4;
  // name describes a structure
  CTLTYPE_STRUCT  =5;
  // inetcfg sysctl code
  CTLTYPE_INETCFG =6;
  // inetver sysctl code
  CTLTYPE_INEVER  =7;

(*
 * Top-level identifiers
 *)
  // "high kernel": proc, limits
  CTL_KERN       = 1;
  // network, see socket.h
  CTL_NET        = 4;
  // OS/2 specific codes
  CTL_OS2        = 9;


{
/*
 * PF_ROUTE - Routing table
 *
 * Three additional levels are defined:
 *      Fourth: address family, 0 is wildcard
 *      Fifth: type of info, defined below
 *      Sixth: flag(s) to mask with for NET_RT_FLAGS
 */
}
  // dump; may limit to a.f.
  NET_RT_DUMP   = 1;
  // by flags, e.g. RESOLVING
  NET_RT_FLAGS  = 2;
  // survey interface list
  NET_RT_IFLIST = 3;
  NET_RT_MAXID  = 4;


(***************************************************************************)
(*                                                                         *)
(*             Maximum queue length specifiable by listen                  *)
(*                                                                         *)
(***************************************************************************)
  // Maximum queue length specifiable by listen
  SOMAXCONN = 1024;


  // process out-of-band data
  MSG_OOB       = $1;
  // peek at incoming message
  MSG_PEEK      = $2;
  // send without using routing tables
  MSG_DONTROUTE = $4;
  // send without using routing tables
  MSG_FULLREAD   = $8;
  // data completes record
  MSG_EOR        = $10;
  // data discarded before delivery
  MSG_TRUNC      = $20;
  // control data lost before delivery
  MSG_CTRUNC     = $40;
  // wait for full request or error
  MSG_WAITALL    = $80;
  // this message should be nonblocking
  MSG_DONTWAIT   = $100;
  MSG_EOF        = $200;
  // mem mapped io
  MSG_MAPIO      = $400;


(***************************************************************************)
(*                                                                         *)
(*                     "Socket"-level control message types                *)
(*                                                                         *)
(***************************************************************************)
  // access rights (array of int)
  SCM_RIGHTS = $01;


// * bsd select definitions

{
 * Select uses bit masks of file descriptors in longs.  These macros
 * manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here should
 * be enough for most uses.
}
  FD_SETSIZE = 64;

{
 * ioctl & ip trace support
}
  FIONREAD      = (Ord('f') SHL 8) OR 127;
  FIONBIO       = (Ord('f') SHL 8) OR 126;

  FIOASYNC      = (Ord('f') SHL 8) OR 125;
  FIOTCPCKSUM   = (Ord('f') SHL 8) OR 128;
  FIONSTATUS    = (Ord('f') SHL 8) OR 120;
  FIONURG       = (Ord('f') SHL 8) OR 121;

  SIOCSHIWAT    = (Ord('s') SHL 8) OR  0;
  SIOCGHIWAT    = (Ord('s') SHL 8) OR  1;
  SIOCSLOWAT    = (Ord('s') SHL 8) OR  2;
  SIOCGLOWAT    = (Ord('s') SHL 8) OR  3;
  SIOCATMARK    = (Ord('s') SHL 8) OR  7;
  SIOCSPGRP     = (Ord('s') SHL 8) OR  8;
  SIOCGPGRP     = (Ord('s') SHL 8) OR  9;
  SIOCSHOSTID   = (Ord('s') SHL 8) OR 10;

  SIOCADDRT     = (Ord('r') SHL 8) OR 10;
  SIOCDELRT     = (Ord('r') SHL 8) OR 11;
  SIOMETRIC1RT  = (Ord('r') SHL 8) OR 12;
  SIOMETRIC2RT  = (Ord('r') SHL 8) OR 13;
  SIOMETRIC3RT  = (Ord('r') SHL 8) OR 14;
  SIOMETRIC4RT  = (Ord('r') SHL 8) OR 15;

  SIOCREGADDNET = (Ord('r') SHL 8) OR 12;
  SIOCREGDELNET = (Ord('r') SHL 8) OR 13;
  SIOCREGROUTES = (Ord('r') SHL 8) OR 14;
  SIOCFLUSHROUTES=(Ord('r') SHL 8) OR 15;

  SIOCSIFADDR   = (Ord('i') SHL 8) OR 12;
  SIOCGIFADDR   = (Ord('i') SHL 8) OR 13;
  SIOCSIFDSTADDR= (Ord('i') SHL 8) OR 14;
  SIOCGIFDSTADDR= (Ord('i') SHL 8) OR 15;
  SIOCSIFFLAGS  = (Ord('i') SHL 8) OR 16;
  SIOCGIFFLAGS  = (Ord('i') SHL 8) OR 17;
  SIOCGIFBRDADDR= (Ord('i') SHL 8) OR 18;
  SIOCSIFBRDADDR= (Ord('i') SHL 8) OR 19;
  SIOCGIFCONF   = (Ord('i') SHL 8) OR 20;
  SIOCGIFNETMASK= (Ord('i') SHL 8) OR 21;
  SIOCSIFNETMASK= (Ord('i') SHL 8) OR 22;
  SIOCGIFMETRIC = (Ord('i') SHL 8) OR 23;
  SIOCSIFMETRIC = (Ord('i') SHL 8) OR 24;
  SIOCSIFSETSIG = (Ord('i') SHL 8) OR 25;
  SIOCSIFCLRSIG = (Ord('i') SHL 8) OR 26;
  SIOCSIFBRD    = (Ord('i') SHL 8) OR 27; { SINGLE-rt bcst. using old # for bkw cmpt }
  SIOCSIFALLRTB = (Ord('i') SHL 8) OR 63; { added to configure all-route broadcst }

  SIOCGIFLOAD     =(Ord('i') SHL 8) OR 27;
  SIOCSIFFILTERSRC=(Ord('i') SHL 8) OR 28;
  SIOCGIFFILTERSRC=(Ord('i') SHL 8) OR 29;

  SIOCSARP      = (Ord('i') SHL 8) OR 30;
  SIOCGARP      = (Ord('i') SHL 8) OR 31;
  SIOCDARP      = (Ord('i') SHL 8) OR 32;
  SIOCSIFSNMPSIG= (Ord('i') SHL 8) OR 33;
  SIOCSIFSNMPCLR= (Ord('i') SHL 8) OR 34;
  SIOCSIFSNMPCRC= (Ord('i') SHL 8) OR 35;
  SIOCSIFPRIORITY=(Ord('i') SHL 8) OR 36;
  SIOCGIFPRIORITY=(Ord('i') SHL 8) OR 37;
  SIOCSIFFILTERDST=(Ord('i') SHL 8) OR 38;
  SIOCGIFFILTERDST=(Ord('i') SHL 8) OR 39;
  SIOCSIF802_3  =  (Ord('i') SHL 8) OR 40;
  SIOCSIFNO802_3=  (Ord('i') SHL 8) OR 41;
  SIOCSIFNOREDIR=  (Ord('i') SHL 8) OR 42;
  SIOCSIFYESREDIR= (Ord('i') SHL 8) OR 43;

  SIOCSIFMTU    = (Ord('i') SHL 8) OR 45;
  SIOCSIFFDDI   = (Ord('i') SHL 8) OR 46;
  SIOCSIFNOFDDI = (Ord('i') SHL 8) OR 47;
  SIOCSRDBRD    = (Ord('i') SHL 8) OR 48;
  SIOCSARP_TR   = (Ord('i') SHL 8) OR 49;
  SIOCGARP_TR   = (Ord('i') SHL 8) OR 50;

{ multicast ioctls }
  SIOCADDMULTI  = (Ord('i') SHL 8) OR 51;    { add m'cast addr }
  SIOCDELMULTI  = (Ord('i') SHL 8) OR 52;    { del m'cast addr }
  SIOCMULTISBC  = (Ord('i') SHL 8) OR 61;    { use broadcast to send IP multicast }
  SIOCMULTISFA  = (Ord('i') SHL 8) OR 62;    { use functional addr to send IP multicast }


{$IFDEF SLBOOTP}
  SIOCGUNIT     = (Ord('i') SHL 8) OR 70;    { Used to retreive unit number on }
                                             { serial interface }
{$ENDIF}

  SIOCSIFSPIPE   = (Ord('i') SHL 8) OR 71;   { used to set pipe size on interface }
                                             { this is used as tcp send buffer size }
  SIOCSIFRPIPE   = (Ord('i') SHL 8) OR 72;   { used to set pipe size on interface }
                                             { this is used as tcp recv buffer size }
  SIOCSIFTCPSEG = (Ord('i') SHL 8) OR 73;    { set the TCP segment size on interface }
  SIOCSIFUSE576 = (Ord('i') SHL 8) OR 74;    { enable/disable the automatic change of mss to 576 }
                                             { if going through a router }
  SIOCGIFVALID  = (Ord('i') SHL 8) OR 75;    { to check if the interface is Valid or not }
                                             { sk June 14 1995 }
  SIOCGIFBOUND  = (Ord('i') SHL 8) OR 76;    { ioctl to return bound/shld bind ifs }
{ Interface Tracing Support }
  SIOCGIFEFLAGS = (Ord('i') SHL 8) OR 150;
  SIOCSIFEFLAGS = (Ord('i') SHL 8) OR 151;
  SIOCGIFTRACE  = (Ord('i') SHL 8) OR 152;
  SIOCSIFTRACE  = (Ord('i') SHL 8) OR 153;

{$IFDEF SLSTATS}
  SIOCSSTAT    = (Ord('i') SHL 8) OR 154;
  SIOCGSTAT    = (Ord('i') SHL 8) OR 155;
{$ENDIF}

{ NETSTAT stuff }
  SIOSTATMBUF   = (Ord('n') SHL 8) OR 40;
  SIOSTATTCP    = (Ord('n') SHL 8) OR 41;
  SIOSTATUDP    = (Ord('n') SHL 8) OR 42;
  SIOSTATIP     = (Ord('n') SHL 8) OR 43;
  SIOSTATSO     = (Ord('n') SHL 8) OR 44;
  SIOSTATRT     = (Ord('n') SHL 8) OR 45;
  SIOFLUSHRT    = (Ord('n') SHL 8) OR 46;
  SIOSTATICMP   = (Ord('n') SHL 8) OR 47;
  SIOSTATIF     = (Ord('n') SHL 8) OR 48;
  SIOSTATAT     = (Ord('n') SHL 8) OR 49;
  SIOSTATARP    = (Ord('n') SHL 8) OR 50;
  SIOSTATIF42   = (Ord('n') SHL 8) OR 51;


{*
 * User-settable options (used with setsockopt).
 *}
  TCP_NODELAY    = $01;    // don't delay send to coalesce packets
  TCP_MAXSEG     = $02;    // set maximum segment size
  TCP_MSL        = $03;    // MSL HACK
  TCP_TIMESTMP   = $04;    // RFC 1323 (RTTM TimeStamp)
  TCP_WINSCALE   = $05;    // RFC 1323 (Window Scale)
  TCP_CC         = $06;    // RFC 1644 (Connection Count)


  IFF_UP                =  $1;          // interface is up
  IFF_BROADCAST         =  $2;          // broadcast address valid
  IFF_DEBUG             =  $4;          // turn on debugging
  IFF_LOOPBACK          =  $8;          // is a loopback net
  IFF_POINTOPOINT       =  $10;         // interface is point-to-point link
  IFF_LINK2             =  $20;         // was trailers, not used
  IFF_NOTRAILERS        =  IFF_LINK2;
  IFF_RUNNING           =  $40;         // resources allocated
  IFF_NOARP             =  $80;         // no address resolution protocol
  IFF_PROMISC           =  $100;        // receive all packets
  IFF_ALLMULTI          =  $200;        // receive all multicast packets
  IFF_BRIDGE            =  $1000;       // support token ring routine field
  IFF_SNAP              =  $2000;       // support extended SAP header
  IFF_DEFMTU            =  $400;        // default mtu of 1500
  IFF_RFC1469_BC        =  1;           // using broadcast
  IFF_RFC1469_FA        =  2;           // using functional
  IFF_RFC1469_MA        =  3;           // using multicast
  IFF_ETHER             =  $4000;       // Ethernet interface
  IFF_LOOPBRD           =  $8000;       // loop back broadcasts
  IFF_MULTICAST         =  $800;        // supports multicast

  IFF_SIMPLEX           =  $10000;      // can't hear own transmissions
  IFF_OACTIVE           =  $20000;      // transmission in progress
  IFF_802_3             =  $40000;
  IFF_CANONICAL         =  $80000;
  IFF_RUNNINGBLK        =  $100000;     // threads waited for intf running

  { Interface enhanced flags }
  IFFE_PKTTRACE         =  $00000001;   // trace datalink where possible
  IFFE_IPTRACE          =  $00000002;   // trace ONLY IP packets


  { physical protocols IDs }
  HT_IP                 =  $01;  // IP
  HT_ETHER              =  $06;  // Ethernet
  HT_ISO88023           =  $07;  // CSMA CD
  HT_ISO88025           =  $09;  // Token Ring
  HT_SLIP               =  $1c;  // Serial Line IP
  HT_PPP                =  $18;  // PPP IP


  IFNAMSIZ              =  16;   // interface name length

{ in.h / inet.h const & func }

{
 * Protocols
}
  IPPROTO_IP              = 0;               { dummy for IP }
  IPPROTO_ICMP            = 1;               { control message protocol }
  IPPROTO_GGP             = 3;               { gateway^2 (deprecated) }
  IPPROTO_TCP             = 6;               { tcp }
  IPPROTO_EGP             = 8;               { exterior gateway protocol }
  IPPROTO_PUP             = 12;              { pup }
  IPPROTO_UDP             = 17;              { user datagram protocol }
  IPPROTO_IDP             = 22;              { xns idp }

  IPPROTO_RAW             = 255;             { raw IP packet }
  IPPROTO_MAX             = 256;

{
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 * Ports > IPPORT_USERRESERVED are reserved
 * for servers, not necessarily privileged.
}
  IPPORT_RESERVED         = 1024;
  IPPORT_USERRESERVED     = 5000;

{
 * Link numbers
}
  IMPLINK_IP              = 155;
  IMPLINK_LOWEXPER        = 156;
  IMPLINK_HIGHEXPER       = 158;

{
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
}
  IN_CLASSA_NET           = $ff000000;
  IN_CLASSA_NSHIFT        = 24;
  IN_CLASSA_HOST          = $00ffffff;
  IN_CLASSA_MAX           = 128;
  IN_CLASSB_NET           = $ffff0000;
  IN_CLASSB_NSHIFT        = 16;
  IN_CLASSB_HOST          = $0000ffff;
  IN_CLASSB_MAX           = 65536;

  IN_CLASSC_NET           = $ffffff00;
  IN_CLASSC_NSHIFT        = 8;
  IN_CLASSC_HOST          = $000000ff;

  INADDR_BROADCAST        = $ffffffff;     { must be masked }

  IN_LOOPBACKNET          = 127;           { official! }

{*
 * Options for use with [gs]etsockopt at the IP level.
 * }
  IP_OPTIONS            = 1;   // buf/ip_opts; set/get IP options
  IP_MULTICAST_IF       = 2;   // u_char; set/get IP multicast i/f
  IP_MULTICAST_TTL      = 3;   // u_char; set/get IP multicast ttl
  IP_MULTICAST_LOOP     = 4;   // u_char; set/get IP multicast loopback
  IP_ADD_MEMBERSHIP     = 5;   // ip_mreq; add an IP group membership
  IP_DROP_MEMBERSHIP    = 6;   // ip_mreq; drop an IP group membership
  IP_HDRINCL            = 7;   // int; header is included with data
  IP_TOS                = 8;   // int; IP type of service and preced.
  IP_TTL                = 9;   // int; IP time to live
  IP_RECVOPTS           = 10;  // bool; receive all IP opts w/dgram
  IP_RECVRETOPTS        = 11;  // bool; receive IP opts for response
  IP_RECVDSTADDR        = 12;  // bool; receive IP dst addr w/dgram
  IP_RETOPTS            = 13;  // ip_opts; set/get IP options
  IP_RECVTRRI           = 14;  // bool; receive token ring routing inf

  IP_DEFAULT_MULTICAST_TTL  = 1;    // normally limit m'casts to 1 hop
  IP_DEFAULT_MULTICAST_LOOP = 1;    // normally hear sends if a member
  IP_MAX_MEMBERSHIPS        = 20;   // per socket; must fit in one mbuf
  MAX_IN_MULTI    = 16*IP_MAX_MEMBERSHIPS;     // 320 max per os2


type
  cushort=word;
  cuint16=word;
  cuint32=cardinal;
  size_t =cuint32;
  ssize_t=cuint16;
  cint   =longint;
  pcint  =^cint;
  tsocklen=cint;
  psocklen=^tsocklen;

function InitEMXHandles: boolean;
(* This procedure shall be called before touching any socket. Once called,  *)
(* it forces dynamic loading of emx.dll and all functions start with socket *)
(* handles compatible to EMX in order to allow interworking with external   *)
(* libraries using EMX libc (e.g. OpenSSL compiled with EMX port of GCC).   *)
(* It returns true in case of successful initialization, false otherwise.   *)

function CheckEMXHandles: boolean;
(* This function checks whether EMX compatible socket handles are used. *)

function EMXSocket (ANativeSocket: cInt): cInt;

function NativeSocket (AEMXSocket: cInt): cInt;

// OS/2 stack based on BSD stack
{$DEFINE BSD}
{$I socketsh.inc}
  INVALID_SOCKET = TSocket(not(0));
  SOCKET_ERROR = -1;


Implementation

uses
  DosCalls;

{******************************************************************************
                          Basic Socket Functions
******************************************************************************}

const
  EMXHandles: boolean = false;
  EMXSysCall: pointer = nil;
  EMXLibHandle: THandle = THandle (-1);

function CheckEMXHandles: boolean;
begin
  CheckEMXHandles := EMXHandles;
end;

function InitEMXHandles: boolean;
const
  EMXLib: string [8] = 'emx.dll'#0;
  CBufLen = 260;
var
  CBuf: array [1..CBufLen] of char;
begin
  if not EMXHandles then
   begin
    if DosLoadModule (@CBuf [1], SizeOf (CBuf), @EMXLib [1], EMXLibHandle) = 0
                                                                           then
     begin
      if DosQueryProcAddr (EMXLibHandle, 2, nil, EMXSysCall) = 0 then
       EMXHandles := true;
     end;
    InitEMXHandles := EMXHandles;
   end;
end;

{$ASMMODE INTEL}
function EMXSocket (ANativeSocket: cInt): cInt; assembler;
asm
  or EMXHandles, 0
  jz @EMXSocketEnd
  mov edx, eax
  mov eax, 7F54h
  mov ecx, 0
  call EMXSysCall
@EMXSocketEnd:
end;

function NativeSocket (AEMXSocket: cInt): cInt; assembler;
asm
  or EMXHandles, 0
  jz @NativeSocketEnd
  push ebx
  mov ebx, eax
  mov eax, 7F3Bh
  call EMXSysCall
  pop ebx
@NativeSocketEnd:
end;

function SocketError: cint;
begin
  SocketError := so32dll.Sock_ErrNo;
end;

Function Socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  Socket := fpSocket (Domain, SocketType, Protocol);
end;

Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
begin
  Send:=fpSend(Sock,@Buf,BufLen,Flags);
end;

Function SendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint;Var Addr; AddrLen : Longint):Longint;
begin
  SendTo:=fpSendTo(Sock,@Buf,BufLen,Flags,@Addr,AddrLen);
end;

Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
begin
  Sock := NativeSocket (Sock);
  Recv:=so32dll.Recv(Sock,Buf,BufLen,Flags);
end;

Function RecvFrom(Sock : Longint; Var Buf; Buflen,Flags : Longint; Var Addr; var AddrLen : longInt) : longint;
begin
  Sock := NativeSocket (Sock);
  RecvFrom:=so32dll.RecvFrom(Sock,Buf,BufLen,Flags,so32dll.SockAddr(Addr),AddrLen);
end;

Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;
begin
  Bind:=fpBind(Sock,@Addr,AddrLen)=0;
end;

Function Listen(Sock,MaxConnect:Longint):Boolean;
begin
  Sock := NativeSocket (Sock);
  Listen := so32dll.Listen(Sock,MaxConnect) = 0;
end;

Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  Sock := NativeSocket (Sock);
  Accept:=so32dll.Accept(Sock,so32dll.SockAddr(Addr), AddrLen);
end;

Function Connect(Sock:Longint;const Addr; Addrlen:Longint):Boolean;
begin
  Connect:=fpConnect(Sock,@Addr,AddrLen)=0;
end;

Function Shutdown(Sock:Longint;How:Longint):Longint;
begin
  ShutDown:=fpShutDown(Sock,How);
end;

Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  Sock := NativeSocket (Sock);
  GetSocketName:=so32dll.GetSockName(Sock, so32dll.SockAddr(Addr),AddrLen);
end;

Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  Sock := NativeSocket (Sock);
  GetPeerName:=so32dll.GetPeerName(Sock,so32dll.SockAddr(Addr),AddrLen);
end;

Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;
begin
  SetSocketOptions:=fpSetSockOpt(Sock,Level,OptName,@OptVal,OptLen);
end;

Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
begin
  Sock := NativeSocket (Sock);
  GetSocketOptions:=so32dll.GetSockOpt(Sock,Level,OptName,OptVal,OptLen);
end;

Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin
{!!TODO!!
  SocketPair:=so32dll.socketpair(Domain,SocketType,Protocol,Pair);}
  //SocketCall(Socket_Sys_SocketPair,Domain,SocketType,Protocol,longint(@Pair),0,0);
  SocketPair:=-1;
end;

{ mimic the linux fpWrite/fpRead calls for the file/text socket wrapper }
function fpWrite(handle : longint;Const bufptr;size : dword) : dword;
begin
  fpWrite := dword(fpsend(handle, @bufptr, size, 0));
  if fpWrite = dword(-1) then
    fpWrite := 0;
end;

function fpRead(handle : longint;var bufptr;size : dword) : dword;
var
  d : dword;
begin
  Handle := NativeSocket (Handle);
  d:=dword(so32dll.os2_ioctl(handle,FIONREAD,d,SizeOf(d)));
  if d=dword(-1) then
   fpRead:=0
  else
   begin
    if size>d then
      size:=d;
    fpRead := dword(so32dll.recv(handle, bufptr, size, 0));
    if fpRead = dword(-1) then
     fpRead := 0
   end;
end;

{$i sockets.inc}

function fpsocket (domain:cint; xtype:cint; protocol: cint):cint;
begin
  if EMXHandles then
   fpSocket := EMXSocket (so32dll.Socket (Domain, xtype, Protocol))
  else
   fpSocket:=so32dll.Socket(Domain,xtype,Protocol);
end;

function fpsend (s:cint; msg:pointer; len:size_t; flags:cint):ssize_t;
begin
  S := NativeSocket (S);
  fpSend:=so32dll.Send(S,msg^,len,flags);
end;

function fpsendto (s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t;
begin
  S := NativeSocket (S);
  // Dubious construct, this should be checked. (IPV6 fails ?)
  fpSendTo:=so32dll.SendTo(S,msg^,Len,Flags,so32dll.SockAddr(tox^),toLen);
end;

function fprecv (s:cint; buf: pointer; len: size_t; flags: cint):ssize_t;
begin
  S := NativeSocket (S);
  fpRecv:=so32dll.Recv(S,Buf,Len,Flags);
end;

function fprecvfrom (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t;
begin
  S := NativeSocket (S);
  fpRecvFrom:=so32dll.RecvFrom(S,Buf,Len,Flags,so32dll.SockAddr(from^),FromLen^);
end;

function fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;
begin
  S := NativeSocket (S);
  fpConnect:=so32dll.Connect(S,so32dll.SockAddr(name^),nameLen);
end;

function fpshutdown     (s:cint; how:cint):cint;
begin
  S := NativeSocket (S);
  fpShutDown:=so32dll.ShutDown(S,How);
end;

function fpbind (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;
begin
  S := NativeSocket (S);
  fpbind:=so32dll.Bind(S,so32dll.SockAddr(Addrx^),AddrLen);
end;

function fplisten      (s:cint; backlog : cint):cint;
begin
  S := NativeSocket (S);
  fplisten:=so32dll.Listen(S,backlog);
end;

function fpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint;
begin
  S := NativeSocket (S);
  fpAccept:=so32dll.Accept(S,so32dll.SockAddr(Addrx^),longint(@AddrLen));
end;

function fpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint;
begin
  S := NativeSocket (S);
  fpGetSockName:=so32dll.GetSockName(S,so32dll.SockAddr(name^),nameLen^);
end;

function fpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint;
begin
  S := NativeSocket (S);
  fpGetPeerName:=so32dll.GetPeerName(S,so32dll.SockAddr(name^),NameLen^);
end;

function fpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint;
begin
  S := NativeSocket (S);
  fpGetSockOpt:=so32dll.GetSockOpt(S,Level,OptName,OptVal,OptLen^);
end;

function fpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint;
begin
  S := NativeSocket (S);
  fpSetSockOpt:=so32dll.SetSockOpt(S,Level,OptName,OptVal,OptLen);
end;

function fpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint;
begin
  fpsocketpair:=-1;
end;

Function CloseSocket(Sock:Longint):Longint;
begin
  Sock := NativeSocket (Sock);
  CloseSocket:=so32dll.soclose (Sock);
end;


Begin
  so32dll.sock_init;
End.
