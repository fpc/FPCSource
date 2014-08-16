{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   OS dependant part of the header.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$linklib nsl}

const
  {
   * Address families.
    }

  { unspecified  }
    AF_UNSPEC = 0;
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
  { XEROX NS protocols  }
    AF_NS = 6;
  { ISO protocols  }
    AF_ISO = 7;
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
  { DEC Direct data link interface  }
    AF_DLI = 13;
  { LAT  }
    AF_LAT = 14;
  { NSC Hyperchannel  }
    AF_HYLINK = 15;
  { Apple Talk  }
    AF_APPLETALK = 16;
  { Internal Routing Protocol  }
    AF_ROUTE = 17;
  { Link layer interface  }
    AF_LINK = 18;
  { Moved to _ALL_SOURCE section since it doesn't begin with AF_  }
    pseudo_AF_XTP = 19;              { eXpress Transfer Protocol (no AF)  }
  { Debugging use only  }
    AF_INTF = 20;
  { raw interface  }
    AF_RIF = 21;
    AF_NETWARE = 22;
    AF_NDD = 23;
  { IPv6  }
    AF_INET6 = 24;
    AF_MAX = 30;
  {
   * Protocol families, same as address families for now.
  }
  PF_UNSPEC = AF_UNSPEC;
  PF_UNIX = AF_UNIX;
  PF_INET = AF_INET;
  PF_IMPLINK = AF_IMPLINK;
  PF_PUP = AF_PUP;
  PF_CHAOS = AF_CHAOS;
  PF_NS = AF_NS;
  PF_ISO = AF_ISO;
  PF_OSI = AF_ISO;
  PF_ECMA = AF_ECMA;
  PF_DATAKIT = AF_DATAKIT;
  PF_CCITT = AF_CCITT;
  PF_SNA = AF_SNA;
  PF_DECnet = AF_DECnet;
  PF_DLI = AF_DLI;
  PF_LAT = AF_LAT;
  PF_HYLINK = AF_HYLINK;
  PF_APPLETALK = AF_APPLETALK;
  PF_ROUTE = AF_ROUTE;
  PF_LINK = AF_LINK;
{ really just proto family, no AF  }
  PF_XTP = pseudo_AF_XTP;
  PF_RIF = AF_RIF;
{ Used by sysx/intf  }
  PF_INTF = AF_INTF;
  PF_NDD = AF_NDD;
  PF_INET6 = AF_INET6;

{ Maximum queue length specifiable by listen.  }
  SOMAXCONN     = 1024;

   SOL_SOCKET = $FFFF;
	
   { turn on debugging info recording  }
     SO_DEBUG = $0001;
   { socket has had listen()  }
     SO_ACCEPTCONN = $0002;
   { allow local address reuse  }
     SO_REUSEADDR = $0004;
   { keep connections alive  }
     SO_KEEPALIVE = $0008;
   { just use interface addresses  }
     SO_DONTROUTE = $0010;
   { permit sending of broadcast msgs  }
     SO_BROADCAST = $0020;
   { bypass hardware when possible  }
     SO_USELOOPBACK = $0040;
   { linger on close if data present  }
     SO_LINGER = $0080;
   { leave received OOB data in line  }
     SO_OOBINLINE = $0100;
   { allow local address & port reuse  }
     SO_REUSEPORT = $0200;
   { Interface will supply buffers  }
     SO_USE_IFBUFS = $0400;
   { defer checksum until receive  }
     SO_CKSUMRECV = $0800;
   { prevent local address reuse  }
     SO_NOREUSEADDR = $1000;
   { derive a in-kernel only socket  }
     SO_KERNACCEPT = $2000;
   { always use first matching route  }
     SO_NOMULTIPATH = $4000;
   { turn on socket auditing  }
     SO_AUDIT = $8000;
   {
    * Additional options, not kept in so_options.
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
   { get peer socket's process id, thread id,
   							euid and egid  }
     SO_PEERID = $1009;

   { "Socket"-level control message types:  }
   { access rights (array of int)  }
     SCM_RIGHTS = $01;
   {
    * Arguments for shutdown().
     }
   { shut down for reading  }
     SHUT_RD = 0;
   { shut down for writing  }
     SHUT_WR = 1;
   { shut down for read & write  }
     SHUT_RDWR = 2;

  { Flags for send, recv etc. }
{ process out-of-band data  }
  MSG_OOB = $1;
{ peek at incoming message  }
  MSG_PEEK = $2;
{ send without using routing tables  }
  MSG_DONTROUTE = $4;
{ data completes record  }
  MSG_EOR = $8;
{ data discarded before delivery  }
  MSG_TRUNC = $10;
{ control data lost before delivery  }
  MSG_CTRUNC = $20;
{ wait for full request or error  }
  MSG_WAITALL = $40;
{ Message contain MPEG2 data  }
  MSG_MPEG2 = $80;
{ Following used within kernel  }
{ nonblocking request  }
  MSG_NONBLOCK = $4000;
{ 4.3-format sockaddr  }
  MSG_COMPAT = $8000;
  MSG_MAXIOVLEN    =    16;
 
     TCP_NODELAY = $01;
  { set maximum segment size  }
     TCP_MAXSEG = $02;
  { set keepalive timer  }
     TCP_KEEPALIVE = $8;
(*
     TCP_NOTIFY_THRESHOLD = $10;
     TCP_ABORT_THRESHOLD = $11;
     TCP_CONN_NOTIFY_THRESHOLD = $12;
     TCP_CONN_ABORT_THRESHOLD = $13;
     TCP_RECVDSTADDR = $14;
     TCP_INIT_CWND = $15;
     TCP_KEEPALIVE_THRESHOLD = $16;
     TCP_KEEPALIVE_ABORT_THRESHOLD = $17;
     TCP_CORK = $18;
*)

//from /usr/include/netinet/in.h

    IPPROTO_IP = 0;
    { IPv6 Hop-by-Hop options  }
    IPPROTO_HOPOPTS = 0;
    { control message protocol  }
    IPPROTO_ICMP = 1;
    { group mgmt protocol  }
    IPPROTO_IGMP = 2;
    { gateway^2 (deprecated)  }
    IPPROTO_GGP = 3;
    { IP inside IP  }
    IPPROTO_IPIP = 4;
    { tcp  }
    IPPROTO_TCP = 6;
    { exterior gateway protocol  }
    IPPROTO_EGP = 8;
    { pup  }
    IPPROTO_PUP = 12;
    { user datagram protocol  }
    IPPROTO_UDP = 17;
    { xns idp  }
    IPPROTO_IDP = 22;
    { tp-4 w/ class negotiation  }
    IPPROTO_TP = 29;
    { IPv6 into IPv4  }
    IPPROTO_IPV6 = 41;
    { IPv6 Routing header  }
    IPPROTO_ROUTING = 43;
    { IPv6 fragmentation header  }
    IPPROTO_FRAGMENT = 44;
    { Quality of Service  }
    IPPROTO_QOS = 45;
    { resource reservation  }
    IPPROTO_RSVP = 46;
    IPPROTO_GRE = 47;
    { encap security payload  }
    IPPROTO_ESP = 50;
    { authentication header  }
    IPPROTO_AH = 51;
    { ICMPv6/IGMPv6/NDPv6  }
    IPPROTO_ICMPV6 = 58;
    { IPv6 no next header  }
    IPPROTO_NONE = 59;
    { IPv6 Destination options  }
    IPPROTO_DSTOPTS = 60;
    { any local network  }
    IPPROTO_LOCAL = 63;
    { ISO cnlp  }
    IPPROTO_EON = 80;
    { Banyan Vines  }
    IPPROTO_BIP = $53;
    { SCTP  }
    IPPROTO_SCTP = 132;
    { IPv6 Mobility Header  }
    IPPROTO_MH = 135;
    { raw IP packet  }
    IPPROTO_RAW = 255;
    IPPROTO_MAX = 256;

    
  { buf/ip_opts; set/get IP per-packet options  }
    IP_OPTIONS = 1;
  { int; header is included with data (raw)  }
    IP_HDRINCL = 2;
  { int; IP type of service and precedence  }
    IP_TOS = 3;
  { int; IP time to live  }
    IP_TTL = 4;
    IP_UNICAST_HOPS = IP_TTL;
  { bool; receive all IP options w/datagram  }
    IP_RECVOPTS = 5;
  { bool; receive IP options for response  }
    IP_RECVRETOPTS = 6;
  { bool; receive IP dst addr w/datagram  }
    IP_RECVDSTADDR = 7;
  { ip_opts; set/get IP per-packet options  }
    IP_RETOPTS = 8;
  { set/get IP multicast interface   }
    IP_MULTICAST_IF = 9;
  { set/get IP multicast timetolive  }
    IP_MULTICAST_TTL = 10;
    IP_MULTICAST_HOPS = IP_MULTICAST_TTL;
  { set/get IP multicast loopback    }
    IP_MULTICAST_LOOP = 11;
  { add  an IP group membership      }
    IP_ADD_MEMBERSHIP = 12;
  { drop an IP group membership      }
    IP_DROP_MEMBERSHIP = 13;
  { bool; receive MAC hdr w/datagram  }
    IP_RECVMACHDR = 14;
  { bool; receive IFNET addr  }
    IP_RECVIFINFO = 15;
  { set/get IP all points broadcast IF  }
    IP_BROADCAST_IF = 16;
  { Used by DHCP  }
    IP_DHCPMODE = 17;
    IP_RECVIF = 20;
  { int; get/set form of returned addrs  }
    IP_ADDRFORM = 22;
    IP_DONTFRAG = 25;
    IP_FINDPMTU = 26;
    IP_PMTUAGE = 27;
  { Options 28 to 31 moved to _ALL_SOURCE block.  }
  { bool; receive inbound interface  }
    IP_RECVINTERFACE = 32;
  { Option 33 moved to _ALL_SOURCE block.  }
  { int; receive TTL/hlim  }
    IP_RECVTTL = 34;
  { Option 35 moved to _ALL_SOURCE block.  }
  { normally limit m'casts to 1 hop   }
    IP_DEFAULT_MULTICAST_TTL = 1;
  { normally hear sends if a member   }
    IP_DEFAULT_MULTICAST_LOOP = 1;
  { ip_max_membership is incremented by this value to support unlimited multicast group per socket  }
    IP_INC_MEMBERSHIPS = 20;
  { ip_max_membership is initialized to 20  }
    IP_INIT_MEMBERSHIP = 20;
    {
     * IPv6 options
    }
    IPV6_UNICAST_HOPS = IP_TTL;
  { u_int  }
    IPV6_MULTICAST_IF = IP_MULTICAST_IF;
  { int  }
    IPV6_MULTICAST_HOPS = IP_MULTICAST_TTL;
    IPV6_MULTICAST_LOOP = IP_MULTICAST_LOOP;
    IPV6_ADD_MEMBERSHIP = IP_ADD_MEMBERSHIP;
    IPV6_JOIN_GROUP = IP_ADD_MEMBERSHIP;
    IPV6_DROP_MEMBERSHIP = IP_DROP_MEMBERSHIP;
    IPV6_LEAVE_GROUP = IP_DROP_MEMBERSHIP;
    IPV6_ADDRFORM = IP_ADDRFORM;
  { int; don't trigger NUD probes  }
    IPV6_NOPROBE = 28;
  { int; receive source route array  }
    IPV6_RECVSRCRT = 29;
  { int; receive interface in array  }
    IPV6_RECVIF = 30;
  { sockaddr; set outgoing iface  }
    IPV6_SENDIF = 31;
  { in6_pktinfo; set packet info  }
    IPV6_PKTINFO = 33;
    IPV6_RECVHOPS = IP_RECVTTL;
  { int; receive packet info  }
    IPV6_RECVPKTINFO = 35;
  { ancillary data style  }
    IPV6_PKTOPTIONS = 36;
  { IPv6 only communication  }
    IPV6_V6ONLY = 37;
  { random generator for flow IDs  }
    FLOW6_RAND = 49;
  { ICMPv6 filter socket option  }
    ICMP6_FILTER = 38;
  { IPV6_CHECKSUM socket option  }
    IPV6_CHECKSUM = 39;
  { ancillary-only option  }
    IPV6_HOPLIMIT = 40;
  { receive incoming hop limit  }
    IPV6_RECVHOPLIMIT = 41;
  { receive incoming traffic class  }
    IPV6_RECVTCLASS = 42;
  { traffic class ancillary value  }
    IPV6_TCLASS = 43;
  { use min mtu socket option  }
    IPV6_USE_MIN_MTU = 44;
  { do not fragment outgoing pkts  }
    IPV6_DONTFRAG = 45;
  { check path MTU for a given dst  }
    IPV6_PATHMTU = 46;
  { recv packet too big notifications  }
    IPV6_RECVPATHMTU = 47;
  { next hop socket option  }
    IPV6_NEXTHOP = 48;
  { routing header option  }
    IPV6_RTHDR = 50;
  { receive routing header  }
    IPV6_RECVRTHDR = 51;
  { Hop-by-Hop Header  }
    IPV6_HOPOPTS = 52;
  { receive hop-by-hop header  }
    IPV6_RECVHOPOPTS = 53;
  { Destination Opts Header after Rthdr
                                          or when there is no Rthdr  }
    IPV6_DSTOPTS = 54;
  { Destination Opts Header before Rthdr  }
    IPV6_RTHDRDSTOPTS = 55;
  { receive both kinds of Dopt hdrs  }
    IPV6_RECVDSTOPTS = 56;
  { deliver IPv6 hdr also  }
    IPV6_AIXRAWSOCKET = 57;

(*
    {
     * protocol-independent multicast membership options.
    }
  { join group for all sources  }
    MCAST_JOIN_GROUP = $29;
  { leave group  }
    MCAST_LEAVE_GROUP = $2a;
  { block specified source  }
    MCAST_BLOCK_SOURCE = $2b;
  { unblock specified source  }
    MCAST_UNBLOCK_SOURCE = $2c;
  { join group for specified source  }
    MCAST_JOIN_SOURCE_GROUP = $2d;
  { leave source/group pair  }
      MCAST_LEAVE_SOURCE_GROUP = $2e;
*)
