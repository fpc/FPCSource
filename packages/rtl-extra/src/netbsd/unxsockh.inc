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

Const
  {
   * Address families.
    }

  { unspecified  }
     AF_UNSPEC = 0;
  { local to host (pipes, portals)  }
     AF_LOCAL = 1;
  { backward compatibility  }
     AF_UNIX = AF_LOCAL;
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
  { Internal Routing Protocol, old message format  }
     AF_OROUTE = 17;
  { Link layer interface  }
     AF_LINK = 18;
  { eXpress Transfer Protocol (no AF)  }
     pseudo_AF_XTP = 19;
  { connection-oriented IP, aka ST II  }
     AF_COIP = 20;
  { Computer Network Technology  }
     AF_CNT = 21;
  { Help Identify RTIP packets  }
     pseudo_AF_RTIP = 22;
  { Novell Internet Protocol  }
     AF_IPX = 23;
  { IP version 6  }
     AF_INET6 = 24;
  { Help Identify PIP packets  }
     pseudo_AF_PIP = 25;
  { Integrated Services Digital Network }
     AF_ISDN = 26;
  { CCITT E.164 recommendation  }
     AF_E164 = AF_ISDN;
  { native ATM access  }
     AF_NATM = 27;
  { (rev.) addr. res. prot. (RFC 826)  }
     AF_ARP = 28;
  { Internal key management protocol   }
     pseudo_AF_KEY = 29;
  { Used by BPF to not rewrite hdrs in interface output routine  }
     pseudo_AF_HDRCMPLT = 30;
  { Bluetooth: HCI, SCO, L2CAP, RFCOMM  }
     AF_BLUETOOTH = 31;
  { IEEE80211  }
     AF_IEEE80211 = 32;
  { MultiProtocol Label Switching  }
     AF_MPLS = 33;
  { Internal Routing Protocol  }
     AF_ROUTE = 34;
     AF_CAN = 35;
     AF_ETHER = 36;
     AF_MAX = 37;

  {
   * Protocol families, same as address families for now.
    }
     PF_UNSPEC = AF_UNSPEC;
     PF_LOCAL = AF_LOCAL;
  { backward compatibility  }
     PF_UNIX = PF_LOCAL;
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
     PF_OROUTE = AF_OROUTE;
     PF_LINK = AF_LINK;
  { really just proto family, no AF  }
     PF_XTP = pseudo_AF_XTP;
     PF_COIP = AF_COIP;
     PF_CNT = AF_CNT;
     PF_INET6 = AF_INET6;
  { same format as AF_NS  }
     PF_IPX = AF_IPX;
     PF_RTIP = pseudo_AF_RTIP;
     PF_PIP = pseudo_AF_PIP;
     PF_ISDN = AF_ISDN;
     PF_E164 = AF_E164;
     PF_NATM = AF_NATM;
     PF_ARP = AF_ARP;
     PF_KEY = pseudo_AF_KEY;
     PF_BLUETOOTH = AF_BLUETOOTH;
     PF_MPLS = AF_MPLS;
     PF_ROUTE = AF_ROUTE;
     PF_CAN = AF_CAN;
     PF_ETHER = AF_ETHER;
     PF_MAX = AF_MAX;


  SOCK_PACKET     = 10;


{ Maximum queue length specifiable by listen.  }
  SOMAXCONN     = 128;

{ Flags for the socket type in fpsocket and for fpaccept4 }
  SOCK_CLOEXEC  = $10000000;
  SOCK_NONBLOCK = $20000000;
  SOCK_NOSIGPIPE = $40000000;
  SOCK_CLOFORK  = cint($80000000);

        SOL_SOCKET 	 = $FFFF;
        SO_DEBUG         = $0001;        { turn on debugging info recording }
        SO_ACCEPTCONN    = $0002;        { socket has had listen() }
        SO_REUSEADDR     = $0004;        { allow local address reuse }
        SO_KEEPALIVE     = $0008;        { keep connections alive }
        SO_DONTROUTE     = $0010;        { just use interface addresses }
        SO_BROADCAST     = $0020;        { permit sending of broadcast msgs }
        SO_USELOOPBACK   = $0040;        { bypass hardware when possible }
        SO_LINGER        = $0080;        { linger on close if data present }
        SO_OOBINLINE     = $0100;        { leave received OOB data in line }
        SO_REUSEPORT     = $0200;        { allow local address & port reuse }
        SO_TIMESTAMP     = $0400;        { SO_OTIMESTAMP: timestamp with timeval of clong tv_sec }
        SO_NOSIGPIPE     = $0800;        { no SIGPIPE from EPIPE }
        SO_ACCEPTFILTER  = $1000;        { there is an accept filter }
        SO_RERROR        = $4000;        { Keep track of receive errors }

{
 * Additional options, not kept in so_options.
 }
        SO_SNDBUF        =$1001;        { send buffer size }
        SO_RCVBUF        =$1002;        { receive buffer size }
        SO_SNDLOWAT      =$1003;        { send low-water mark }
        SO_RCVLOWAT      =$1004;        { receive low-water mark }
        SO_SNDTIMEO      =$1005;        { SO_OSNDTIMEO: send timeout, timeval of clong tv_sec }
        SO_RCVTIMEO      =$1006;        { SO_ORCVTIMEO: receive timeout, timeval of clong tv_sec }
        SO_ERROR         =$1007;        { get error status and clear }
        SO_TYPE          =$1008;        { get socket type }
        SO_OVERFLOWED    =$1009;        { datagrams: return packets dropped }
        SO_NOHEADER      =$100a;        { user supplies no header to kernel }


        SHUT_RD         =0;             { shut down the reading side }
        SHUT_WR         =1;             { shut down the writing side }
        SHUT_RDWR       =2;             { shut down both sides }

	IPPROTO_IP              = 0;               { dummy for IP }
	IPPROTO_ICMP            = 1;               { control message protocol }
	IPPROTO_TCP             = 6;               { tcp }
	IPPROTO_UDP             = 17;              { user datagram protocol }


	IPPROTO_HOPOPTS		= 0 ; 		{ IP6 hop-by-hop options }
	IPPROTO_IGMP		= 2 ; 		{ group mgmt protocol }
	IPPROTO_GGP		= 3 ; 		{ gateway^2 (deprecated) }
	IPPROTO_IPV4		= 4 ; 		{ IPv4 encapsulation }
	IPPROTO_IPIP		= IPPROTO_IPV4;	{ for compatibility }
	IPPROTO_ST		= 7 ; 		{ Stream protocol II }
	IPPROTO_EGP		= 8 ; 		{ exterior gateway protocol }
	IPPROTO_PIGP		= 9 ; 		{ private interior gateway }
	IPPROTO_RCCMON		= 10; 		{ BBN RCC Monitoring }
	IPPROTO_NVPII		= 11; 		{ network voice protocol}
	IPPROTO_PUP		= 12; 		{ pup }
	IPPROTO_ARGUS		= 13; 		{ Argus }
	IPPROTO_EMCON		= 14; 		{ EMCON }
	IPPROTO_XNET		= 15; 		{ Cross Net Debugger }
	IPPROTO_CHAOS		= 16; 		{ Chaos}
	IPPROTO_MUX		= 18; 		{ Multiplexing }
	IPPROTO_MEAS		= 19; 		{ DCN Measurement Subsystems }
	IPPROTO_HMP		= 20; 		{ Host Monitoring }
	IPPROTO_PRM		= 21; 		{ Packet Radio Measurement }
	IPPROTO_IDP		= 22; 		{ xns idp }
	IPPROTO_TRUNK1		= 23; 		{ Trunk-1 }
	IPPROTO_TRUNK2		= 24; 		{ Trunk-2 }
	IPPROTO_LEAF1		= 25; 		{ Leaf-1 }
	IPPROTO_LEAF2		= 26; 		{ Leaf-2 }
	IPPROTO_RDP		= 27; 		{ Reliable Data }
	IPPROTO_IRTP		= 28; 		{ Reliable Transaction }
	IPPROTO_TP		= 29; 		{ tp-4 w/ class negotiation }
	IPPROTO_BLT		= 30; 		{ Bulk Data Transfer }
	IPPROTO_NSP		= 31; 		{ Network Services }
	IPPROTO_INP		= 32; 		{ Merit Internodal }
	IPPROTO_SEP		= 33; 		{ Sequential Exchange }
	IPPROTO_DCCP		= 33; 		{ DCCP }
	IPPROTO_3PC		= 34; 		{ Third Party Connect }
	IPPROTO_IDPR		= 35; 		{ InterDomain Policy Routing }
	IPPROTO_XTP		= 36; 		{ XTP }
	IPPROTO_DDP		= 37; 		{ Datagram Delivery }
	IPPROTO_CMTP		= 38; 		{ Control Message Transport }
	IPPROTO_TPXX		= 39; 		{ TP++ Transport }
	IPPROTO_IL		= 40; 		{ IL transport protocol }
	IPPROTO_IPV6		= 41; 		{ IP6 header }
	IPPROTO_SDRP		= 42; 		{ Source Demand Routing }
	IPPROTO_ROUTING		= 43; 		{ IP6 routing header }
	IPPROTO_FRAGMENT	= 44; 		{ IP6 fragmentation header }
	IPPROTO_IDRP		= 45; 		{ InterDomain Routing}
	IPPROTO_RSVP		= 46; 		{ resource reservation }
	IPPROTO_GRE		= 47; 		{ General Routing Encap. }
	IPPROTO_MHRP		= 48; 		{ Mobile Host Routing }
	IPPROTO_BHA		= 49; 		{ BHA }
	IPPROTO_ESP		= 50; 		{ IP6 Encap Sec. Payload }
	IPPROTO_AH		= 51; 		{ IP6 Auth Header }
	IPPROTO_INLSP		= 52; 		{ Integ. Net Layer Security }
	IPPROTO_SWIPE		= 53; 		{ IP with encryption }
	IPPROTO_NHRP		= 54; 		{ Next Hop Resolution }
	IPPROTO_MOBILE		= 55; 		{ IP Mobility }
	IPPROTO_TLSP		= 56; 		{ Transport Layer Security }
	IPPROTO_SKIP		= 57; 		{ SKIP }
	IPPROTO_ICMPV6		= 58; 		{ ICMP6 }
	IPPROTO_IPV6_ICMP	= 58; 		{ IPv6 ICMP }
	IPPROTO_NONE		= 59; 		{ IP6 no next header }
	IPPROTO_DSTOPTS		= 60; 		{ IP6 destination option }
	IPPROTO_AHIP		= 61; 		{ any host internal protocol }
	IPPROTO_CFTP		= 62; 		{ CFTP }
	IPPROTO_HELLO		= 63; 		{ "hello" routing protocol }
	IPPROTO_SATEXPAK	= 64; 		{ SATNET/Backroom EXPAK }
	IPPROTO_KRYPTOLAN	= 65; 		{ Kryptolan }
	IPPROTO_RVD		= 66; 		{ Remote Virtual Disk }
	IPPROTO_IPPC		= 67; 		{ Pluribus Packet Core }
	IPPROTO_ADFS		= 68; 		{ Any distributed FS }
	IPPROTO_SATMON		= 69; 		{ Satnet Monitoring }
	IPPROTO_VISA		= 70; 		{ VISA Protocol }
	IPPROTO_IPCV		= 71; 		{ Packet Core Utility }
	IPPROTO_CPNX		= 72; 		{ Comp. Prot. Net. Executive }
	IPPROTO_CPHB		= 73; 		{ Comp. Prot. HeartBeat }
	IPPROTO_WSN		= 74; 		{ Wang Span Network }
	IPPROTO_PVP		= 75; 		{ Packet Video Protocol }
	IPPROTO_BRSATMON	= 76; 		{ BackRoom SATNET Monitoring }
	IPPROTO_ND		= 77; 		{ Sun net disk proto (temp.) }
	IPPROTO_WBMON		= 78; 		{ WIDEBAND Monitoring }
	IPPROTO_WBEXPAK		= 79; 		{ WIDEBAND EXPAK }
	IPPROTO_EON		= 80; 		{ ISO cnlp }
	IPPROTO_VMTP		= 81; 		{ VMTP }
	IPPROTO_SVMTP		= 82; 		{ Secure VMTP }
	IPPROTO_VINES		= 83; 		{ Banyon VINES }
	IPPROTO_TTP		= 84; 		{ TTP }
	IPPROTO_IGP		= 85; 		{ NSFNET-IGP }
	IPPROTO_DGP		= 86; 		{ dissimilar gateway prot. }
	IPPROTO_TCF		= 87; 		{ TCF }
	IPPROTO_IGRP		= 88; 		{ Cisco/GXS IGRP }
	IPPROTO_OSPFIGP		= 89; 		{ OSPFIGP }
	IPPROTO_SRPC		= 90; 		{ Strite RPC protocol }
	IPPROTO_LARP		= 91; 		{ Locus Address Resolution }
	IPPROTO_MTP		= 92; 		{ Multicast Transport }
	IPPROTO_AX25		= 93; 		{ AX.25 Frames }
	IPPROTO_IPEIP		= 94; 		{ IP encapsulated in IP }
	IPPROTO_MICP		= 95; 		{ Mobile Int.ing control }
	IPPROTO_SCCSP		= 96; 		{ Semaphore Comm. security }
	IPPROTO_ETHERIP		= 97; 		{ Ethernet IP encapsulation }
	IPPROTO_ENCAP		= 98; 		{ encapsulation header }
	IPPROTO_APES		= 99; 		{ any private encr. scheme }
	IPPROTO_GMTP		= 100;		{ GMTP}
	IPPROTO_IPCOMP		= 108;		{ payload compression (IPComp) }
{ 101-254: Partly Unassigned }
	IPPROTO_PIM		= 103;		{ Protocol Independent Mcast }
	IPPROTO_CARP		= 112;		{ CARP }
	IPPROTO_VRRP		= 112;		{ VRRP RFC 2338 }
	IPPROTO_PGM		= 113;		{ PGM }
	IPPROTO_L2TP		= 115;		{ L2TPv3 }
	IPPROTO_SCTP		= 132;		{ SCTP }
	IPPROTO_PFSYNC		= 240;		{ PFSYNC }

{ last return value of *_input(), meaning "all job for this pkt is done".  }
	IPPROTO_RAW             = 255;
	IPPROTO_MAX		= 256;
	IPPROTO_DONE		= 257;

{
 * Options for use with [gs]etsockopt at the IP level.
 * First word of comment is data type; bool is stored in int.
 }
	IP_OPTIONS		= 1 ;   { buf/ip_opts; set/get IP options }
	IP_HDRINCL		= 2 ;   { int; header is included with data }
	IP_TOS			= 3 ;   { int; IP type of service and precede. }
	IP_TTL			= 4 ;   { int; IP time to live }
	IP_RECVOPTS		= 5 ;   { bool; receive all IP opts w/dgram }
	IP_RECVRETOPTS		= 6 ;   { bool; receive IP opts for response }
	IP_RECVDSTADDR		= 7 ;   { bool; receive IP dst addr w/dgram }
	IP_SENDSRCADDR		= IP_RECVDSTADDR; { cmsg_type to set src addr }
	IP_RETOPTS		= 8 ;   { ip_opts; set/get IP options }
	IP_MULTICAST_IF		= 9 ;   { u_char; set/get IP multicast i/f  }
	IP_MULTICAST_TTL	= 10;   { u_char; set/get IP multicast ttl }
	IP_MULTICAST_LOOP	= 11;   { u_char; set/get IP multicast loopback }
	IP_ADD_MEMBERSHIP	= 12;   { ip_mreq; add an IP group membership }
	IP_DROP_MEMBERSHIP	= 13;   { ip_mreq; drop an IP group membership }
	IP_MULTICAST_VIF	= 14;   { set/get IP mcast virt. iface }
	IP_RSVP_ON		= 15;   { enable RSVP in kernel }
	IP_RSVP_OFF		= 16;   { disable RSVP in kernel }
	IP_RSVP_VIF_ON		= 17;   { set RSVP per-vif socket }
	IP_PORTALGO		= 18;   { int; port selection algo (rfc6056) }
	IP_PORTRANGE		= 19;   { int; range to choose for unspecified port }
	IP_RECVIF		= 20;   { bool; receive reception if w/dgram }
	IP_ERRORMTU		= 21;   { int; get MTU of last xmit = EMSGSIZE }
	IP_IPSEC_POLICY		= 22;   { struct; get/set security policy }
	IP_RECVTTL		= 23;   { bool; receive IP TTL w/dgram }
	IP_MINTTL		= 24;   { minimum TTL for packet or drop }
	IP_PKTINFO		= 25;   { struct; set default src if/addr }
	IP_RECVPKTINFO		= 26;   { int; receive dst if/addr w/dgram }
	IP_BINDANY		= 27;   { bool: allow bind to any address }

	IP_FW_TABLE_ADD		= 40;   { add entry }
	IP_FW_TABLE_DEL		= 41;   { delete entry }
	IP_FW_TABLE_FLUSH	= 42;   { flush table }
	IP_FW_TABLE_GETSIZE	= 43;   { get table size }
	IP_FW_TABLE_LIST	= 44;   { list table contents }

	IP_FW_ADD		= 50;   { add a firewall rule to chain }
	IP_FW_DEL		= 51;   { delete a firewall rule from chain }
	IP_FW_FLUSH		= 52;   { flush firewall rule chain }
	IP_FW_ZERO		= 53;   { clear single/all firewall counter(s) }
	IP_FW_GET		= 54;   { get entire firewall rule chain }
	IP_FW_RESETLOG		= 55;   { reset logging counters }

	IP_DUMMYNET_CONFIGURE	= 60;   { add/configure a dummynet pipe }
	IP_DUMMYNET_DEL		= 61;   { delete a dummynet pipe from chain }
	IP_DUMMYNET_FLUSH	= 62;   { flush dummynet }
	IP_DUMMYNET_GET		= 64;   { get entire dummynet pipes }

	IPV6_SOCKOPT_RESERVED1	= 3 ; { reserved for future use }
	IPV6_UNICAST_HOPS	= 4 ; { int; IP6 hops }
	IPV6_MULTICAST_IF	= 9 ; { u_int; setget IP6 multicast if  }
	IPV6_MULTICAST_HOPS	= 10; { int; setget IP6 multicast hops }
	IPV6_MULTICAST_LOOP	= 11; { u_int; setget IP6 multicast loopback }
	IPV6_JOIN_GROUP		= 12; { ip6_mreq; join a group membership }
	IPV6_LEAVE_GROUP	= 13; { ip6_mreq; leave a group membership }
	IPV6_PORTRANGE		= 14; { int; range to choose for unspecified port }
	IPV6_PORTALGO		= 17; { int; port selection algo (rfc6056) }
	IPV6_CHECKSUM		= 26; { int; checksum offset for raw socket }
	IPV6_V6ONLY		= 27; { bool; make AF_INET6 sockets v6 only }
	IPV6_IPSEC_POLICY	= 28; { struct; get/set security policy }
	IPV6_FAITH		= 29; { bool; accept FAITH'ed connections }
	IPV6_RTHDRDSTOPTS	= 35; { ip6_dest; send dst option before rthdr }
	IPV6_RECVPKTINFO	= 36; { bool; recv if, dst addr }
	IPV6_RECVHOPLIMIT	= 37; { bool; recv hop limit }
	IPV6_RECVRTHDR		= 38; { bool; recv routing header }
	IPV6_RECVHOPOPTS	= 39; { bool; recv hop-by-hop option }
	IPV6_RECVDSTOPTS	= 40; { bool; recv dst option after rthdr }
	IPV6_USE_MIN_MTU	= 42; { bool; send packets at the minimum MTU }
	IPV6_RECVPATHMTU	= 43; { bool; notify an according MTU }
	IPV6_PATHMTU		= 44; { mtuinfo; get the current path MTU }

	IPV6_PKTINFO            = 46; { in6_pktinfo; send if, src addr }
 	IPV6_HOPLIMIT           = 47; { int; send hop limit }
 	IPV6_NEXTHOP            = 48; { sockaddr; next hop addr }
 	IPV6_HOPOPTS            = 49; { ip6_hbh; send hop-by-hop option }
 	IPV6_DSTOPTS            = 50; { ip6_dest; send dst option before rthdr }
 	IPV6_RTHDR              = 51; { ip6_rthdr; send routing header }
 	IPV6_PKTOPTIONS         = 52; { buf/cmsghdr; set/get IPv6 options }
	IPV6_RECVTCLASS		= 57; { bool; recv traffic class values }
	IPV6_TCLASS		= 61; { int; send traffic class value }
	IPV6_DONTFRAG		= 62; { bool; disable IPv6 fragmentation }
	IPV6_PREFER_TEMPADDR	= 63; { int; prefer temporary address as source }
	IPV6_BINDANY		= 64; { bool: allow bind to any address }

  { Flags for send, recv etc. }
  MSG_OOB       = $0001;              { Process out-of-band data}
  MSG_PEEK      = $0002;              { Peek at incoming messages }
  MSG_DONTROUTE = $0004;              { Don't use local routing }
  MSG_EOR       = $0008;              { End of record }
  MSG_TRUNC     = $0010;
  MSG_CTRUNC    = $0020;              { Control data lost before delivery }
  MSG_WAITALL   = $0040;              { Wait for a full request }
  MSG_DONTWAIT  = $0080;              { Non-blocking I/O }
  MSG_BCAST     = $0100;              { Received as link-level broadcast }
  MSG_MCAST     = $0200;              { Received as link-level multicast }
  MSG_NOSIGNAL  = $0400;              { Do not generate SIGPIPE }
  MSG_CMSG_CLOEXEC = $0800;           { Close-on-exec for received descriptors }
  MSG_NBIO      = $1000;              { Non-blocking I/O }
  MSG_WAITFORONE = $2000;             { recvmmsg: wait for one message }
  MSG_NOTIFICATION = $4000;           { SCTP notification }
  MSG_CMSG_CLOFORK = $8000;           { Close-on-fork for received descriptors }

     TCP_NODELAY = 1;
  { set maximum segment size  }
     TCP_MAXSEG = 2;
     TCP_KEEPIDLE = 3;
  { reserved for FreeBSD compat  }
     TCP_NOPUSH = 4;
     TCP_KEEPINTVL = 5;
     TCP_KEEPCNT = 6;
     TCP_KEEPINIT = 7;
  { reserved for FreeBSD compat  }
     TCP_NOOPT = 8;
  { retrieve tcp_info structure  }
     TCP_INFO = 9;
  { use MD5 digests (RFC2385)  }
     TCP_MD5SIG = $10;
  { selected congestion control  }
     TCP_CONGCTL = $20;
  { socket options for UDP  }
     UDP_ENCAP = 100;
  { Encapsulation types  }
  { draft-ietf-ipsec-nat-t-ike-00/01  }
     UDP_ENCAP_ESPINUDP_NON_IKE = 1;
  { draft-ietf-ipsec-udp-encaps-06  }
     UDP_ENCAP_ESPINUDP = 2;
  { Default encapsulation port  }
     UDP_ENCAP_ESPINUDP_PORT = 500;
  { Maximum UDP fragment size for ESP over UDP  }
     UDP_ENCAP_ESPINUDP_MAXFRAGLEN = 552;

