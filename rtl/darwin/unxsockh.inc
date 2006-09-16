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
  AF_AX25         = 3;      { Amateur Radio AX.25          }
  AF_IPX          = 4;      { Novell IPX                   }
  AF_APPLETALK    = 5;      { Appletalk DDP                }
  AF_NETROM       = 6;      { Amateur radio NetROM         }
  AF_BRIDGE       = 7;       { Multiprotocol bridge         }
  AF_AAL5         = 8;       { Reserved for Werner's ATM    }
  AF_X25          = 9;       { Reserved for X.25 project    }
  AF_INET6        = 10;      { IP version 6                 }
  AF_MAX          = 12;

  SOCK_PACKET     = 10;

  PF_AX25         = AF_AX25;
  PF_IPX          = AF_IPX;
  PF_APPLETALK    = AF_APPLETALK;
  PF_NETROM       = AF_NETROM;
  PF_BRIDGE       = AF_BRIDGE;
  PF_AAL5         = AF_AAL5;
  PF_X25          = AF_X25;
  PF_INET6        = AF_INET6;

  PF_MAX          = AF_MAX;


{ Maximum queue length specifiable by listen.  }
  SOMAXCONN     = 128;

        SOL_SOCKET = $FFFF;
        SO_DEBUG         =$0001;        { turn on debugging info recording }
        SO_ACCEPTCONN    =$0002;        { socket has had listen() }
        SO_REUSEADDR     =$0004;        { allow local address reuse }
        SO_KEEPALIVE     =$0008;        { keep connections alive }
        SO_DONTROUTE     =$0010;        { just use interface addresses }
        SO_BROADCAST     =$0020;        { permit sending of broadcast msgs }
        SO_USELOOPBACK   =$0040;        { bypass hardware when possible }
        SO_LINGER        =$0080;        { linger on close if data present }
        SO_OOBINLINE     =$0100;        { leave received OOB data in line }
        SO_REUSEPORT     =$0200;        { allow local address & port reuse }
        SO_TIMESTAMP     =$0400;        { timestamp received dgram traffic }

{
 * Additional options, not kept in so_options.
 }
        SO_SNDBUF        =$1001;        { send buffer size }
        SO_RCVBUF        =$1002;        { receive buffer size }
        SO_SNDLOWAT      =$1003;        { send low-water mark }
        SO_RCVLOWAT      =$1004;        { receive low-water mark }
        SO_SNDTIMEO      =$1005;        { send timeout }
        SO_RCVTIMEO      =$1006;        { receive timeout }
        SO_ERROR         =$1007;        { get error status and clear }
        SO_TYPE          =$1008;        { get socket type }


        SHUT_RD         =0;             { shut down the reading side }
        SHUT_WR         =1;             { shut down the writing side }
        SHUT_RDWR       =2;             { shut down both sides }

      	IPPROTO_IP		= 0;		{ dummy for IP }
      	IPPROTO_HOPOPTS		= 0;		{ IP6 hop-by-hop options }
      	IPPROTO_ICMP		= 1;		{ control message protocol }
      	IPPROTO_IGMP		= 2;		{ group mgmt protocol }
      	IPPROTO_GGP		= 3;		{ gateway^2 (deprecated) }
        IPPROTO_IPV4		= 4; 		{ IPv4 encapsulation }
        IPPROTO_IPIP		= IPPROTO_IPV4;	{ for compatibility }
      	IPPROTO_TCP		= 6;		{ tcp }
      	IPPROTO_ST		= 7;		{ Stream protocol II }
      	IPPROTO_EGP		= 8;		{ exterior gateway protocol }
      	IPPROTO_PIGP		= 9;		{ private interior gateway }
      	IPPROTO_RCCMON		= 10;		{ BBN RCC Monitoring }
      	IPPROTO_NVPII		= 11;		{ network voice protocol}
      	IPPROTO_PUP		= 12;		{ pup }
      	IPPROTO_ARGUS		= 13;		{ Argus }
      	IPPROTO_EMCON		= 14;		{ EMCON }
      	IPPROTO_XNET		= 15;		{ Cross Net Debugger }
      	IPPROTO_CHAOS		= 16;		{ Chaos}
      	IPPROTO_UDP		= 17;		{ user datagram protocol }
      	IPPROTO_MUX		= 18;		{ Multiplexing }
      	IPPROTO_MEAS		= 19;		{ DCN Measurement Subsystems }
      	IPPROTO_HMP		= 20;		{ Host Monitoring }
      	IPPROTO_PRM		= 21;		{ Packet Radio Measurement }
      	IPPROTO_IDP		= 22;		{ xns idp }
      	IPPROTO_TRUNK1		= 23;		{ Trunk-1 }
      	IPPROTO_TRUNK2		= 24;		{ Trunk-2 }
      	IPPROTO_LEAF1		= 25;		{ Leaf-1 }
      	IPPROTO_LEAF2		= 26;		{ Leaf-2 }
      	IPPROTO_RDP		= 27;		{ Reliable Data }
      	IPPROTO_IRTP		= 28;		{ Reliable Transaction }
      	IPPROTO_TP		= 29; 		{ tp-4 w/ class negotiation }
      	IPPROTO_BLT		= 30;		{ Bulk Data Transfer }
      	IPPROTO_NSP		= 31;		{ Network Services }
      	IPPROTO_INP		= 32;		{ Merit Internodal }
      	IPPROTO_SEP		= 33;		{ Sequential Exchange }
      	IPPROTO_3PC		= 34;		{ Third Party Connect }
      	IPPROTO_IDPR		= 35;		{ InterDomain Policy Routing }
      	IPPROTO_XTP		= 36;		{ XTP }
      	IPPROTO_DDP		= 37;		{ Datagram Delivery }
      	IPPROTO_CMTP		= 38;		{ Control Message Transport }
      	IPPROTO_TPXX		= 39;		{ TP++ Transport }
      	IPPROTO_IL		= 40;		{ IL transport protocol }
       	IPPROTO_IPV6		= 41;		{ IP6 header }
      	IPPROTO_SDRP		= 42;		{ Source Demand Routing }
       	IPPROTO_ROUTING		= 43;	        { IP6 routing header }
       	IPPROTO_FRAGMENT	= 44;		{ IP6 fragmentation header }
      	IPPROTO_IDRP		= 45;		{ InterDomain Routing}
       	IPPROTO_RSVP		= 46; 		{ resource reservation }
      	IPPROTO_GRE		= 47;		{ General Routing Encap. }
      	IPPROTO_MHRP		= 48;		{ Mobile Host Routing }
      	IPPROTO_BHA		= 49;		{ BHA }
      	IPPROTO_ESP		= 50;		{ IP6 Encap Sec. Payload }
      	IPPROTO_AH		= 51;		{ IP6 Auth Header }
      	IPPROTO_INLSP		= 52;		{ Integ. Net Layer Security }
      	IPPROTO_SWIPE		= 53;		{ IP with encryption }
      	IPPROTO_NHRP		= 54;		{ Next Hop Resolution }
{ 55-57: Unassigned }               
       	IPPROTO_ICMPV6	  	= 58; 		{ ICMP6 }
       	IPPROTO_NONE		= 59;		{ IP6 no next header }
       	IPPROTO_DSTOPTS		= 60;  		{ IP6 destination option }
      	IPPROTO_AHIP		= 61;		{ any host internal protocol }
      	IPPROTO_CFTP		= 62;		{ CFTP }
      	IPPROTO_HELLO		= 63;		{ "hello" routing protocol }
      	IPPROTO_SATEXPAK	= 64;		{ SATNET/Backroom EXPAK }
      	IPPROTO_KRYPTOLAN	= 65;		{ Kryptolan }
      	IPPROTO_RVD		= 66;		{ Remote Virtual Disk }
      	IPPROTO_IPPC		= 67;		{ Pluribus Packet Core }
      	IPPROTO_ADFS		= 68;		{ Any distributed FS }
      	IPPROTO_SATMON		= 69;		{ Satnet Monitoring }
      	IPPROTO_VISA		= 70;		{ VISA Protocol }
      	IPPROTO_IPCV		= 71;		{ Packet Core Utility }
      	IPPROTO_CPNX		= 72;		{ Comp. Prot. Net. Executive }
      	IPPROTO_CPHB		= 73;		{ Comp. Prot. HeartBeat }
      	IPPROTO_WSN		= 74;		{ Wang Span Network }
      	IPPROTO_PVP		= 75;		{ Packet Video Protocol }
      	IPPROTO_BRSATMON	= 76;		{ BackRoom SATNET Monitoring }
      	IPPROTO_ND		= 77;		{ Sun net disk proto (temp.) }
      	IPPROTO_WBMON		= 78;		{ WIDEBAND Monitoring }
      	IPPROTO_WBEXPAK		= 79;		{ WIDEBAND EXPAK }
      	IPPROTO_EON		= 80;		{ ISO cnlp }
      	IPPROTO_VMTP		= 81;		{ VMTP }
      	IPPROTO_SVMTP		= 82;		{ Secure VMTP }
      	IPPROTO_VINES		= 83;		{ Banyon VINES }
      	IPPROTO_TTP		= 84; 		{ TTP }
      	IPPROTO_IGP		= 85;		{ NSFNET-IGP }
      	IPPROTO_DGP		= 86;		{ dissimilar gateway prot. }
      	IPPROTO_TCF		= 87;		{ TCF }
      	IPPROTO_IGRP		= 88;		{ Cisco/GXS IGRP }
      	IPPROTO_OSPFIGP		= 89;		{ OSPFIGP }
      	IPPROTO_SRPC		= 90;		{ Strite RPC protocol }
      	IPPROTO_LARP		= 91;		{ Locus Address Resoloution }
      	IPPROTO_MTP		= 92;		{ Multicast Transport }
      	IPPROTO_AX25		= 93;		{ AX.25 Frames }
      	IPPROTO_IPEIP		= 94;		{ IP encapsulated in IP }
      	IPPROTO_MICP		= 95;		{ Mobile Int.ing control }
      	IPPROTO_SCCSP		= 96;		{ Semaphore Comm. security }
      	IPPROTO_ETHERIP		= 97;		{ Ethernet IP encapsulation }
      	IPPROTO_ENCAP		= 98;		{ encapsulation header }
      	IPPROTO_APES		= 99;		{ any private encr. scheme }
      	IPPROTO_GMTP		=100;		{ GMTP}
      	IPPROTO_IPCOMP		=108;		{ payload compression (IPComp) }
{ 101-254: Partly Unassigned }	    
      	IPPROTO_PIM		=103;		{ Protocol Independent Mcast }
      	IPPROTO_PGM		=113;		{ PGM }
{ 255: Reserved }		    
{ BSD Private, local use, namespace incursion }
      	IPPROTO_DIVERT		=254;		{ divert pseudo-protocol }
      	IPPROTO_RAW		=255;		{ raw IP packet }

      	IPPROTO_MAX		=256;

      	IPPROTO_DONE		=257;

        IPV6_OPTIONS		= 1;  { buf/ip6_opts; set/get IP6 options }
        IPV6_RECVOPTS		= 5;  { bool; receive all IP6 opts w/dgram }
        IPV6_RECVRETOPTS	= 6;  { bool; receive IP6 opts for response }
        IPV6_RECVDSTADDR	= 7;  { bool; receive IP6 dst addr w/dgram }
        IPV6_RETOPTS		= 8;  { ip6_opts; set/get IP6 options }
        IPV6_SOCKOPT_RESERVED1	= 3;  { reserved for future use }
        IPV6_UNICAST_HOPS	= 4;  { int; IP6 hops }
        IPV6_MULTICAST_IF	= 9;  { __uint8_t; set/get IP6 multicast i/f  }
        IPV6_MULTICAST_HOPS	=10; { __uint8_t; set/get IP6 multicast hops }
        IPV6_MULTICAST_LOOP	=11; { __uint8_t; set/get IP6 mcast loopback }
        IPV6_JOIN_GROUP		=12; { ip6_mreq; join a group membership }
        IPV6_LEAVE_GROUP	=13; { ip6_mreq; leave a group membership }
        IPV6_PORTRANGE		=14; { int; range to choose for unspec port }
        ICMP6_FILTER		=18; { icmp6_filter; icmp6 filter }
        IPV6_PKTINFO		=19; { bool; send/recv if, src/dst addr }
        IPV6_HOPLIMIT		=20; { bool; hop limit }
        IPV6_NEXTHOP		=21; { bool; next hop addr }
        IPV6_HOPOPTS		=22; { bool; hop-by-hop option }
        IPV6_DSTOPTS		=23; { bool; destination option }
        IPV6_RTHDR		=24; { bool; routing header }
        IPV6_PKTOPTIONS		=25; { buf/cmsghdr; set/get IPv6 options }
        IPV6_CHECKSUM		=26; { int; checksum offset for raw socket }
        IPV6_V6ONLY		=27; { bool; only bind INET6 at wildcard bind }
        IPV6_BINDV6ONLY		=IPV6_V6ONLY;
        IPV6_IPSEC_POLICY	=28; { struct; get/set security policy }
      	IP_OPTIONS		=  1;    { buf/ip_opts; set/get IP options }
      	IP_HDRINCL		=  2;    { int; header is included with data }
      	IP_TOS			=  3;    { int; IP type of service and preced. }
      	IP_TTL			=  4;    { int; IP time to live }
      	IP_RECVOPTS		=  5;    { bool; receive all IP opts w/dgram }
      	IP_RECVRETOPTS		=  6;    { bool; receive IP opts for response }
      	IP_RECVDSTADDR		=  7;    { bool; receive IP dst addr w/dgram }
      	IP_RETOPTS		=  8;    { ip_opts; set/get IP options }
      	IP_MULTICAST_IF		=  9;    { u_char; set/get IP multicast i/f  }
      	IP_MULTICAST_TTL	= 10;   { u_char; set/get IP multicast ttl }
      	IP_MULTICAST_LOOP	= 11;   { u_char; set/get IP multicast loopback }
      	IP_ADD_MEMBERSHIP	= 12;   { ip_mreq; add an IP group membership }
      	IP_DROP_MEMBERSHIP	= 13;   { ip_mreq; drop an IP group membership }
