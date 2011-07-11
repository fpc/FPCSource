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

{$linklib libsocket.so}
{$linklib libnsl.so}

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
  { nbs protocols  }
     AF_NBS = 7;
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
  { Apple Talk  }
     AF_APPLETALK = 16;
  { Network Interface Tap  }
     AF_NIT = 17;
  { IEEE 802.2, also ISO 8802  }
     AF_802 = 18;
  { umbrella for all families used  }
     AF_OSI = 19;
  { CCITT X.25 in particular  }
     AF_X25 = 20;
  { AFI = 47, IDI = 4  }
     AF_OSINET = 21;
  { U.S. Government OSI  }
     AF_GOSIP = 22;
  { Novell Internet Protocol  }
     AF_IPX = 23;
  { Internal Routing Protocol  }
     AF_ROUTE = 24;
  { Link-layer interface  }
     AF_LINK = 25;
  { Internet Protocol, Version 6  }
     AF_INET6 = 26;
  { Security Association DB socket  }
     AF_KEY = 27;
  { NCA socket  }
     AF_NCA = 28;
  { Security Policy DB socket  }
     AF_POLICY = 29;
     AF_MAX = 29;
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
     PF_NBS = AF_NBS;
     PF_ECMA = AF_ECMA;
     PF_DATAKIT = AF_DATAKIT;
     PF_CCITT = AF_CCITT;
     PF_SNA = AF_SNA;
     PF_DECnet = AF_DECnet;
     PF_DLI = AF_DLI;
     PF_LAT = AF_LAT;
     PF_HYLINK = AF_HYLINK;
     PF_APPLETALK = AF_APPLETALK;
     PF_NIT = AF_NIT;
     PF_802 = AF_802;
     PF_OSI = AF_OSI;
     PF_X25 = AF_X25;
     PF_OSINET = AF_OSINET;
     PF_GOSIP = AF_GOSIP;
     PF_IPX = AF_IPX;
     PF_ROUTE = AF_ROUTE;
     PF_LINK = AF_LINK;
     PF_INET6 = AF_INET6;
     PF_KEY = AF_KEY;
     PF_NCA = AF_NCA;
     PF_POLICY = AF_POLICY;
     PF_MAX = AF_MAX;

{ Maximum queue length specifiable by listen.  }
  SOMAXCONN     = 128;

   SOL_SOCKET = $FFFF;
	
   SO_DEBUG        = $0001;          { turn on debugging info recording }
   SO_ACCEPTCONN   = $0002;          { socket has had listen() }
   SO_REUSEADDR    = $0004;          { allow local address reuse }
   SO_KEEPALIVE    = $0008;          { keep connections alive }
   SO_DONTROUTE    = $0010;          { just use interface addresses }
   SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
   SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
   SO_LINGER       = $0080;          { linger on close if data present }
   SO_OOBINLINE    = $0100;          { leave received OOB data in line }
   SO_DGRAM_ERRIND = $0200;          { Application wants delayed error }

   SO_DONTLINGER   = not SO_LINGER;    { ~SO_LINGER }

   { * Additional options, not kept in so_options. }
   SO_SNDBUF      = $1001;          { send buffer size }
   SO_RCVBUF      = $1002;          { receive buffer size }
   SO_SNDLOWAT    = $1003;          { send low-water mark }
   SO_RCVLOWAT    = $1004;          { receive low-water mark }
   SO_SNDTIMEO    = $1005;          { send timeout }
   SO_RCVTIMEO    = $1006;          { receive timeout }
   SO_ERROR       = $1007;          { get error status and clear }
   SO_TYPE        = $1008;          { get socket type }
   SO_PROTOTYPE   = $1009;          { get/set protocol type }

   { "Socket"-level control message types: }
   SCM_RIGHTS     = $1010;          { access rights (array of int) }

   SO_STATE       = $2000;          { Internal: get so_state }

   SHUT_RD         = 0;             { shut down the reading side }
   SHUT_WR         = 1;             { shut down the writing side }
   SHUT_RDWR       = 2;             { shut down both sides }

  { Flags for send, recv etc. }
  MSG_OOB          = $0001;              { Process out-of-band data}
  MSG_PEEK         = $0002;              { Peek at incoming messages }
  MSG_DONTROUTE    = $0004;              { Don't use local routing }
  MSG_EOR          = $0008;              { End of record }
  MSG_CTRUNC       = $0010;              { Control data lost before delivery }
  MSG_TRUNC        = $0020;
  MSG_WAITALL      = $0040;              { Wait for a full request }
  MSG_DONTWAIT     = $0080;              { Non-blocking I/O }
  MSG_NOTIFICATION = $0100;           { Notification, not data }
  MSG_XPG4_2       = $8000;	      { Private: XPG4.2 flag }
  MSG_MAXIOVLEN    =    16;
 
     TCP_NODELAY = $01;
  { set maximum segment size  }
     TCP_MAXSEG = $02;
  { set keepalive timer  }
     TCP_KEEPALIVE = $8;
     TCP_NOTIFY_THRESHOLD = $10;
     TCP_ABORT_THRESHOLD = $11;
     TCP_CONN_NOTIFY_THRESHOLD = $12;
     TCP_CONN_ABORT_THRESHOLD = $13;
     TCP_RECVDSTADDR = $14;
     TCP_INIT_CWND = $15;
     TCP_KEEPALIVE_THRESHOLD = $16;
     TCP_KEEPALIVE_ABORT_THRESHOLD = $17;
     TCP_CORK = $18;

//from /usr/include/netinet/in.h

    IPPROTO_IP = 0;    
  { Hop by hop header for IPv6  }
    IPPROTO_HOPOPTS = 0;    
  { control message protocol  }
    IPPROTO_ICMP = 1;    
  { group control protocol  }
    IPPROTO_IGMP = 2;    
  { gateway^2 (deprecated)  }
    IPPROTO_GGP = 3;    
  { IP in IP encapsulation  }
    IPPROTO_ENCAP = 4;    
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
  { IPv6 encapsulated in IP  }
    IPPROTO_IPV6 = 41;    
  { Routing header for IPv6  }
    IPPROTO_ROUTING = 43;    
  { Fragment header for IPv6  }
    IPPROTO_FRAGMENT = 44;    
  { rsvp  }
    IPPROTO_RSVP = 46;    
  { IPsec Encap. Sec. Payload  }
    IPPROTO_ESP = 50;    
  { IPsec Authentication Hdr.  }
    IPPROTO_AH = 51;    
  { ICMP for IPv6  }
    IPPROTO_ICMPV6 = 58;    
  { No next header for IPv6  }
    IPPROTO_NONE = 59;    
  { Destination options  }
    IPPROTO_DSTOPTS = 60;    
  { "hello" routing protocol  }
    IPPROTO_HELLO = 63;    
  { UNOFFICIAL net disk proto  }
    IPPROTO_ND = 77;    
  { ISO clnp  }
    IPPROTO_EON = 80;    
  { OSPF  }
    IPPROTO_OSPF = 89;    
  { PIM routing protocol  }
    IPPROTO_PIM = 103;    
  { Stream Control  }
    IPPROTO_SCTP = 132;    
  { Transmission Protocol  }
  { raw IP packet  }
    IPPROTO_RAW = 255;    
    IPPROTO_MAX = 256;    

    
  { set/get IP per-packet options    }
    IP_OPTIONS = 1;    
  { int; header is included with data (raw)  }
    IP_HDRINCL = 2;    
  { int; IP type of service and precedence  }
    IP_TOS = 3;    

  { int; IP time to live  }
    IP_TTL = 4;    
  { int; receive all IP options w/datagram  }
    IP_RECVOPTS = $5;    
  { int; receive IP options for response  }
    IP_RECVRETOPTS = $6;    
  { int; receive IP dst addr w/datagram  }
    IP_RECVDSTADDR = $7;    
  { ip_opts; set/get IP per-packet options  }
    IP_RETOPTS = $8;    
  { int; receive the inbound interface index  }
    IP_RECVIF = $9;    
  { sockaddr_dl; get source link layer address  }
    IP_RECVSLLA = $a;    
  { uint8_t; get TTL for inbound packet  }
    IP_RECVTTL = $b;    
  { set/get IP multicast interface   }
    IP_MULTICAST_IF = $10;    
  { set/get IP multicast timetolive  }
    IP_MULTICAST_TTL = $11;    
  { set/get IP multicast loopback    }
    IP_MULTICAST_LOOP = $12;    
  { add	an IP group membership	    }
    IP_ADD_MEMBERSHIP = $13;    
  { drop an IP group membership	    }
    IP_DROP_MEMBERSHIP = $14;    
  { block   mcast pkts from source   }
    IP_BLOCK_SOURCE = $15;    
  { unblock mcast pkts from source   }
    IP_UNBLOCK_SOURCE = $16;    
  { add  mcast group/source pair	    }
    IP_ADD_SOURCE_MEMBERSHIP = $17;    
  { drop mcast group/source pair	    }
    IP_DROP_SOURCE_MEMBERSHIP = $18;    
  { send directly to next hop	    }
    IP_NEXTHOP = $19;    
  {
   * IP_PKTINFO and IP_RECVPKTINFO have same value. Size of argument passed in
   * is used to differentiate b/w the two.
    }
  { specify src address and/or index  }
    IP_PKTINFO = $1a;    
  { recv dest/matched addr and index  }
    IP_RECVPKTINFO = $1a;    
    {
     * IPv6 options
      }
    { hop limit value for unicast  }
      IPV6_UNICAST_HOPS = $5;      
    { packets.  }
    { argument type: uint_t  }
    { outgoing interface for  }
      IPV6_MULTICAST_IF = $6;      
    { multicast packets.  }
    { argument type: struct in6_addr  }
    { hop limit value to use for  }
      IPV6_MULTICAST_HOPS = $7;      
    { multicast packets.  }
    { argument type: uint_t  }
    { enable/disable delivery of  }
      IPV6_MULTICAST_LOOP = $8;      
    { multicast packets on same socket.  }
    { argument type: uint_t  }
    { join an IPv6 multicast group.  }
      IPV6_JOIN_GROUP = $9;      
    { argument type: struct ipv6_mreq  }
    { leave an IPv6 multicast group  }
      IPV6_LEAVE_GROUP = $a;      
    { argument type: struct ipv6_mreq  }
    {
     * IPV6_ADD_MEMBERSHIP and IPV6_DROP_MEMBERSHIP are being kept
     * for backward compatibility. They have the same meaning as IPV6_JOIN_GROUP
     * and IPV6_LEAVE_GROUP respectively.
      }
    { join an IPv6 multicast group.  }
      IPV6_ADD_MEMBERSHIP = $9;      
    { argument type: struct ipv6_mreq  }
    { leave an IPv6 multicast group  }
      IPV6_DROP_MEMBERSHIP = $a;      
    { argument type: struct ipv6_mreq  }
    { addr plus interface index  }
      IPV6_PKTINFO = $b;      
    { arg type: "struct in6_pktingo" -  }
    { hoplimit for datagram  }
      IPV6_HOPLIMIT = $c;      
    { next hop address   }
      IPV6_NEXTHOP = $d;      
    { hop by hop options  }
      IPV6_HOPOPTS = $e;      
    { destination options - after  }
      IPV6_DSTOPTS = $f;      
    { the routing header  }
    { routing header   }
      IPV6_RTHDR = $10;      
    { destination options - before  }
      IPV6_RTHDRDSTOPTS = $11;      
    { the routing header  }
    { enable/disable IPV6_PKTINFO  }
      IPV6_RECVPKTINFO = $12;      
    { enable/disable IPV6_HOPLIMIT  }
      IPV6_RECVHOPLIMIT = $13;      
    { enable/disable IPV6_HOPOPTS  }
      IPV6_RECVHOPOPTS = $14;      
    {
     * This options exists for backwards compatability and should no longer be
     * used.  Use IPV6_RECVDSTOPTS instead.
      }
      _OLD_IPV6_RECVDSTOPTS = $15;      
    { enable/disable IPV6_RTHDR  }
      IPV6_RECVRTHDR = $16;      
    {
     * enable/disable IPV6_RTHDRDSTOPTS.  Now obsolete.  IPV6_RECVDSTOPTS enables
     * the receipt of both headers.
      }
      IPV6_RECVRTHDRDSTOPTS = $17;      
    { Control checksum on raw sockets  }
      IPV6_CHECKSUM = $18;      
    { enable/disable IPV6_CLASS  }
      IPV6_RECVTCLASS = $19;      
    { send packets with minimum MTU  }
      IPV6_USE_MIN_MTU = $20;      
    { don't fragment packets  }
      IPV6_DONTFRAG = $21;      
    { Used to set IPSEC options  }
      IPV6_SEC_OPT = $22;      
    { Control socket's src addr select  }
      IPV6_SRC_PREFERENCES = $23;      
    { receive PMTU info  }
      IPV6_RECVPATHMTU = $24;      
    { get the PMTU  }
      IPV6_PATHMTU = $25;      
    { traffic class  }
      IPV6_TCLASS = $26;      
    { v6 only socket option  }
      IPV6_V6ONLY = $27;      
    {
     * enable/disable receipt of both both IPV6_DSTOPTS headers.
      }
      IPV6_RECVDSTOPTS = $28;      
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
