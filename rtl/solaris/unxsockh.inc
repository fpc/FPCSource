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
   AF_IMPLINK    = 3;               { arpanet imp addresses }
   AF_PUP        = 4;               { pup protocols: e.g. BSP }
   AF_CHAOS      = 5;               { mit CHAOS protocols }
   AF_NS         = 6;               { XEROX NS protocols }
   AF_NBS        = 7;               { nbs protocols }
   AF_ECMA       = 8;               { european computer manufacturers }
   AF_DATAKIT    = 9;               { datakit protocols }
   AF_CCITT      = 10;              { CCITT protocols, X.25 etc }
   AF_SNA        = 11;              { IBM SNA }
   AF_DECnet     = 12;              { DECnet }
   AF_DLI        = 13;              { Direct data link interface }
   AF_LAT        = 14;              { LAT }
   AF_HYLINK     = 15;              { NSC Hyperchannel }
   AF_APPLETALK  = 16;              { Apple Talk }
   AF_NIT        = 17;              { Network Interface Tap }
   AF_802        = 18;              { IEEE 802.2, also ISO 8802 }
   AF_OSI        = 19;              { umbrella for all families used }
   AF_X25        = 20;              { CCITT X.25 in particular }
   AF_OSINET     = 21;              { AFI = 47, IDI = 4 }
   AF_GOSIP      = 22;              { U.S. Government OSI }
   AF_IPX        = 23;              { Novell Internet Protocol }
   AF_ROUTE      = 24;              { Internal Routing Protocol }
   AF_LINK       = 25;              { Link-layer interface }
   AF_INET6      = 26;              { Internet Protocol, Version 6 }
   AF_KEY        = 27;              { Security Association DB socket }
   AF_NCA        = 28;              { NCA socket }

   AF_MAX        = 28;

   PF_IMPLINK    = AF_IMPLINK;
   PF_PUP        = AF_PUP;
   PF_CHAOS      = AF_CHAOS;
   PF_NS         = AF_NS;
   PF_NBS        = AF_NBS;
   PF_ECMA       = AF_ECMA;
   PF_DATAKIT    = AF_DATAKIT;
   PF_CCITT      = AF_CCITT;
   PF_SNA        = AF_SNA;
   PF_DECnet     = AF_DECnet;
   PF_DLI        = AF_DLI;
   PF_LAT        = AF_LAT;
   PF_HYLINK     = AF_HYLINK;
   PF_APPLETALK  = AF_APPLETALK;
   PF_NIT        = AF_NIT;
   PF_802        = AF_802;
   PF_OSI        = AF_OSI;
   PF_X25        = AF_X25;
   PF_OSINET     = AF_OSINET;
   PF_GOSIP      = AF_GOSIP;
   PF_IPX        = AF_IPX;
   PF_ROUTE      = AF_ROUTE;
   PF_LINK       = AF_LINK;
   PF_INET6      = AF_INET6;
   PF_KEY        = AF_KEY;
   PF_NCA        = AF_NCA;

   PF_MAX        = AF_MAX;

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




