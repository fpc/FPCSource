{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   socket call implementations for FreeBSD

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

Const
//  AF_LOCAL        =1;              { local to host (pipes, portals) }
  AF_IMPLINK      =3;               { arpanet imp addresses }
  AF_PUP          =4;              { pup protocols: e.g. BSP }
  AF_CHAOS        =5;               { mit CHAOS protocols }
  AF_NS           =6;              { XEROX NS protocols }
  AF_ISO          =7;              { ISO protocols }
  AF_OSI          =AF_ISO;
  AF_ECMA         =8;              { European computer manufacturers }
  AF_DATAKIT      =9;              { datakit protocols }
  AF_CCITT        =10;             { CCITT protocols, X.25 etc }
  AF_SNA          =11;             { IBM SNA }
  AF_DECnet       =12;             { DECnet }
  AF_DLI          =13;             { DEC Direct data link interface }
  AF_LAT          =14;             { LAT }
  AF_HYLINK       =15;             { NSC Hyperchannel }
  AF_APPLETALK    =16;             { Apple Talk }
  AF_ROUTE        =17;             { Internal Routing Protocol }
  AF_LINK         =18;             { Link layer interface }
  pseudo_AF_XTP   =19;             { eXpress Transfer Protocol (no AF) }
  AF_COIP         =20;             { connection-oriented IP, aka ST II }
  AF_CNT          =21;             { Computer Network Technology }
  pseudo_AF_RTIP  =22;             { Help Identify RTIP packets }
  AF_IPX          =23;             { Novell Internet Protocol }
  AF_SIP          =24;             { Simple Internet Protocol }
  pseudo_AF_PIP   =25;             { Help Identify PIP packets }
  AF_ISDN         =26;             { Integrated Services Digital Network}
  AF_E164         =AF_ISDN;        { CCITT E.164 recommendation }
  pseudo_AF_KEY   =27;             { Internal key-management function }
  AF_INET6        =28;             { IPv6 }
  AF_NATM         =29;             { native ATM access }
  AF_ATM          =30;             { ATM }
  pseudo_AF_HDRCMPLT=31;           { Used by BPF to not rewrite headers
                                    in interface output routine}
  AF_NETGRAPH     =32;             { Netgraph sockets }
  AF_MAX          =33;

  SOCK_MAXADDRLEN =255;             { longest possible addresses }

{
* Protocol families, same as address families for now.
}
//  PF_LOCAL        =AF_LOCAL;
  PF_IMPLINK      =AF_IMPLINK;
  PF_PUP          =AF_PUP;
  PF_CHAOS        =AF_CHAOS;
  PF_NS           =AF_NS;
  PF_ISO          =AF_ISO;
  PF_OSI          =AF_ISO;
  PF_ECMA         =AF_ECMA;
  PF_DATAKIT      =AF_DATAKIT;
  PF_CCITT        =AF_CCITT;
  PF_SNA          =AF_SNA;
  PF_DECnet       =AF_DECnet;
  PF_DLI          =AF_DLI;
  PF_LAT          =AF_LAT;
  PF_HYLINK       =AF_HYLINK;
  PF_APPLETALK    =AF_APPLETALK;
  PF_ROUTE        =AF_ROUTE;
  PF_LINK         =AF_LINK;
  PF_XTP          =pseudo_AF_XTP;  { really just proto family, no AF }
  PF_COIP         =AF_COIP;
  PF_CNT          =AF_CNT;
  PF_SIP          =AF_SIP;
  PF_IPX          =AF_IPX;         { same format as AF_NS }
  PF_RTIP         =pseudo_AF_RTIP; { same format as AF_INET }
  PF_PIP          =pseudo_AF_PIP;
  PF_ISDN         =AF_ISDN;
  PF_KEY          =pseudo_AF_KEY;
  PF_INET6        =AF_INET6;
  PF_NATM         =AF_NATM;
  PF_ATM          =AF_ATM;
  PF_NETGRAPH     =AF_NETGRAPH;
  PF_MAX          =AF_MAX;

{ For setsockoptions(2) }
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
{ To add :         SO_REUSEPORT 15 }
         SO_PASSCRED=    16;
         SO_PEERCRED=    17;
         SO_RCVLOWAT=    18;
         SO_SNDLOWAT=    19;
         SO_RCVTIMEO=    20;
         SO_SNDTIMEO=    21;

{ Security levels - as per NRL IPv6 - don't actually do anything }

         SO_SECURITY_AUTHENTICATION      =   22;
         SO_SECURITY_ENCRYPTION_TRANSPORT=   23;
         SO_SECURITY_ENCRYPTION_NETWORK  =   24;

         SO_BINDTODEVICE=   25;

{ Socket filtering }

         SO_ATTACH_FILTER=  26;
         SO_DETACH_FILTER=  27;
         SO_PEERNAME     =  28;
         SO_TIMESTAMP    = 29;
         SCM_TIMESTAMP   = SO_TIMESTAMP;
         SO_ACCEPTCONN   = 30;


        SHUT_RD         =0;             { shut down the reading side }
        SHUT_WR         =1;             { shut down the writing side }
        SHUT_RDWR       =2;             { shut down both sides }

