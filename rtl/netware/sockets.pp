{ Netware:UNTESTED !!
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Sockets;
Interface

const
 {$Ifndef BSD}
  { Adress families, Linux specific }
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
 {$ELSE}
 {BSD}
  AF_LOCAL        =1;              { local to host (pipes, portals) }
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
  PF_LOCAL        =AF_LOCAL;
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
{$ENDIF}

type
  TUnixSockAddr = packed Record
    family:word; { was byte, fixed }
    path:array[0..108] of char;
    end;

{$i socketsh.inc}

{ unix socket specific functions }
Procedure Str2UnixSockAddr(const addr:string;var t:TUnixSockAddr;var len:longint);
Function Bind(Sock:longint;const addr:string):boolean;
Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:text):Boolean;
Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:file):Boolean;
Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:text):Boolean;
Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:File):Boolean;

Implementation
{$ifndef netware}
Uses Unix;
{$endif}

{ Include filerec and textrec structures }
{$i filerec.inc}
{$i textrec.inc}
{******************************************************************************
                          Kernel Socket Callings
******************************************************************************}

{$ifdef BSD}
 {$I bsdsock.inc}
{$else}
 {$ifdef netware}
   {$I nwsock.inc}
 {$else}
   {$I linsock.inc}
 {$endif}
{$endif}

{$i sockets.inc}

end.

{
  $Log$
  Revision 1.2  2002-09-07 16:01:21  peter
    * old logs removed and tabs fixed

}
