{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF ver1_0}
  {$IFNDEF ver1_9_4}
    {$INLINE ON}
    {$define HASINLINE}
  {$endif}  
{$ENDIF}

Type
{$ifdef SOCK_HAS_SINLEN}
    sa_family_t=cuchar;
{$else}
    sa_family_t=cushort;
{$endif} 

Const
  { Socket types }
  SOCK_STREAM     = 1;               { stream (connection) socket   }
  SOCK_DGRAM      = 2;               { datagram (conn.less) socket  }
  SOCK_RAW        = 3;               { raw socket                   }
  SOCK_RDM        = 4;               { reliably-delivered message   }
  SOCK_SEQPACKET  = 5;               { sequential packet socket     }

  { Protocol families }
  PF_UNSPEC    = 0;                  { Unspecified }
  PF_LOCAL     = 1;                  { Local to host (pipes and file-domain) }
  PF_UNIX      = PF_LOCAL;           { Old BSD name for PF_LOCAL }
  PF_FILE      = PF_LOCAL;           { Another non-standard name for PF_LOCAL }
  PF_INET      = 2;                  { IP protocol family }

  { Address families }
  AF_UNSPEC    = PF_UNSPEC;
  AF_LOCAL     = PF_LOCAL;
  AF_UNIX      = PF_UNIX;
  AF_FILE      = PF_FILE;
  AF_INET      = PF_INET;

  { Flags for send, recv etc. }
  MSG_OOB      = $0001;              { Process out-of-band data}
  MSG_PEEK     = $0002;              { Peek at incoming messages }
  MSG_DONTROUTE= $0004;              { Don't use local routing }
  MSG_TRYHARD  = MSG_DONTROUTE;
  MSG_CTRUNC   = $0008;              { Control data lost before delivery }
  MSG_PROXY    = $0010;              { Supply or ask second address }
  MSG_TRUNC    = $0020;
  MSG_DONTWAIT = $0040;              { Non-blocking I/O }
  MSG_EOR      = $0080;              { End of record }
  MSG_WAITALL  = $0100;              { Wait for a full request }
  MSG_FIN      = $0200;
  MSG_SYN      = $0400;
  MSG_CONFIRM  = $0800;              { Confirm path validity }
  MSG_RST      = $1000;
  MSG_ERRQUERE = $2000;              { Fetch message from error queue }
  MSG_NOSIGNAL = $4000;              { Do not generate SIGPIPE }
  MSG_MORE     = $8000;              { Sender will send more }

const
  { Two constants to determine whether part of soket is for in or output }
  S_IN = 0;
  S_OUT = 1;

Type
  in_addr = packed record
	     s_addr  : cuint32;		// inaddr_t=cuint32
            end; 

  TSockAddr = packed Record
  {$ifdef SOCK_HAS_SINLEN}
     sa_len     : cuchar;
  {$endif}
     sa_family  : sa_family_t;
     sa_data    : array [0..13] of char;
    end;
  pSockAddr = ^TSockAddr;

TInetSockAddr = packed Record
    case boolean of   
     false : (
  {$ifdef SOCK_HAS_SINLEN} 
     sin_len     : cuchar;
  {$endif}
     sin_family  : sa_family_t;
     sin_port    : cushort;   
     sin_addr    : in_addr;
     xpad         : array [0..7] of char; { to get to the size of sockaddr... }
      );
   true: (
  {$ifdef SOCK_HAS_SINLEN}
     len     : cuchar;
  {$endif}
     family  : sa_family_t;
     port    : cushort;
     addr    : cardinal;
     pad         : array [0..7] of char; { to get to the size of sockaddr... }
      );                
    end;
  pInetSockAddr = ^TInetSockAddr;

  Tin6_addr = packed record
  case byte of
    0: (u6_addr8  : array[0..15] of byte);
    1: (u6_addr16 : array[0..7] of Word);
    2: (u6_addr32 : array[0..3] of Cardinal);
    3: (s6_addr8  : array[0..15] of shortint);
    4: (s6_addr   : array[0..15] of shortint);
    5: (s6_addr16 : array[0..7] of smallint);
    6: (s6_addr32 : array[0..3] of LongInt);
  end;
  pIn6_Addr=^TIn6_addr;

  TInetSockAddr6 = packed Record
    {$ifdef SOCKET_HAS_SINLEN}  // as per RFC 2553
      sin6_len    : byte;
    {$endif}
    sin6_family   : sa_family_t;
    sin6_port     : cuint16;
    sin6_flowinfo : cuint32;
    sin6_addr     : Tin6_addr;
    sin6_scope_id : cuint32;
  end;

  sockaddr_in6 = TInetSockAddr6;
  psockaddr_in6 = ^sockaddr_in6;

  TSockPairArray = Array[0..1] of Longint;
  TSockArray     = Array[1..2] of Longint; 		//legacy

Var
  SocketError:cint;

{$ifndef FPC_USE_LIBC}
function  fpsocket      (domain:cint; xtype:cint; protocol: cint):cint; 
function  fprecv        (s:cint; buf: pointer; len: size_t; flags: cint):ssize_t; 
function  fprecvfrom    (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t; 
function  fpsend        (s:cint; msg:pointer; len:size_t; flags:cint):ssize_t; 
function  fpsendto      (s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t; 
function  fpbind        (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;  
function  fplisten      (s:cint; backlog : cint):cint;                          
function  fpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint; 
function  fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;  
function  fpshutdown    (s:cint; how:cint):cint; 
function  fpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint; 
function  fpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint; 
function  fpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint; 
function  fpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : tsocklen):cint; 
function  fpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint; 
{$else}
function  fpsocket      (domain:cint; xtype:cint; protocol: cint):cint; cdecl; external;
function  fprecv        (s:cint; buf: pointer; len: size_t; flags: cint):ssize_t; cdecl; external;
function  fprecvfrom    (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t; cdecl; external;
function  fpsend        (s:cint; msg:pointer; len:size_t; flags:cint):ssize_t; cdecl; external;
function  fpsendto      (s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t; cdecl; external;
function  fpbind        (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;  cdecl; external;
function  fplisten      (s:cint; backlog : cint):cint;                          cdecl; external;
function  fpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint; cdecl; external;
function  fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;  cdecl; external;
function  fpshutdown    (s:cint; how:cint):cint; cdecl; external;
function  fpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint; cdecl; external;
function  fpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint; cdecl; external;
function  fpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint; cdecl; external;
function  fpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : tsocklen):cint; cdecl; external;
function  fpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint; cdecl; external;
{$endif}

{Basic Socket Functions}
Function Socket(Domain,SocketType,Protocol:Longint):Longint;
Function CloseSocket(Sock:Longint):Longint;
Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
Function SendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint;Var Addr; AddrLen : Longint):Longint;
Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
Function RecvFrom(Sock : Longint; Var Buf; Buflen,Flags : Longint; Var Addr; var AddrLen : longInt) : longint;
Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;
Function Listen (Sock,MaxConnect:Longint):Boolean;
Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function Connect(Sock:Longint;Const Addr;Addrlen:Longint):boolean;
Function Shutdown(Sock:Longint;How:Longint):Longint;
Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function SetSocketOptions(Sock,Level,OptName:Longint;const OptVal;optlen:longint):Longint;
Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;

{Text Support}
Procedure Sock2Text(Sock:Longint;Var SockIn,SockOut:Text);

{Untyped File Support}
Procedure Sock2File(Sock:Longint;Var SockIn,SockOut:File);

{Better Pascal Calling, Overloaded Functions!}
Function Accept(Sock:longint;var addr:TInetSockAddr;var SockIn,SockOut:File):Boolean;
Function Accept(Sock:longint;var addr:TInetSockAddr;var SockIn,SockOut:text):Boolean;
Function Connect(Sock:longint;const addr:TInetSockAddr;var SockIn,SockOut:text):Boolean;
Function Connect(Sock:longint;const addr:TInetSockAddr;var SockIn,SockOut:file):Boolean;

function htonl( host : longint):longint;  {$ifdef HASINLINE} inline; {$endif}
Function NToHl (Net : Longint) : Longint; {$ifdef HASINLINE} inline; {$endif}
function htons( host : word):word;	  {$ifdef HASINLINE} inline; {$endif}
Function NToHs (Net : word):word;         {$ifdef HASINLINE} inline; {$endif}

{
  $Log$
  Revision 1.20  2004-12-21 14:36:18  marco
   * more maybelibc removal

  Revision 1.19  2004/12/21 09:48:14  michael
  + Removed maybelibc macro

  Revision 1.18  2004/11/01 19:39:19  peter
    * disable inline for 1.9.4

  Revision 1.17  2004/11/01 17:35:22  marco
   * typo fixed, aarght

  Revision 1.16  2004/11/01 17:29:47  marco
   * inline problems fixed

  Revision 1.15  2004/11/01 16:23:15  marco
   * htons etc

  Revision 1.14  2004/03/16 19:15:57  marco
   * sockaddr is now a union between the old and new struct as grace period

  Revision 1.13  2004/03/16 18:03:37  marco
   * first changes sockets units

  Revision 1.12  2003/11/23 11:00:07  michael
  + Added IPV6 patch from Johannes Berg

  Revision 1.11  2003/11/22 16:28:56  michael
  + Added several constants

  Revision 1.10  2003/11/09 21:43:15  michael
  + Added some missing socket options and the shut_* constants

  Revision 1.9  2003/03/23 17:47:15  armin
  * CloseSocket added

  Revision 1.8  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.7  2002/02/04 21:29:34  michael
  + merged missing sendto/rcvfrom functions

}
