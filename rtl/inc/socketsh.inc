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
Const
  { Socket Types }
  SOCK_STREAM     = 1;               { stream (connection) socket   }
  SOCK_DGRAM      = 2;               { datagram (conn.less) socket  }
  SOCK_RAW        = 3;               { raw socket                   }
  SOCK_RDM        = 4;               { reliably-delivered message   }
  SOCK_SEQPACKET  = 5;               { sequential packet socket     }

  AF_UNSPEC       = 0;
  AF_UNIX         = 1;      { Unix domain sockets          }
  AF_INET         = 2;      { Internet IP Protocol         }

  {  Protocol Families }
  PF_UNSPEC       = AF_UNSPEC;
  PF_UNIX         = AF_UNIX;
  PF_INET         = AF_INET;

{$Ifdef Unix}
{$ifndef BSD}
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
{$ELSE}
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

{$endif}
{$ENDIF}

const
  { Two constants to determine whether part of soket is for in or output }
  S_IN = 0;
  S_OUT = 1;

Type
  TSockAddr = packed Record
    {$ifdef BSD}
     len : byte;
     family:byte;
    {$ELSE}
     family:word;  { was byte, fixed }
    {$ENDIF}
    data  :array [0..13] of char;
    end;

  TInetSockAddr = packed Record
    family:Word;
    port  :Word;
    addr  :Cardinal;
    pad   :array [1..8] of byte; { to get to the size of sockaddr... }
    end;

  TSockArray = Array[1..2] of Longint;

Var
  SocketError:Longint;

{Basic Socket Functions}
Function Socket(Domain,SocketType,Protocol:Longint):Longint;
Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
Function SendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint;Var Addr; AddrLen : Longint):Longint;
Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
Function RecvFrom(Sock : Longint; Var Buf; Buflen,Flags : Longint; Var Addr; AddrLen : Integer) : longint;
Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;
Function Listen (Sock,MaxConnect:Longint):Boolean;
Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function Connect(Sock:Longint;Const Addr;Addrlen:Longint):boolean;
Function Shutdown(Sock:Longint;How:Longint):Longint;
Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;
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

{
  $Log$
  Revision 1.8  2002-09-07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.7  2002/02/04 21:29:34  michael
  + merged missing sendto/rcvfrom functions

}
