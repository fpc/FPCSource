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

{$ifdef linux}
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
{$endif}
const
  { Two constants to determine whether part of soket is for in or output }
  S_IN = 0;
  S_OUT = 1;

Type
  TSockAddr = packed Record
    family:word;  { was byte, fixed }
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
Function Send(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint;
Function Recv(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint;
Function Bind(Sock:Longint;Var Addr;AddrLen:Longint):Boolean;
Function Listen (Sock,MaxConnect:Longint):Boolean;
Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function Connect(Sock:Longint;Var Addr;Addrlen:Longint):boolean;
Function Shutdown(Sock:Longint;How:Longint):Longint;
Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function SetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
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
  Revision 1.6  2000-06-02 17:30:43  marco
   * added some constants for getsocketoptions under a linux define.
  	Allows server example to work ok.

  Revision 1.5  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.4  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.3  2000/01/07 16:32:25  daniel
    * copyright 2000 added

}
