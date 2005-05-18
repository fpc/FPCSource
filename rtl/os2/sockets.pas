{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 Yuri Prokushev

    Sockets implementation for OS/2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE ObjFPC}
unit Sockets;

Interface

Uses
  so32dll;

Const
  AF_LOCAL       = so32dll.AF_LOCAL;
  AF_OS2         = so32dll.AF_OS2;
  AF_IMPLINK     = so32dll.AF_IMPLINK;     // arpanet imp addresses
  AF_PUP         = so32dll.AF_PUP;         // pup protocols: e.g. BSP
  AF_CHAOS       = so32dll.AF_CHAOS;       // mit CHAOS protocols
  AF_NS          = so32dll.AF_NS;          // XEROX NS protocols
  AF_ISO         = so32dll.AF_ISO;         // ISO protocols
  AF_OSI         = so32dll.AF_OSI;
  AF_ECMA        = so32dll.AF_ECMA;        // european computer manufacturers
  AF_DATAKIT     = so32dll.AF_DATAKIT;     // datakit protocols
  AF_CCITT       = so32dll.AF_CCITT;       // CCITT protocols, X.25 etc
  AF_SNA         = so32dll.AF_SNA;         // IBM SNA
  AF_DECnet      = so32dll.AF_DECnet;      // DECnet
  AF_DLI         = so32dll.AF_DLI;         // DEC Direct data link interface
  AF_LAT         = so32dll.AF_LAT;         // LAT
  AF_HYLINK      = so32dll.AF_HYLINK;      // NSC Hyperchannel
  AF_APPLETALK   = so32dll.AF_APPLETALK;   // Apple Talk
  AF_NB          = so32dll.AF_NB;          // Netbios
  AF_NETBIOS     = so32dll.AF_NETBIOS;     // Netbios
  AF_LINK        = so32dll.AF_LINK;        // Link layer interface
  pseudo_AF_XTP  = so32dll.pseudo_AF_XTP;  // eXpress Transfer Protocol (no AF)
  AF_COIP        = so32dll.AF_COIP;        // connection-oriented IP, aka ST II
  AF_CNT         = so32dll.AF_CNT;         // Computer Network Technology
  pseudo_AF_RTIP = so32dll.pseudo_AF_RTIP; // Help Identify RTIP packets
  AF_IPX         = so32dll.AF_IPX;         // Novell Internet Protocol
  AF_SIP         = so32dll.AF_SIP;         // Simple Internet Protocol
  AF_INET6       = so32dll.AF_INET6;
  pseudo_AF_PIP  = so32dll.pseudo_AF_PIP;  // Help Identify PIP packets
  AF_ROUTE       = so32dll.AF_ROUTE;       // Internal Routing Protocol
  AF_FWIP        = so32dll.AF_FWIP;        // firewall support
  AF_IPSEC       = so32dll.AF_IPSEC;       // IPSEC and encryption techniques
  AF_DES         = so32dll.AF_DES;         // DES
  AF_MD5         = so32dll.AF_MD5;
  AF_CDMF        = so32dll.AF_CDMF;

  AF_MAX         = so32dll.AF_MAX;

  PF_LOCAL     = so32dll.PF_LOCAL;
  PF_OS2       = so32dll.PF_OS2;
  PF_IMPLINK   = so32dll.PF_IMPLINK;
  PF_PUP       = so32dll.PF_PUP;
  PF_CHAOS     = so32dll.PF_CHAOS;
  PF_NS        = so32dll.PF_NS;
  PF_ISO       = so32dll.PF_ISO;
  PF_OSI       = so32dll.PF_OSI;
  PF_ECMA      = so32dll.PF_ECMA;
  PF_DATAKIT   = so32dll.PF_DATAKIT;
  PF_CCITT     = so32dll.PF_CCITT;
  PF_SNA       = so32dll.PF_SNA;
  PF_DECnet    = so32dll.PF_DECnet;
  PF_DLI       = so32dll.PF_DLI;
  PF_LAT       = so32dll.PF_LAT;
  PF_HYLINK    = so32dll.PF_HYLINK;
  PF_APPLETALK = so32dll.PF_APPLETALK;
  PF_NETBIOS   = so32dll.PF_NB;
  PF_NB        = so32dll.PF_NB;
  PF_ROUTE     = so32dll.PF_ROUTE;
  PF_LINK      = so32dll.PF_LINK;
  PF_XTP       = so32dll.PF_XTP;  // really just proto family, no AF
  PF_COIP      = so32dll.PF_COIP;
  PF_CNT       = so32dll.PF_CNT;
  PF_SIP       = so32dll.PF_SIP;
  PF_INET6     = so32dll.PF_INET6;
  PF_IPX       = so32dll.PF_IPX;     // same format as AF_NS
  PF_RTIP      = so32dll.PF_RTIP;    // same format as AF_INET
  PF_PIP       = so32dll.PF_PIP;

  PF_MAX       = so32dll.PF_MAX;


// OS/2 stack based on BSD stack
{$DEFINE BSD}
{$I socketsh.inc}

Implementation

{Include filerec and textrec structures}
{$I filerec.inc}
{$I textrec.inc}

{******************************************************************************
                          Basic Socket Functions
******************************************************************************}

Function socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  Socket:=so32dll.Socket(Domain,SocketType,ProtoCol);
  if Socket<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
begin
  Send:=so32dll.Send(Sock,Buf,BufLen,Flags);
  if Send<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function SendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint;Var Addr; AddrLen : Longint):Longint;
begin
  SendTo:=so32dll.SendTo(Sock,Buf,BufLen,Flags,so32dll.SockAddr(Addr),AddrLen);
  if SendTo<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
begin
  Recv:=so32dll.Recv(Sock,Buf,BufLen,Flags);
  if Recv<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;


Function RecvFrom(Sock : Longint; Var Buf; Buflen,Flags : Longint; Var Addr; AddrLen : Integer) : longint;

begin
  RecvFrom:=so32dll.RecvFrom(Sock,Buf,BufLen,Flags,so32dll.SockAddr(Addr),AddrLen);
  if RecvFrom<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;
var
  l : longint;
begin
  l:=so32dll.Bind(Sock,so32dll.sockaddr(Addr),AddrLen);
  if l<0 then
    begin
       SocketError:=so32dll.sock_errno;
       Bind:=false;
    end
  else
    begin
       SocketError:=0;
       Bind:=true;
    end;
end;

Function Listen(Sock,MaxConnect:Longint):Boolean;
var
  l : longint;
begin
  l:=so32dll.Listen(Sock,MaxConnect);
  if l<0 then
    begin
       SocketError:=so32dll.sock_errno;
       Listen:=false;
    end
  else
    begin
       SocketError:=0;
       Listen:=true;
    end;
end;

Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  Accept:=so32dll.Accept(Sock,so32dll.SockAddr(Addr), AddrLen);
  if Accept<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function Connect(Sock:Longint;Const Addr;Addrlen:Longint):Boolean;
begin
  Connect:=so32dll.Connect(Sock,so32dll.SockAddr(Addr),AddrLen)=0;
  if not Connect then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function Shutdown(Sock:Longint;How:Longint):Longint;
begin
  ShutDown:=so32dll.ShutDown(Sock,How);
  if ShutDown<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetSocketName:=so32dll.GetSockName(Sock, so32dll.SockAddr(Addr),AddrLen);
  if GetSocketName<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetPeerName:=so32dll.GetPeerName(Sock,so32dll.SockAddr(Addr),AddrLen);
  if GetPeerName<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;
begin
  SetSocketOptions:=so32dll.SetSockOpt(Sock,Level,OptName,OptVal,OptLen);
  if SetSocketOptions<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
begin
  GetSocketOptions:=so32dll.GetSockOpt(Sock,Level,OptName,OptVal,OptLen);
  if GetSocketOptions<0 then
    SocketError:=so32dll.sock_errno
  else
    SocketError:=0;
end;

Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin

{!!TODO!!
  SocketPair:=so32dll.socketpair(Domain,SocketType,Protocol,Pair);}
  //SocketCall(Socket_Sys_SocketPair,Domain,SocketType,Protocol,longint(@Pair),0,0);
end;

{ mimic the linux fdWrite/fdRead calls for the file/text socket wrapper }
function fdWrite(handle : longint;Const bufptr;size : dword) : dword;
begin
  fdWrite := so32dll.send(handle, bufptr, size, 0);
  if fdWrite = -1 then
  begin
    SocketError := so32dll.sock_errno;
    fdWrite := 0;
  end
  else
    SocketError := 0;
end;

function fdRead(handle : longint;var bufptr;size : dword) : dword;
var
  d : dword;
begin
{!!TODO!!
  if so32dll.ioctlsocket(handle,FIONREAD,@d) = -1 then
  begin
    SocketError:=so32dll.sock_errno;
    fdRead:=0;
    exit;
  end;
}
  if d>0 then
  begin
    if size>d then
    size:=d;
    fdRead := so32dll.recv(handle, bufptr, size, 0);
    if fdRead = -1 then
    begin
      SocketError:= so32dll.sock_errno;
      fdRead := 0;
    end else
      SocketError:=0;
  end
  else
    SocketError:=0;
end;

{$i sockets.inc}

Begin
  so32dll.sock_init;
End.
