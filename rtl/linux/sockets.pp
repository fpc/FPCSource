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
unit Sockets;
Interface

const
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
Uses Linux;

{ Include filerec and textrec structures }
{$i filerec.inc}
{$i textrec.inc}

{******************************************************************************
                          Kernel Socket Callings
******************************************************************************}

Const
  {
    Arguments to the Linux Kernel system call for sockets. All
    Socket Connected calls go through the same system call,
    with an extra argument to determine what action to take.
  }
  Socket_Sys_SOCKET      = 1;
  Socket_Sys_BIND        = 2;
  Socket_Sys_CONNECT     = 3;
  Socket_Sys_LISTEN      = 4;
  Socket_Sys_ACCEPT      = 5;
  Socket_Sys_GETSOCKNAME = 6;
  Socket_Sys_GETPEERNAME = 7;
  Socket_Sys_SOCKETPAIR  = 8;
  Socket_Sys_SEND        = 9;
  Socket_Sys_RECV        = 10;
  Socket_Sys_SENDTO      = 11;
  Socket_Sys_RECVFROM    = 12;
  Socket_Sys_SHUTDOWN    = 13;
  Socket_Sys_SETSOCKOPT  = 14;
  Socket_Sys_GETSOCKOPT  = 15;
  Socket_Sys_SENDMSG     = 16;
  Socket_Sys_RECVMSG     = 17;


Function SocketCall(SockCallNr,a1,a2,a3,a4,a5,a6:longint):longint;
var
  Regs:SysCallRegs;
  Args:array[1..6] of longint;
begin
  args[1]:=a1;
  args[2]:=a2;
  args[3]:=a3;
  args[4]:=a4;
  args[5]:=a5;
  args[6]:=a6;
  regs.reg2:=SockCallNr;
  regs.reg3:=Longint(@args);
  SocketCall:=Syscall(syscall_nr_socketcall,regs);
  If SocketCall<0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;



Function SocketCall(SockCallNr,a1,a2,a3:longint):longint;
begin
  SocketCall:=SocketCall(SockCallNr,a1,a2,a3,0,0,0);
end;


{******************************************************************************
                          Basic Socket Functions
******************************************************************************}

Function socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  Socket:=SocketCall(Socket_Sys_Socket,Domain,SocketType,ProtoCol);
end;



Function Send(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint;
begin
  Send:=SocketCall(Socket_Sys_Send,Sock,Longint(@Addr),AddrLen,Flags,0,0);
end;



Function Recv(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint;
begin
  Recv:=SocketCall(Socket_Sys_Recv,Sock,Longint(@Addr),AddrLen,Flags,0,0);
end;



Function Bind(Sock:Longint;Var Addr;AddrLen:Longint):Boolean;
begin
  Bind:=(SocketCall(Socket_Sys_Bind,Sock,Longint(@Addr),AddrLen)=0);
end;



Function Listen(Sock,MaxConnect:Longint):Boolean;
begin
  Listen:=(SocketCall(Socket_Sys_Listen,Sock,MaxConnect,0)=0);
end;



Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  Accept:=SocketCall(Socket_Sys_Accept,Sock,longint(@Addr),longint(@AddrLen));
  If Accept<0 Then
    Accept:=-1;
end;



Function Connect(Sock:Longint;Var Addr;Addrlen:Longint): boolean;

begin
  Connect:=SocketCall(Socket_Sys_Connect,Sock,longint(@Addr),AddrLen)=0;
end;



Function Shutdown(Sock:Longint;How:Longint):Longint;
begin
  ShutDown:=SocketCall(Socket_Sys_ShutDown,Sock,How,0);
end;



Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetSocketName:=SocketCall(Socket_Sys_GetSockName,Sock,longint(@Addr),longint(@AddrLen));
end;



Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetPeerName:=SocketCall(Socket_Sys_GetPeerName,Sock,longint(@Addr),longint(@AddrLen));
end;



Function SetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
begin
  SetSocketOptions:=SocketCall(Socket_Sys_SetSockOpt,Sock,Level,OptName,Longint(@OptVal),OptLen,0);
end;



Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
begin
  GetSocketOptions:=SocketCall(Socket_Sys_GetSockOpt,Sock,Level,OptName,Longint(@OptVal),OptLen,0);
end;



Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin
  SocketPair:=SocketCall(Socket_Sys_SocketPair,Domain,SocketType,Protocol,longint(@Pair),0,0);
end;

{******************************************************************************
                               UnixSock
******************************************************************************}

Procedure Str2UnixSockAddr(const addr:string;var t:TUnixSockAddr;var len:longint);
begin
  Move(Addr[1],t.Path,length(Addr));
  t.Family:=AF_UNIX;
  t.Path[length(Addr)]:=#0;
  Len:=Length(Addr)+3;
end;


Function Bind(Sock:longint;const addr:string):boolean;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  Str2UnixSockAddr(addr,UnixAddr,AddrLen);
  Bind(Sock,UnixAddr,AddrLen);
  Bind:=(SocketError=0);
end;



Function DoAccept(Sock:longint;var addr:string):longint;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  AddrLen:=length(addr)+3;
  DoAccept:=Accept(Sock,UnixAddr,AddrLen);
  Move(UnixAddr.Path,Addr[1],AddrLen);
  SetLength(Addr,AddrLen);
end;



Function DoConnect(Sock:longint;const addr:string):Boolean;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  Str2UnixSockAddr(addr,UnixAddr,AddrLen);
  DoConnect:=Connect(Sock,UnixAddr,AddrLen);
end;

Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:text):Boolean;
var
  s : longint;
begin
  S:=DoAccept(Sock,addr);
  if S>0 then
   begin
     Sock2Text(S,SockIn,SockOut);
     Accept:=true;
   end
  else
   Accept:=false;
end;



Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:File):Boolean;
var
  s : longint;
begin
  S:=DoAccept(Sock,addr);
  if S>0 then
   begin
     Sock2File(S,SockIn,SockOut);
     Accept:=true;
   end
  else
   Accept:=false;
end;



Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:text):Boolean;
begin
  Connect:=DoConnect(Sock,addr);
  If Connect then
     Sock2Text(Sock,SockIn,SockOut);
end;



Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:file):Boolean;
begin
  Connect:=DoConnect(Sock,addr);
  if Connect then
     Sock2File(Sock,SockIn,SockOut);
end;

{$i sockets.inc}

end.

{
  $Log$
  Revision 1.13  2000-02-09 16:59:32  peter
    * truncated log

  Revision 1.12  2000/01/07 16:41:41  daniel
    * copyright 2000

  Revision 1.11  2000/01/07 16:32:28  daniel
    * copyright 2000 added

  Revision 1.10  1999/11/14 21:35:04  peter
    * removed warnings

}
