{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 2000 by Marco van de Voort
     member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

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
{$IFNDEF BSD}
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
 {$ELSE}
  SocketError:=-1;
 {$ENDIF}
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

Function CloseSocket (Sock:Longint):Longint;
begin
  if fdClose (Sock) then
    CloseSocket := 0 else
    CloseSocket := -1;
end;

Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
begin
  Send:=SocketCall(Socket_Sys_Send,Sock,Longint(@Buf),BufLen,Flags,0,0);
end;


Function SendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint;Var Addr; AddrLen : Longint):Longint;
begin
  Sendto:=SocketCall(Socket_Sys_SendTo,Sock,Longint(@Buf),BufLen,Flags,Longint(@Addr),AddrLen);
end;

Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
begin
  Recv:=SocketCall(Socket_Sys_Recv,Sock,Longint(@Buf),BufLen,Flags,0,0);
end;

Function RecvFrom(Sock : Longint; Var Buf; Buflen,Flags : Longint; Var Addr ; AddrLen : Integer) : longint;

begin
  RecvFrom:=SocketCall(Socket_Sys_Recv,Sock,Longint(@buf),buflen,flags,Longint(@Addr),AddrLen);
end;

Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;
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



Function Connect(Sock:Longint;Const Addr;Addrlen:Longint): boolean;
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



Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;
begin
  SetSocketOptions:=SocketCall(Socket_Sys_SetSockOpt,Sock,Level,OptName,Longint(@OptVal),OptLen,0);
end;



Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
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

{
  $Log$
  Revision 1.7  2003-03-23 17:47:15  armin
  * CloseSocket added

  Revision 1.6  2002/09/07 16:01:20  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/02/04 21:29:34  michael
  + merged missing sendto/rcvfrom functions

}
