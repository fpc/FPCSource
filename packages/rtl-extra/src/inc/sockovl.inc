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


{******************************************************************************
                          Basic Socket Functions
******************************************************************************}


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
  Bind := fpBind(Sock, @UnixAddr, AddrLen)=0; // what the flying fuck were you thinking?
end;


Function DoAccept(Sock:longint;var addr:string):longint;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  AddrLen:=length(addr)+3;
  DoAccept:=fpAccept(Sock,@UnixAddr,@AddrLen);
  Move(UnixAddr.Path,Addr[1],AddrLen);
  SetLength(Addr,AddrLen);
end;



Function DoConnect(Sock:longint;const addr:string):Boolean;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  Str2UnixSockAddr(addr,UnixAddr,AddrLen);
  DoConnect:=fpConnect(Sock,@UnixAddr,AddrLen)=0;
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

Function CloseSocket (Sock:Longint):Longint;
begin
  if fpclose(Sock)=0 then
    CloseSocket := 0 else
    CloseSocket := -1;
end;


