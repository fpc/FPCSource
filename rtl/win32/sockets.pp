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
{$mode objfpc}
unit Sockets;

Interface

  Uses
     windows,winsock;

  Const
     AF_MAX          = WinSock.AF_MAX;
     PF_MAX          = AF_MAX;

{$i socketsh.inc}

Implementation

{ Include filerec and textrec structures }
{$i filerec.inc}
{$i textrec.inc}

{******************************************************************************
                          Basic Socket Functions
******************************************************************************}

Function socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  Socket:=WinSock.Socket(Domain,SocketType,ProtoCol);
  if Socket<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
begin
  Send:=WinSock.Send(Sock,Buf,BufLen,Flags);
  if Send<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function SendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint;Var Addr; AddrLen : Longint):Longint;
begin
  // Dubious construct, this should be checked.
  Send:=WinSock.SendTo(Sock,Buf,BufLen,Flags,Winsock.TSockAddr(Addr),AddrLen);
  if Send<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
begin
  Recv:=WinSock.Recv(Sock,Buf,BufLen,Flags);
  if Recv<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;


Function RecvFrom(Sock : Longint; Var Buf; Buflen,Flags : Longint; Var Addr; AddrLen : Integer) : longint;

begin
  Recv:=WinSock.RecvFrom(Sock,Buf,BufLen,Flags,Winsock.TSockAddr(Addr),AddrLen);
  if Recv<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;

  var
     l : longint;

begin
  l:=WinSock.Bind(Sock,WinSock.PSockAddr(@Addr),AddrLen);
  if l<0 then
    begin
       SocketError:=WSAGetLastError;
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
  l:=WinSock.Listen(Sock,MaxConnect);
  if l<0 then
    begin
       SocketError:=WSAGetLastError;
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
  Accept:=WinSock.Accept(Sock,WinSock.PSockAddr(@Addr),plongint(@AddrLen));
  if Accept<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function Connect(Sock:Longint;Const Addr;Addrlen:Longint):Boolean;

begin
  Connect:=WinSock.Connect(Sock,WinSock.TSockAddr(Addr),AddrLen)=0;
  if not Connect then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function Shutdown(Sock:Longint;How:Longint):Longint;
begin
  ShutDown:=WinSock.ShutDown(Sock,How);
  if ShutDown<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetSocketName:=WinSock.GetSockName(Sock,WinSock.TSockAddr(Addr),AddrLen);
  if GetSocketName<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetPeerName:=WinSock.GetPeerName(Sock,WinSock.TSockAddr(Addr),AddrLen);
  if GetPeerName<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;
begin
  SetSocketOptions:=WinSock.SetSockOpt(Sock,Level,OptName,OptVal,OptLen);
  if SetSocketOptions<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
begin
  GetSocketOptions:=WinSock.GetSockOpt(Sock,Level,OptName,OptVal,OptLen);
  if GetSocketOptions<0 then
    SocketError:=WSAGetLastError
  else
    SocketError:=0;
end;

Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin
  // SocketPair:=SocketCall(Socket_Sys_SocketPair,Domain,SocketType,Protocol,longint(@Pair),0,0);
end;


{ mimic the linux fdWrite/fdRead calls for the file/text socket wrapper }
function fdWrite(handle : longint;Const bufptr;size : dword) : dword;
begin
  fdWrite := WinSock.send(handle, bufptr, size, 0);
  if fdWrite = SOCKET_ERROR then
  begin
    SocketError := WSAGetLastError;
    fdWrite := 0;
  end
  else
    SocketError := 0;
end;

function fdRead(handle : longint;var bufptr;size : dword) : dword;
  var
     d : dword;

  begin
     if ioctlsocket(handle,FIONREAD,@d) = SOCKET_ERROR then
       begin
         SocketError:=WSAGetLastError;
         fdRead:=0;
         exit;
       end;
     if d>0 then
       begin
         if size>d then
           size:=d;
         fdRead := WinSock.recv(handle, bufptr, size, 0);
         if fdRead = SOCKET_ERROR then
         begin
           SocketError:= WSAGetLastError;
           fdRead := 0;
         end else
           SocketError:=0;
       end
     else
       SocketError:=0;
  end;


{$i sockets.inc}

{ winsocket stack needs an init. and cleanup code }
var
  wsadata : twsadata;

initialization
  WSAStartUp($2,wsadata);
finalization
  WSACleanUp;
end.
{
  $Log$
  Revision 1.6  2002-02-04 21:29:34  michael
  + merged missing sendto/rcvfrom functions

  Revision 1.5  2001/06/06 21:58:24  peter
    * Win32 fixes for Makefile so it doesn't require sh.exe

  Revision 1.1.2.4  2002/02/04 21:25:28  michael
  + Added missing sendto/rcvfrom functions

  Revision 1.1.2.3  2001/06/06 22:00:40  peter
    * Win32 fixes

  Revision 1.4  2000/07/28 08:43:46  sg
  * Applied patch by Markus Kaemmerer: Fixes fdRead and fdWrite

  Revision 1.3  2000/07/28 06:34:53  sg
  * Applied patch to "Connect" by Markus Kaemmerer: WinSock.Connect returns
    zero when it succeeded, and not vice versa.

  Revision 1.2  2000/07/13 11:33:58  michael
  + removed logs

}
