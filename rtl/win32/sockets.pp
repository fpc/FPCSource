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

Function CloseSocket(Sock:Longint):Longint;
var i : longint;
begin
  i := Winsock.CloseSocket (Sock);
  if i <> 0 then
  begin
    SocketError:=WSAGetLastError;
    CloseSocket := i;
  end else
  begin
    CloseSocket := 0;
    SocketError := 0;
  end;
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
  SendTo:=WinSock.SendTo(Sock,Buf,BufLen,Flags,Winsock.TSockAddr(Addr),AddrLen);
  if SendTo<0 then
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
  RecvFrom:=WinSock.RecvFrom(Sock,Buf,BufLen,Flags,Winsock.TSockAddr(Addr),AddrLen);
  if RecvFrom<0 then
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


{$ifdef unix}
{ mimic the linux fpWrite/fpRead calls for the file/text socket wrapper }
function fpWrite(handle : longint;Const bufptr;size : dword) : dword;
begin
  fpWrite := dword(WinSock.send(handle, bufptr, size, 0));
  if fpWrite = dword(SOCKET_ERROR) then
  begin
    SocketError := WSAGetLastError;
    fpWrite := 0;
  end
  else
    SocketError := 0;
end;

function fpRead(handle : longint;var bufptr;size : dword) : dword;
  var
     d : dword;

  begin
     if ioctlsocket(handle,FIONREAD,@d) = SOCKET_ERROR then
       begin
         SocketError:=WSAGetLastError;
         fpRead:=0;
         exit;
       end;
     if d>0 then
       begin
         if size>d then
           size:=d;
         fpRead := dword(WinSock.recv(handle, bufptr, size, 0));
         if fpRead = dword(SOCKET_ERROR) then
         begin
           SocketError:= WSAGetLastError;
           fpRead := 0;
         end else
           SocketError:=0;
       end
     else
       SocketError:=0;
  end;
{$else}
{ mimic the linux fdWrite/fdRead calls for the file/text socket wrapper }
function fdWrite(handle : longint;Const bufptr;size : dword) : dword;
begin
  fdWrite := dword(WinSock.send(handle, bufptr, size, 0));
  if fdWrite = dword(SOCKET_ERROR) then
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
         fdRead := dword(WinSock.recv(handle, bufptr, size, 0));
         if fdRead = dword(SOCKET_ERROR) then
         begin
           SocketError:= WSAGetLastError;
           fdRead := 0;
         end else
           SocketError:=0;
       end
     else
       SocketError:=0;
  end;
{$endif}

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
  Revision 1.12  2003-09-17 15:06:36  peter
    * stdcall patch

  Revision 1.11  2003/03/23 17:47:15  armin
  * CloseSocket added

  Revision 1.10  2003/01/01 14:34:22  peter
    * sendto overload

  Revision 1.9  2002/09/07 16:01:29  peter
    * old logs removed and tabs fixed

  Revision 1.8  2002/07/17 07:28:21  pierre
   * avoid constant evaluation problems if cycling with -Cr

  Revision 1.7  2002/02/04 21:41:15  michael
  + merged ixed syntax

  Revision 1.6  2002/02/04 21:29:34  michael
  + merged missing sendto/rcvfrom functions

}
