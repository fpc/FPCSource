{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}

unit ssockets;


interface

uses SysUtils, Classes, sockets;

type

  TSocketErrorType = (
    seHostNotFound,
    seCreationFailed,
    seBindFailed,
    seListenFailed,
    seConnectFailed,
    seAcceptFailed,
    seAcceptWouldBlock);

  TSocketOption = (soDebug,soReuseAddr,soKeepAlive,soDontRoute,soBroadcast,
                   soOOBinline);
  TSocketOptions = Set of TSocketOption;

  ESocketError = class(Exception)
    Code: TSocketErrorType;
    constructor Create(ACode: TSocketErrorType; const MsgArgs: array of const);
  end;

  TSocketStream = class(THandleStream)
  Private
    FSocketOptions : TSocketOptions;
    Procedure GetSockOptions;
    Procedure SetSocketOptions(Value : TSocketOptions);
  Public
    Constructor Create (AHandle : Longint);virtual;
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    Property SocketOptions : TSocketOptions Read FSocketOptions
                                            Write SetSocketOptions;
  end;

  TConnectEvent = Procedure (Sender : TObject; Data : TSocketStream) Of Object;
  TConnectQuery = Procedure (Sender : TObject; ASocket : Longint; Var Allow : Boolean) of Object;

  TSocketServer = Class(TObject)
  Private
    FOnIdle : TNotifyEvent;
    FNonBlocking : Boolean;
    FSocket : longint;
    FListened : Boolean;
    FAccepting : Boolean;
    FMaxConnections : Longint;
    FQueueSize : Longint;
    FOnConnect : TConnectEvent;
    FOnConnectQuery : TConnectQuery;
    Procedure DoOnIdle;
  Protected
    FSockType : Longint;
    FBound : Boolean;
    Procedure DoConnect(ASocket : TSocketStream); Virtual;
    Function  DoConnectQuery(ASocket : longint): Boolean ;Virtual;
    Procedure Bind; Virtual; Abstract;
    Function  Accept: Longint;Virtual;Abstract;
    Function  SockToStream (ASocket : Longint) : TSocketStream;Virtual;Abstract;
    Procedure Close; Virtual;
  Public
    Constructor Create(ASocket : Longint);
    Destructor Destroy; Override;
    Procedure Listen;
    Procedure StartAccepting;
    Procedure StopAccepting;
    Procedure SetNonBlocking;
    Property Bound : Boolean Read FBound;
    Property MaxConnections : longint Read FMaxConnections Write FMaxConnections;
    Property QueueSize : Longint Read FQueueSize Write FQueueSize default 5;
    Property OnConnect : TConnectEvent Read FOnConnect Write FOnConnect;
    Property OnConnectQuery : TConnectQuery Read FOnConnectQuery Write FOnConnectQuery;
    Property OnIdle : TNotifyEvent Read FOnIdle Write FOnIdle;
    Property NonBlocking : Boolean Read FNonBlocking;
    Property Socket : Longint Read FSocket;
    Property SockType : Longint Read FSockType;
  end;

  TInetServer = Class(TSocketServer)
  Protected
    FAddr : TINetSockAddr;
    Function  SockToStream (ASocket : Longint) : TSocketStream;Override;
    Function Accept : Longint;override;
    FPort : Word;
  Public
    Procedure Bind; Override;
    Constructor Create(APort: Word);
    Property Port : Word Read FPort;
  end;

  TUnixServer = Class(TSocketServer)
  Private
    FUnixAddr : TUnixSockAddr;
    FFileName : String;
  Protected
    Procedure Bind; Override;
    Function Accept : Longint;override;
    Function SockToStream (ASocket : Longint) : TSocketStream;Override;
    Procedure Close; override;
  Public
    Constructor Create(AFileName : String);
    Property FileName : String Read FFileName;
  end;

  TInetSocket = Class(TSocketStream)
  Private
    FHost : String;
    FPort : Word;
  Protected
    Procedure DoConnect(ASocket : longint); Virtual;
  Public
    Constructor Create(ASocket : longint); Override; {$ifndef ver1_0}Overload;{$endif}
    Constructor Create(const AHost: String; APort: Word); {$ifndef ver1_0}Overload;{$endif}
    Property Host : String Read FHost;
    Property Port : Word Read FPort;
  end;

  TUnixSocket = Class(TSocketStream)
  Private
    FFileName : String;
  Protected
    Procedure DoConnect(ASocket : longint); Virtual;
  Public
    Constructor Create(ASocket : Longint); {$ifndef ver1_0}Overload;{$endif}
    Constructor Create(AFileName : String); {$ifndef ver1_0}Overload;{$endif}
    Property FileName : String Read FFileName;
  end;

Implementation

uses
{$ifdef unix}
  {$ifdef ver1_0}
    Linux,
  {$else}
    Unix,
  {$endif}
 {$endif}
  inet
  ;

Const
  SocketWouldBlock = -2;

{ ---------------------------------------------------------------------
  ESocketError
  ---------------------------------------------------------------------}

resourcestring
  strHostNotFound = 'Host name resolution for "%s" failed.';
  strSocketCreationFailed = 'Creation of socket failed: %s';
  strSocketBindFailed = 'Binding of socket failed: %s';
  strSocketListenFailed = 'Listening on port #%d failed: %s';
  strSocketConnectFailed = 'Connect to %s failed.';
  strSocketAcceptFailed = 'Could not accept a client connection: %s';
  strSocketAcceptWouldBlock = 'Accept would block on socket: %d';

constructor ESocketError.Create(ACode: TSocketErrorType; const MsgArgs: array of const);
var
  s: String;
begin
  Code := ACode;
  case ACode of
    seHostNotFound  : s := strHostNotFound;
    seCreationFailed: s := strSocketCreationFailed;
    seBindFailed    : s := strSocketBindFailed;
    seListenFailed  : s := strSocketListenFailed;
    seConnectFailed : s := strSocketConnectFailed;
    seAcceptFailed  : s := strSocketAcceptFailed;
    seAcceptWouldBLock : S:= strSocketAcceptWouldBlock;
  end;
  s := Format(s, MsgArgs);
  inherited Create(s);
end;

{ ---------------------------------------------------------------------
    TSocketStream
  ---------------------------------------------------------------------}
Constructor TSocketStream.Create (AHandle : Longint);

begin
  Inherited Create(AHandle);
  GetSockOptions;
end;

destructor TSocketStream.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;

Procedure TSocketStream.GetSockOptions;

begin
end;

Procedure TSocketStream.SetSocketOptions(Value : TSocketOptions);

begin
end;

Function TSocketStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Result:=0;
end;

{ ---------------------------------------------------------------------
    TSocketServer
  ---------------------------------------------------------------------}

Constructor TSocketServer.Create(ASocket : Longint);

begin
  FSocket:=ASocket;
  FQueueSize :=5;
end;

Destructor TSocketServer.Destroy;

begin
  Close;
end;

Procedure TSocketServer.Close;

begin
  If FSocket<>-1 Then
    FileClose(FSocket);
  FSocket:=-1;
end;

Procedure TSocketServer.Listen;

begin
  If Not FBound then
    Bind;
  If Not Sockets.Listen(FSocket,FQueueSize) then
    Raise ESocketError.Create(seListenFailed,[FSocket]);
end;

Procedure TSocketServer.StartAccepting;

Var
 NoConnections,
 NewSocket : longint;
 Stream : TSocketStream;

begin
  FAccepting := True; 
  Listen;
  Repeat
    Repeat
      Try
        NewSocket:=Accept;
        If NewSocket>=0 then
          begin
          Inc (NoConnections);
          If DoConnectQuery(NewSocket) Then
            begin
            Stream:=SockToStream(NewSocket);
            DoConnect(Stream);
            end
          end
      except
        On E : ESocketError do
          If E.Code=seAcceptWouldBlock then
            begin
            DoOnIdle;
            NewSocket:=-1;
            end;
          else
            Raise;
       end;
    Until (NewSocket>=0) or (Not NonBlocking);
  Until Not (FAccepting) or ((FMaxConnections<>-1) and (NoConnections>=FMaxConnections));
end;

Procedure TSocketServer.StopAccepting;

begin
  FAccepting:=False;
end;

Procedure TSocketServer.DoOnIdle;

begin
  If Assigned(FOnIdle) then
    FOnIdle(Self);
end;

Procedure TSocketServer.DoConnect(ASocket : TSocketStream);

begin
  If Assigned(FOnConnect) Then
    FOnConnect(Self,ASocket);
end;

Function TSocketServer.DoConnectQuery(ASocket : Longint) : Boolean;

begin
  Result:=True;
  If Assigned(FOnConnectQuery) then
    FOnConnectQuery(Self,ASocket,Result);
end;

Procedure TSocketServer.SetNonBlocking;

begin
  fcntl(FSocket,F_SETFL,OPEN_NONBLOCK);
  FNonBlocking:=True;
end;

{ ---------------------------------------------------------------------
    TInetServer
  ---------------------------------------------------------------------}

Constructor TInetServer.Create(APort: Word);

Var S : longint;

begin
  FPort:=APort;
  S:=Sockets.Socket(AF_INET,SOCK_STREAM,0);
  If S=-1 Then
    Raise ESocketError.Create(seCreationFailed,[Format('%d',[APort])]);
  Inherited Create(S);
end;


Procedure TInetServer.Bind;


begin
  Faddr.family := AF_INET;
  Faddr.port := ShortHostToNet(FPort);
  Faddr.addr := 0;
  if not Sockets.Bind(FSocket, FAddr, Sizeof(FAddr)) then
    raise ESocketError.Create(seBindFailed, [IntToStr(FPort)]);
  FBound:=True;
end;

Function  TInetServer.SockToStream (ASocket : Longint) : TSocketStream;

begin
  Result:=TInetSocket.Create(ASocket);
  (Result as TInetSocket).FHost:='';
  (Result as TInetSocket).FPort:=FPort;
end;

Function TInetServer.Accept : Longint;

Var l : longint;

begin
  L:=SizeOf(FAddr);
  Result:=Sockets.Accept(Socket,Faddr,L);
  If Result<0 then
    If SocketError=Sys_EWOULDBLOCK then
      Raise ESocketError.Create(seAcceptWouldBlock,[socket])
    else
      Raise ESocketError.Create(seAcceptFailed,[socket]);
end;

{ ---------------------------------------------------------------------
    TUnixServer
  ---------------------------------------------------------------------}

Constructor TUnixServer.Create(AFileName : String);

Var S : Longint;

begin
  FFileName:=AFileName;
  S:=Sockets.Socket(AF_UNIX,SOCK_STREAM,0);
  If S=-1 then
    Raise ESocketError.Create(seCreationFailed,[AFileName])
  else
    Inherited Create(S);
end;

Procedure TUnixServer.Close;
begin
  Inherited Close;
  DeleteFile(FFileName);
  FFileName:='';
end;

Procedure TUnixServer.Bind;

var
  AddrLen  : longint;
begin
  Str2UnixSockAddr(FFilename,FUnixAddr,AddrLen);
  If Not Sockets.Bind(Socket,FUnixAddr,AddrLen) then
    Raise ESocketError.Create(seBindFailed,[FFileName]);
  FBound:=True;
end;

Function TUnixServer.Accept : Longint;

Var L : longint;

begin
  L:=Length(FFileName);
  Result:=Sockets.Accept(Socket,FUnixAddr,L);
  If Result<0 then
    If SocketError=Sys_EWOULDBLOCK then
      Raise ESocketError.Create(seAcceptWouldBlock,[socket])
    else
      Raise ESocketError.Create(seAcceptFailed,[socket]);
end;

Function  TUnixServer.SockToStream (ASocket : Longint) : TSocketStream;

begin
  Result:=TUnixSocket.Create(ASocket);
  (Result as TUnixSocket).FFileName:=FFileName;
end;


{ ---------------------------------------------------------------------
    TInetSocket
  ---------------------------------------------------------------------}
Constructor TInetSocket.Create(ASocket : Longint);

begin
  Inherited Create(ASocket);
end;

Constructor TInetSocket.Create(const AHost: String; APort: Word);

Var
  S : Longint;

begin
  FHost:=AHost;
  FPort:=APort;
  S:=Socket(AF_INET,SOCK_STREAM,0);
  DoConnect(S);
  Inherited Create(S);
end;

Procedure TInetSocket.DoConnect(ASocket : Longint);

Var
  TheHost: THost;
  addr: TInetSockAddr;

begin

  TheHost.NameLookup(FHost);
  if TheHost.LastError <> 0 then
    raise ESocketError.Create(seHostNotFound, [FHost]);

  addr.family := AF_INET;
  addr.port := ShortHostToNet(FPort);
  addr.addr := HostToNet(LongInt(TheHost.IPAddress));

  If not Sockets.Connect(ASocket, addr, sizeof(addr)) then
    raise ESocketError.Create(seConnectFailed, [Format('%s:%d',[FHost, FPort])]);
end;

{ ---------------------------------------------------------------------
    TUnixSocket
  ---------------------------------------------------------------------}

Constructor TUnixSocket.Create(ASocket : Longint);

begin
  Inherited Create(ASocket);
end;

Constructor TUnixSocket.Create(AFileName : String);

Var S : Longint;

begin
  FFileName:=AFileName;
  S:=Socket(AF_UNIX,SOCK_STREAM,0);
  DoConnect(S);
  Inherited Create(S);
end;

Procedure TUnixSocket.DoConnect(ASocket : longint);

Var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  Str2UnixSockAddr(FFilename,UnixAddr,AddrLen);
  If Not Connect(ASocket,UnixAddr,AddrLen) then
    Raise ESocketError.Create(seConnectFailed,[FFilename]);
end;

end.

{
  $Log$
  Revision 1.13  2002-12-12 17:53:49  peter
    * add FAccepting:=true to StartAccepting

  Revision 1.12  2002/09/07 15:15:25  peter
    * old logs removed and tabs fixed

  Revision 1.11  2002/05/31 11:31:46  marco
   * 1.0.x Renamefest for FCL. Fixed some oddities in 1.1 too

}
