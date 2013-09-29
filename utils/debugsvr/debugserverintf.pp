{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Interface for debug server.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit debugserverintf;

Interface

Uses
  msgintf,baseunix,classes,sockets,sysutils;

Const
  MsgTypes : Array[-1..3] of string =
    ('Disconnect','Information','Warning','Error','Identify');


Type
  Thandle = Longint; // Abstraction for easier porting.

  TClient = Class(TObject)
    Handle : THandle;
    Peer : ShortString;
    Data : Pointer;
  end;

  TDebugEvent = Record
    Client : TClient;
    LogCode : Integer;
    TimeStamp : TDateTime;
    Event : String;
  end;

Var
  FClients : TList;
  Accepting : Boolean;
  Quit : Boolean;
  DebugLogCallback : Procedure (Const Event : TDebugEvent);
  DebugObjLogCallBack : Procedure (Const Event : TDebugEvent) of Object;
  CloseConnectionCallBack : Procedure (Client : TClient);
  CloseObjConnectionCallBack : Procedure (Client : TClient) of Object;


Procedure OpenDebugServer;
Procedure CloseDebugServer;
Function  ClientFromHandle (AHandle : THandle) : TClient;
Procedure ReadMessage(Handle : THandle);
Procedure ReadMessageEvent(Handle : THandle; Var Event : TDebugEvent);
Function  CheckNewConnection : TClient;
procedure CloseConnection(Client : TClient);
Procedure CloseClientHandle(Handle : THandle);

ResourceString
  SClientLog = 'Client log %d';
  SEvent     = 'Event';
  SMessage   = 'Message';
  SStopAccepting = 'Stop accepting new connections';
  SStartAccepting = 'Start accepting new connections';
  SErrSocketFailed = 'Creation of socket failed: %s';
  SErrBindFailed = 'Binding of socket failed: %s';
  SErrListenFailed = 'Listening on port #%d failed: %s';
  SErrAcceptFailed = 'Could not accept a client connection: %d';
  SClosingConnection = 'Closing connection.';
  SErrFailedToSetSignalHandler = 'Failed to set signal handler.';
  SPeerAt = 'Peer at %d';


Implementation

Function ClientFromHandle (AHandle : THandle) : TClient;

Var
  I : Longint;

begin
  Result:=Nil;
  I:=0;
  With FClients do
    While (I<Count) and (Result=Nil) do
      Begin
      If TClient(Items[i]).Handle=AHandle then
        Result:=TClient(Items[i]);
      Inc(I);
      end;
end;

{ ---------------------------------------------------------------------
    Communications handling: Unix Socket setup
  ---------------------------------------------------------------------}

Var
  FSocket : Integer;


Procedure SetupUnixSocket;

var
  Flags,AddrLen : Integer;
  FUnixAddr : TUnixSockAddr;
  FFileName : String;
  Quit : Boolean;

begin
  FFileName:=DebugSocket;
  FSocket:=FPSocket(AF_UNIX,SOCK_STREAM,0);
  If FSocket<0 Then
    Raise Exception.Create(SErrSocketFailed);
  Flags:=fpFCntl(FSOCket,F_GETFL);
  Flags:=Flags or O_NONBLOCK;
  fpFCntl(FSocket,F_SETFL,Flags);
  Str2UnixSockAddr(FFilename,FUnixAddr,AddrLen);
  If FPBind(FSocket,@FUnixAddr,AddrLen)<>0 then
     Raise Exception.CreateFmt(SErrBindFailed,[FFileName]);
  If (fpListen(FSocket,5)<>0) then
    Raise Exception.CreateFmt(SErrListenFailed,[FSocket]);
  FClients:=TList.Create;
  Accepting:=True;
end;

Procedure DestroyUnixSocket;

Var
  C : TClient;

begin
  If Assigned(FClients) then
    begin
    With FClients do
      While Count>0 do
        begin
        C:=TClient(Items[Count-1]);
        FileClose(C.Handle);
        C.Free;
        Delete(Count-1);
        end;
    FileClose(FSocket);
    DeleteFile(DebugSocket);
    end;
end;

{ ---------------------------------------------------------------------
    Communications handling: Inet Socket setup
  ---------------------------------------------------------------------}

Procedure SetupInetSocket(Aport : Word);

var
  Flags,AddrLen : Integer;
  FInetAddr : TInetSockAddr;
  FFileName : String;
  Quit : Boolean;

begin
  FSocket:=FPSocket(AF_INET,SOCK_STREAM,0);
  If FSocket<0 Then
    Raise Exception.Create(SErrSocketFailed);
  Flags:=fpFCntl(FSocket,F_GETFL);
  Flags:=Flags or O_NONBLOCK;
  fpFCntl(FSocket,F_SETFL,Flags);
  FInetAddr.Family := AF_INET;
  Writeln('Using port : ',APort);
  FInetAddr.Port := Swap(APort);
  FInetAddr.Addr := 0;
  If FPBind(FSocket,@FInetAddr,SizeOf(FInetAddr))<>0 then
     Raise Exception.CreateFmt(SErrBindFailed,[FFileName]);
  If fpListen(FSocket,5)<>0 then
    Raise Exception.CreateFmt(SErrListenFailed,[FSocket]);
end;

Procedure DestroyInetSocket;

Var
  C : TClient;

begin
  If Assigned(FClients) then
    begin
    With FClients do
      While Count>0 do
        begin
        C:=TClient(Items[Count-1]);
        FileClose(C.Handle);
        C.Free;
        Delete(Count-1);
        end;
    FileClose(FSocket);
    end;
end;

{ ---------------------------------------------------------------------
    Communications handling: Public interface
  ---------------------------------------------------------------------}


Procedure OpenDebugServer;

begin
  Case DebugConnection of
    dcUnix : SetupUnixSocket;
    dcInet : SetupInetSocket(DebugPort);
  end;
  FClients:=TList.Create;
  Accepting:=True;
end;

Procedure CloseDebugServer;

begin
  Accepting:=False;
  Case DebugConnection of
    dcUnix : DestroyUnixSocket;
    dcInet : DestroyInetSocket;
  end;
  FClients.Free;
  FClients:=Nil;
end;


{ ---------------------------------------------------------------------
    Communications handling: Connection handling
  ---------------------------------------------------------------------}

Function GetNewConnection : THandle;

Var
  ClientAddr: TUnixSockAddr;
  L : Integer;

begin
  If Accepting then
    begin
    L:=SizeOf(ClientAddr);
    Result:=fpAccept(FSocket,@ClientAddr,@L);
    If (Result<0) Then
      if (Errno<>ESYSEAgain) then
        Raise Exception.CreateFmt(SErrAcceptFailed,[FSocket])
      else
        Result:=-1
{$ifdef debug}
    else
      Writeln('New connection detected at ',Result)
{$endif debug}
    end
  else
    Result:=-1;
end;

Function CheckNewConnection : TClient;

Var
  NC : THandle;

begin
  NC:=GetNewConnection;
  If (NC=-1) then
    Result:=Nil
  else
    begin
    Result:=TClient.Create;
    Result.Handle:=NC;
{$ifdef debug}
    Writeln('Added new client', nc, ' at : ',FClients.Add(Result));
{$else}
    FClients.Add(Result);
{$endif debug}
    end;
end;

Procedure CloseClientHandle(Handle : THandle);

begin
  fpShutDown(Handle,2);
  FileClose(Handle);
end;

Procedure CloseConnection(Client : TClient);

Var
  I : longint;
  C : TClient;

begin
  If Assigned(Client) then
    begin
    If Assigned(CloseConnectionCallBack) then
      CloseConnectionCallBack(Client);
    If Assigned(CloseObjConnectionCallBack) then
      CloseObjConnectionCallBack(Client);
    CloseClientHandle(Client.Handle);
    FClients.Remove(Client);
    Client.Free;
    end;
end;

{ ---------------------------------------------------------------------
    Message handling
  ---------------------------------------------------------------------}

Function MsgToEvent(AHandle: THandle; ALogCode : Integer; ATimeStamp : TDateTime; AEvent : String) : TDebugEvent;

begin
  With Result do
    begin
    Client:=ClientFromHandle(AHandle);
    If (Client<>Nil) then
      begin
      If (ALogCode=lctIdentify) then
        Client.Peer:=AEvent;
      end;
    LogCode:=ALogCode;
    TimeStamp:=ATimeStamp;
    Event:=AEvent;
    end;
end;

Procedure LogEvent(Event : TDebugEvent);

begin
  if Assigned(DebugLogCallback) then
    DebugLogCallBack(Event);
  If Assigned(DebugObjLogCallBack) then
    DebugObjLogCallBack(Event);
end;

Procedure ReadMessageEvent(Handle : THandle; Var Event : TDebugEvent);

Var
  FDebugMessage : TDebugMessage;
  msgSize : Integer;

begin
  Try
    With FDebugMessage do
      begin
      // Select reports read ready when closed, so check for this.
      If (FileRead(Handle,msgType,SizeOf(Integer))=0) or (MsgType=-1) then
        begin
        event:=MsgToEvent(Handle,lctStop,Now,SClosingConnection);
        If Assigned(Event.Client) then
          CloseConnection(Event.Client)
        else
          CloseClientHandle(Handle);
        end
      else
        begin
        FileRead(Handle,msgTimeStamp,sizeof(TDateTime));
        FileRead(Handle,MsgSize,SizeOf(Integer));
        SetLength(Msg,MsgSize);
        FileRead(Handle,Msg[1],MsgSize);
        Event:=MsgToEvent(Handle,msgType,msgTimeStamp,Msg);
        end
      end;
  except
    On E : Exception do
      Event:=MsgToEvent(Handle,lctError,Now,E.Message);
  end;
end;

Procedure ReadMessage(Handle : THandle);

Var
  Event : TDebugEvent;

begin
  ReadMessageEvent(Handle,Event);
  LogEvent(Event);
end;

end.
