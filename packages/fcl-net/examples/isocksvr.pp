Program server;
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TInetServer server program. This will listen on port 4100 till
  a client connects. You can connect by running the 'isockcli' or
  'dsockcli -i' programs in another terminal.
}

{$mode objfpc}{$H+}
uses 
  {$IFDEF UNIX}cthreads,{$ENDIF} 
  classes, sockets, ssockets;


const
  ThePort=4100;

Type
  { TAcceptThread }

  TAcceptThread = Class(TThread)
    FSocket : TInetServer;
    Constructor Create(aSocket : TInetServer);
    Procedure Execute; override;
  end;

  TINetServerApp = Class(TObject)
  Private
    FServer : TInetServer;
  Public
    Constructor Create(Port : longint);
    Destructor Destroy;override;
    Procedure OnConnect (Sender : TObject; Data : TSocketStream);
    Procedure OnDisConnect (Sender : TObject; Data : TSocketStream);
    Procedure Run;
  end;


{ TAcceptThread }

constructor TAcceptThread.Create(aSocket: TInetServer);
begin
  FSocket:=aSocket;
  FreeOnTerminate:=True;
  Inherited Create(False);
end;

procedure TAcceptThread.Execute;
begin
  FSocket.StartAccepting;
end;

{ TInetServerApp }

Constructor TInetServerApp.Create(Port : longint);

begin
  FServer:=TINetServer.Create(Port);
  FServer.OnConnect:=@OnConnect;
  FServer.OnDisConnect:=@OnDisConnect;
end;

Destructor TInetServerApp.Destroy;

begin
  FServer.Free;
end;


Function SocketAddrToString(ASocketAddr: TSockAddr): AnsiString;

Var
  S : ShortString;

begin
  Result:='';
  if ASocketAddr.sa_family = AF_INET then
    begin
    S := NetAddrToStr(ASocketAddr.sin_addr);
    Result:=S;
    end;
end;

Procedure TInetServerApp.OnDisConnect (Sender : TObject; Data : TSocketStream);


var
  PeerHost : String;

begin
  PeerHost:=SocketAddrToString(Data.RemoteAddress);
  Writeln('Disconnecting from ',PeerHost);
end;

Procedure TInetServerApp.OnConnect (Sender : TObject; Data : TSocketStream);


Var Buf : ShortString;
    Count : longint;

begin
  Writeln('Connection from ',SocketAddrToString(Data.RemoteAddress));
  Repeat
    Count:=Data.Read(Buf[1],255);
    SetLength(Buf,Count);
    Write('Server got : ',Buf);
  Until (Count=0) or (Pos('QUIT',Buf)<>0);
  if Pos('QUIT',Buf)<>0 then
    FServer.StopAccepting;
  Data.Free;
end;

Procedure TInetServerApp.Run;

begin
  TAcceptThread.Create(FServer).WaitFor;
end;

Var
  Application : TInetServerApp;

begin
  Application:=TInetServerApp.Create(ThePort);
  Application.Run;
  Application.Free;
end.
