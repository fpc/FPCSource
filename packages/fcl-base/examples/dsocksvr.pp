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
  Dual server program. This will listen on port 4100 till
  a client connects if '-i' is on the command-line.
  Otherwise it will open a unix socket. You can connect by
  running the 'sockcli' or 'dsockcli' programs in another
  terminal.

  specifying -b on the command-line will disable blocking.
}
{$mode objfpc} {$H+}
uses sysutils,ssockets,getopts;

const
  ThePort=4100;
  TheSocket = 'ServerSoc';

Var
  DoInet,NonBlocking : boolean;

Type
  TServerApp = Class(TObject)
  Private
    FCalls : longint;
    FServer : TSocketServer;
  Public
    Constructor Create(Port : longint);
    Constructor Create(Socket : String);
    Destructor Destroy;override;
    Procedure OnConnect (Sender : TObject; Data : TSocketStream);
    Procedure OnIdle(Sender : TObject);
    Procedure Run;
  end;

Constructor TServerApp.Create(Port : longint);

begin
  FServer:=TINetServer.Create(Port);
  FServer.OnConnect:=@OnConnect;
end;

Constructor TServerApp.Create(Socket : String);

begin
  FServer:=TUnixServer.Create(Socket);
  FServer.OnConnect:=@OnConnect;
end;

Destructor TServerApp.Destroy;

begin
  FServer.Free;
end;

Procedure TServerApp.OnConnect (Sender : TObject; Data : TSocketStream);


Var Buf : ShortString;
    Count : longint;

begin
  Repeat
    Count:=Data.Read(Buf[1],255);
    SetLength(Buf,Count);
    Write('Server got : ',Buf);
  Until (Count=0) or (Pos('QUIT',Buf)<>0);
  Data.Free;
  FServer.StopAccepting;
end;

Procedure TServerApp.OnIdle(Sender : TObject);

begin
  Inc(FCalls);
  Write('.');
end;
Procedure TServerApp.Run;

begin
  Write ('Listening on ');
  if FServer is TUnixServer Then
    Writeln ('socket : ',(FServer as TUnixServer).Filename)
  else If FServer is TINetServer Then
    Writeln ('port : ',(FServer as TInetServer).port);
  If NonBlocking then
    begin
    FServer.SetNonBlocking;
    FServer.OnIdle:=@OnIdle;
    end;
  FServer.StartAccepting;
end;

Var
  Application : TServerApp;
  c : char;

begin
  DoInet:=False;
  NonBlocking:=False;
  repeat
    c:=getopt('ib');
    case c of
    'b' : NonBlocking:=True;
    'i' : DoInet:=True;
    end;
  until c=EndOfOptions;
  If DoInet then
    Application:=TServerApp.Create(ThePort)
  else
    Application:=TServerApp.Create(TheSocket);
  Application.Run;
  Application.Free;
end.
