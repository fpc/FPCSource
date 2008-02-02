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
uses ssockets;


const
  ThePort=4100;

Type
  TINetServerApp = Class(TObject)
  Private
    FServer : TInetServer;
  Public
    Constructor Create(Port : longint);
    Destructor Destroy;override;
    Procedure OnConnect (Sender : TObject; Data : TSocketStream);
    Procedure Run;
  end;

Constructor TInetServerApp.Create(Port : longint);

begin
  FServer:=TINetServer.Create(Port);
  FServer.OnConnect:=@OnConnect;
end;

Destructor TInetServerApp.Destroy;

begin
  FServer.Free;
end;

Procedure TInetServerApp.OnConnect (Sender : TObject; Data : TSocketStream);


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

Procedure TInetServerApp.Run;

begin
  FServer.StartAccepting;
end;

Var
  Application : TInetServerApp;

begin
  Application:=TInetServerApp.Create(ThePort);
  Application.Run;
  Application.Free;
end.
