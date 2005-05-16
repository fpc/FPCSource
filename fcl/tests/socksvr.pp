Program server;
{
    $Id: socksvr.pp,v 1.5 2005/03/16 13:35:59 marco Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TUnixServerApp server program. This will listen on a socket till
  a client connects. You can connect by running the 'dsockcli' or
  'sockcli' programs in another terminal.
}

{$mode objfpc}{$h+}
uses ssockets;

const
  SPath='ServerSoc';

Type
  TUnixServerApp = Class(TObject)
  Private
    FServer : TUnixServer;
  Public
    Constructor Create(SockName : String);
    Destructor Destroy;override;
    Procedure OnConnect (Sender : TObject; Data : TSocketStream);
    Procedure Run;
  end;

Constructor TUnixServerApp.Create(SockName : String);

begin
  FServer:=TUnixServer.Create(SockName);
  FServer.OnConnect:=@OnConnect;
end;

Destructor TUNixServerApp.Destroy;

begin
  FServer.Free;
end;

Procedure TUnixServerApp.OnConnect (Sender : TObject; Data : TSocketStream);


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

Procedure TUnixServerApp.Run;

begin
  FServer.StartAccepting;
end;

Var
  Application : TUnixServerApp;

begin
  Application:=TUnixServerApp.Create(SPath);
  Application.Run;
  Application.Free;
end.

{
  $Log: socksvr.pp,v $
  Revision 1.5  2005/03/16 13:35:59  marco
   * some fixes for objfpc mode

  Revision 1.4  2005/02/14 17:13:18  peter
    * truncate log

}
