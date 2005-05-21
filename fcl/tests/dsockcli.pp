Program Client;
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
  Dual socket client program. Before running this, run either
  'isocksvr', 'dsocksvr' or 'dsocksvr -i' in another terminal
  or in the  background.

  Make sure you run this with the same protocol as the server.
}

{$mode objfpc}{$H+}
uses ssockets;

Const
  TheSocket = 'ServerSoc';
  TheHost = 'localhost';
  ThePort = 4100;

var
  Stream : TSocketStream;
  S : String;
  i : longint;

begin
  S:='This is a textstring sent by the client'#10;
  If (ParamCount=1) and (paramstr(1)='-i') then
    Stream:=TInetSocket.Create(TheHost,ThePort)
  else
    Stream:=TUnixSocket.Create(TheSocket);
  With Stream do
    begin
    For I:=1 to 10 do
      Write(S[1],Length(S));
    S:='QUIT'#10;
    Write(S[1],Length(S));
    Free;
    end;
end.
