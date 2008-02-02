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
  TInetSocket client program. Before running this, run either
  'isocksvr' or 'dsocksvr -i' in another terminal or in the
  background.
}
{$mode objfpc}{$H+}
uses ssockets;

Const
  TheHost = 'localhost';
  ThePort = 4100;

var
  S : String;
  i : longint;

begin
  S:='This is a textstring sent by the client'#10;
  With TInetSocket.Create(TheHost,ThePort) do
    begin
    For I:=1 to 10 do
      Write(S[1],Length(S));
    S:='QUIT'#10;
    Write(S[1],Length(S));
    Free;
    end;
end.
