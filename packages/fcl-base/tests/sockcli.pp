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
  TUnixSocket client program. Before running this, run either
  'socksvr', or 'dsocksvr' in another terminal
  or in the  background.
}

{$ifndef unix}
  {$fatal This test is only for Unix platforms}
{$endif}

{$mode objfpc}{$H+}
uses ssockets;

var
  S : String;
  i : longint;

begin
  S:='This is a textstring sent by the client'#10;
  With TUnixSocket.Create('ServerSoc') do
    begin
    For I:=1 to 10 do
      Write(S[1],Length(S));
    S:='QUIT'#10;
    Write(S[1],Length(S));
    Free;
    end;
end.
