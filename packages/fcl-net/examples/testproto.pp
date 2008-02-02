{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Marco van de Voort

    test netdb unit, /etc/protocols part

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

program testproto;

uses netdb,Sockets;

Procedure DumpProtoEntry(Const H : TProtocolEntry);

begin
  With H do
    begin
    Writeln('Name     : ',Name);
    Writeln('Number   : ',Number);
    Writeln('Aliases  : ',Aliases);
    Writeln;
    end;
end;

Procedure Testnumber(number : integer);

Var
  H : TProtocolEntry;

begin
  If GetProtocolByNumber(number,H) then
    DumpProtoEntry(H)
  else
    Writeln('No entry for number ',Number)
end;

Procedure TestName(Const N : string);

Var
  H : TProtocolEntry;

begin
  If GetProtocolByName(N,H) then
    DumpProtoEntry(H)
  else
    Writeln('No entry for protocol name ',N)
end;

var i : integer;

begin
  testname('ip');
  testname('IP');
  testname('udp');
  testname('UDP');
  testname('xx');   // should fail
  for i:=0 to 10 do
     testnumber(i);
  testnumber(99);   // should fail
end.
