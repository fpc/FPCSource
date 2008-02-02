{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    test netdb unit, services part

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testsvc;

uses netdb,sockets;

Procedure DumpServiceEntry(Const E : TserviceEntry);

begin
  With E do
    begin
    Writeln('Name     : ',Name);
    Writeln('Protocol : ',Protocol);
    Writeln('Port     : ',Port);
    Writeln('Aliases  : ',Aliases);
    Writeln;
    end;
end;

Procedure TestPort(P : Word; Const Proto : string);

Var
  E : TServiceEntry;

begin
  If GetServiceByPort(P,Proto,E) then
    DumpServiceEntry(E)
  else
    Writeln('No entry for port ',P)
end;

Procedure TestName(Const N,Proto : string);

Var
  E : TServiceEntry;

begin
  If GetServiceByName(N,Proto,E) then
    DumpServiceEntry(E)
  else
    Writeln('No entry for service ',N)
end;

begin
  testport(80,'tcp');
  testport(25,'');
  testport(23,'');
  testport(53,'udp');
  testname('mail','');
  testname('ftp','');
  testname('domain','udp');
end.
