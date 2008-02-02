{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    test netdb unit, hosts part.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testhst;

uses netdb,sockets;

Procedure DumpHostEntry(Const H : THostEntry);

begin
  With H do
    begin
    Writeln('Name     : ',Name);
    Writeln('Addr     : ',HostAddrToStr(Addr));
    Writeln('Aliases  : ',Aliases);
    Writeln;
    end;
end;

Procedure TestAddr(Addr : string);

Var
  H : THostEntry;

begin
  If GetHostByAddr(StrToHostAddr(Addr),H) then
    DumpHostEntry(H)
  else
    Writeln('No entry for address ',Addr)
end;

Procedure TestName(Const N : string);

Var
  H : THostEntry;

begin
  If GetHostByName(N,H) then
    DumpHostEntry(H)
  else
    Writeln('No entry for hostname ',N)
end;

begin
  testaddr('127.0.0.1');
  testaddr('212.224.143.213');
  testname('LOCALHOST');
  testname('www.freepascal.org');
  testname('obelix.wisa.be');
end.
