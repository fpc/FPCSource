{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    test netdb unit, host part

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

program testdns;

uses netdb,Sockets;

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
  If ResolveHostByAddr(StrToHostAddr(Addr),H) then
    DumpHostEntry(H)
  else
    Writeln('No entry for address ',Addr)
end;

Procedure TestName(Const N : string);

Var
  H : THostEntry;

begin
  If ResolveHostByName(N,H) then
    DumpHostEntry(H)
  else
    Writeln('No entry for hostname ',N)
end;

Var
  I,l : INteger;
  Ans : Array [1..10] of THostAddr;
  H   : THostAddr;
  NAns : Array[1..10] of String;



begin
  Writeln('Resolving name ');
  l:=ResolveName('db.wisa.be',Ans);
  Writeln('Got : ',l,' answers');
  For I:=1 to l do
    Writeln(i:2,': ',hostAddrtostr(Ans[i]));
  Writeln('Resolving address ');
  H:=StrtoHostAddr('212.224.143.202');
  L:=ResolveAddress(H,NAns);
  Writeln('Got : ',l,' answers');
  For I:=1 to l do
    Writeln(i:2,': ',NAns[i]);
  Writeln('ResolveHostByName:');
  testname('malpertuus.wisa.be');
  Writeln('ResolveHostByAddr:');
  testaddr('212.224.143.202');
end.
