{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    test netdb unit, network part

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testhst;

uses netdb,sockets;

Procedure DumpNetEntry(Const N : TNetworkEntry);

begin
  With N do
    begin
    Writeln('Name     : ',Name);
    Writeln('Addr     : ',HostAddrToStr(Addr));
    Writeln('Aliases  : ',Aliases);
    Writeln;
    end;
end;

Procedure TestAddr(Addr : string);

Var
  N : TNetworkEntry;

begin
  If GetNetworkByAddr(StrToHostAddr(Addr),N) then
    DumpNetEntry(N)
  else
    Writeln('No entry for address ',Addr)
end;

Procedure TestName(Const Net : string);

Var
  N : TNetworkEntry;

begin
  If GetNetworkByName(Net,N) then
    DumpNetEntry(N)
  else
    Writeln('No entry for netname ',Net)
end;

begin
  testaddr('127.0.0.0');
  testname('loopback');
end.
