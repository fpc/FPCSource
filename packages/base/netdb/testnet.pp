program testhst;

uses netdb;

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
