program testhst;

uses netdb;

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
