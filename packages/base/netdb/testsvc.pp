program testsvc;

uses netdb;

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
  testport(25,'');
  testport(23,'');
  testport(53,'udp');
  testname('mail','');
  testname('ftp','');
  testname('domain','udp');
end.
