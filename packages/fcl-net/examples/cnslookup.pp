
{$mode objfpc}
uses cnetdb,sockets;

var host : PHostEnt;
    h_addr: in_addr;
    s : ansistring;

begin
  if paramcount<>1 then
    begin
      writeln(stderr,'Usage: nslookup <inet_address>');
      halt(1);
    end;
  s:=paramstr(1);
  host:=gethostbyname(pansichar(s));
  if not assigned(host) then
    begin
      writeln(stderr,'(mini) nslookup failed on ',s);
      halt(1);
    end;
  //writeln(host^.h_addr_list[0]);
  h_addr.s_addr:= pcardinal(host^.h_addr_list[0])^;
  writeln(NetAddrToStr(h_addr));
//            h_addr.s_addr = *((unsigned long *) host->h_addr_list[0]);
//            fprintf(stdout, "%s\n", inet_ntoa(h_addr));
  halt(0);
end.