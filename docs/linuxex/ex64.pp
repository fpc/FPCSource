program Example64;

{ Example to demonstrate the SysInfo function.
  Sysinfo is Linux-only. }

{$ifdef Linux}
Uses Linux;

Function Mb(L : Longint) : longint;

begin
  Mb:=L div (1024*1024);
end;

Var Info : TSysInfo;
    D,M,Secs,H : longint;
{$endif}

begin
  {$ifdef Linux}
  If Not SysInfo(Info) then
    Halt(1);
  With Info do
    begin
    D:=Uptime div (3600*24);
    UpTime:=UpTime mod (3600*24);
    h:=uptime div 3600;
    uptime:=uptime mod 3600;
    m:=uptime div 60;
    secs:=uptime mod 60;
    Writeln('Uptime : ',d,'days, ',h,' hours, ',m,' min, ',secs,' s.');
    Writeln('Loads  : ',Loads[1],'/',Loads[2],'/',Loads[3]);
    Writeln('Total Ram  : ',Mb(totalram),'Mb.');
    Writeln('Free Ram   : ',Mb(freeram),'Mb.');
    Writeln('Shared Ram : ',Mb(sharedram),'Mb.');
    Writeln('Buffer Ram : ',Mb(bufferram),'Mb.');
    Writeln('Total Swap : ',Mb(totalswap),'Mb.');
    Writeln('Free Swap  : ',Mb(freeswap),'Mb.');
    end;
  {$endif}
end.
