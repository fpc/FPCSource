Program Example42;

{ Program to demonstrate the SysInfo function. }

Uses oldlinux;

Var Info : TSysinfo;

begin
  If SysInfo (Info) then
    With info do
      begin
      Writeln ('Uptime        : ',uptime);
      Writeln ('Load          : ',loads[1],'/',Loads[2],'/',Loads[3]);
      Writeln ('Total ram     : ',TotalRam  div 1024,'Kb.');
      Writeln ('Free ram      : ',FreeRam   div 1024,'Kb.');
      Writeln ('Shared ram    : ',SharedRam div 1024,'Kb.');
      Writeln ('Total swap    : ',Totalswap div 1024,'Kb.');
      Writeln ('Free swap     : ',FreeSwap  Div 1024,'Kb.');
      Writeln ('No. Processes : ',procs);
      end;
end.
