Program Example75;

{ Program to demonstrate the setsid function. }

Uses BaseUnix;

Var
  Pid : pid_t;

begin
  Writeln('Current process group: ',fpgetpgrp);
  // Force non-process group leader.
  Pid:=fpFork;
  if (Pid=0) then
    begin
    Writeln('SetSid returned : ',FpSetSid);
    Writeln('New process group: ',fpgetpgrp);
    end
  else
    Writeln('Child PID :',Pid); 
end.
