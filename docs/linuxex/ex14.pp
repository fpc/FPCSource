Program Example14;

{ Program to demonstrate the Fork and WaitPidfunction. }

Uses BaseUnix;

Var PID, ExitStatus : cint;
  
begin
  Writeln ('Spawning a child');
  PID:=fpFork;
  If PID=0 then
    begin 
    Writeln ('Hello From the Child !!');
    Writeln ('Exiting with exit status 1 !');
    Halt (1);
    end
  Else 
    begin
    Writeln ('Spawned child with PID : ',PID);
    fpWaitPid (PID,@ExitStatus,0);
    Writeln ('Child exited with status : ',ExitStatus shr 8);
    end; 
end.
