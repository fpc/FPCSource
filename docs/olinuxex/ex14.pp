Program Example14;

{ Program to demonstrate the Fork and WaitPidfunction. }

Uses oldlinux;

Var PID, ExitStatus : Longint;

begin
  Writeln ('Spawning a child');
  PID:=Fork;
  If PID=0 then
    begin
    Writeln ('Hello From the Child !!');
    Writeln ('Exiting with exit status 1 !');
    Halt (1);
    end
  Else
    begin
    Writeln ('Spawned child with PID : ',PID);
    WaitPid (PID,@ExitStatus,0);
    Writeln ('Child exited with status : ',ExitStatus shr 8);
    end;
end.
