Program Example15;

{ Program to demonstrate the Nice and Get/SetPriority functions. }

Uses BaseUnix,Unix;

begin
  writeln ('Setting priority to 5');
  fpsetpriority (prio_process,fpgetpid,5);
  writeln ('New priority = ',fpgetpriority (prio_process,fpgetpid));
  writeln ('Doing nice 10');
  fpnice (10);
  writeln ('New Priority = ',fpgetpriority (prio_process,fpgetpid));
end.
