Program Example15;

{ Program to demonstrate the Nice and Get/SetPriority functions. }

Uses oldlinux;

begin
  writeln ('Setting priority to 5');
  setpriority (prio_process,getpid,5);
  writeln ('New priority = ',getpriority (prio_process,getpid));
  writeln ('Doing nice 10');
  nice (10);
  writeln ('New Priority = ',getpriority (prio_process,getpid));
end.
