Program Example16;

{ Program to demonstrate the GetPid, GetPPid function. }

Uses oldlinux;

begin
  Writeln ('Process Id = ',getpid,' Parent process Id = ',getppid);
end.
