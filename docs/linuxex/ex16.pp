Program Example16;

{ Program to demonstrate the GetPid, GetPPid function. }

Uses linux;

begin
  Writeln ('Process Id = ',getpid,' Parent process Id = ',getppid);
end.
