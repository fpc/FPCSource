Program Example16;

{ Program to demonstrate the GetPid, GetPPid function. }

Uses BaseUnix;

begin
  Writeln ('Process Id = ',fpgetpid,' Parent process Id = ',fpgetppid);
end.
