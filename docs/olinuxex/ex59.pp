Program Example59;

{ Program to demonstrate the Alarm function. }

Uses oldlinux;

Procedure AlarmHandler(Sig : longint);cdecl;

begin
  Writeln ('Got to alarm handler');
end;

begin
  Writeln('Setting alarm handler');
  Signal(SIGALRM,@AlarmHandler);
  Writeln ('Scheduling Alarm in 10 seconds');
  Alarm(10);
  Writeln ('Pausing');
  Pause;
  Writeln ('Pause returned');
end.
