{ %target=linux,darwin,freebsd,netbsd,openbsd,sunos,beos,haiku }
Program Example59;

{ Program to demonstrate the Alarm function. }

Uses BaseUnix;

Procedure AlarmHandler(Sig : cint);cdecl;

begin
  Writeln ('Got to alarm handler');
end;

begin
  Writeln('Setting alarm handler');
  fpSignal(SIGALRM,SignalHandler(@AlarmHandler));
  Writeln ('Scheduling Alarm in 10 seconds');
  fpAlarm(2);
  Writeln ('Pausing');
  fpPause;
  if fpGetErrno<>ESysEINTR then
    halt(1);
  Writeln ('Pause returned');
end.
