Program Example5;
uses Dos;

{ Program to demonstrate the Exec and DosExitCode function. }

begin
{$IFDEF Unix}
  WriteLn('Executing /bin/ls -la');
  Exec('/bin/ls','-la');
{$ELSE}
  WriteLn('Executing Dir');
  Exec(GetEnv('COMSPEC'),'/C dir');
{$ENDIF}
  WriteLn('Program returned with ExitCode ',Lo(DosExitCode));
end.
