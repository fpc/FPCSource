{$mode objfpc}
uses
  process,sysutils;

procedure ExecuteProcess(const Path: string);
var
  P: TProcess;

begin
  P := TProcess.Create(nil);
  try
    writeln('Running ',Path);
    P.Executable:=Path;
    P.Execute;
    P.WaitOnExit(1337);

    while P.Running do
      begin
        P.Terminate(255);
        Writeln(stderr,'Terminate requested for ',Path);
        Sleep(1);
      end;
    writeln(Path,' returned with exit code: ',P.ExitCode);

  finally
    P.Free;
  end;
end;

begin
  ExecuteProcess('./infinity');
  ExecuteProcess('./empty');
end.
