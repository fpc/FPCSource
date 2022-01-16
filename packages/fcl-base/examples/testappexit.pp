program testappexit;

uses sysutils,custapp;

type
  TApplication = Class(TCustomApplication)
    Procedure DoRun; override;
  end;
  
Procedure TApplication.DoRun;

begin
  ExceptionExitCode:=9;
  If ParamStr(1)='-h' then
    Terminate(10)
  else if Paramstr(1)='-e' then
    Raise Exception.Create('Stopping with exception')
  else
    Writeln('Normal stop');  
  Terminate;  
end;

begin
  With TApplication.Create(Nil) do
    try
      StopOnException:=True;
      Initialize;
      Run;
    finally
      Free;
    end;     
end.