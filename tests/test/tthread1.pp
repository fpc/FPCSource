program tthread1;

{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Classes;

type
  TTestThread = class(TThread)
  protected
    procedure Execute; override;
  public
    property ReturnValue;
  end;

procedure TTestThread.Execute;
var
  thrd: TThread;
begin
  thrd := CurrentThread;
  if thrd <> Self then
    ReturnValue := 1
  else
    ReturnValue := 0;
end;

var
  t: TTestThread;
begin
  t := TTestThread.Create(False);
  try
    t.WaitFor;
    ExitCode := t.ReturnValue;
  finally
    t.Free;
  end;
  Writeln(ExitCode);
end.
