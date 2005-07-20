{ Source provided for Free Pascal Bug Report 3695 }
{ Submitted by "Pedro Lopez-Cabanillas" on  2005-02-25 }
{ e-mail: plcl@telefonica.net }
program testsync;

{$ifdef FPC}
    {$mode delphi}
{$endif}

uses
{$ifdef unix}
cthreads,
{$endif unix}
Classes, SysUtils
;

type
    Tester = class
    private
	counter: Integer;
    public
	procedure count;
	procedure run;
    end;

    MyThread = class(TThread)
    private
      worker: Tester;
    public
      constructor Create(w: Tester);
	    procedure Execute; override;
    end;

constructor MyThread.Create(w: Tester);
begin
  worker:= w;
  inherited Create(false);
end;

procedure MyThread.Execute;
begin
  WriteLn('Starting MyThread.Execute');
  repeat
    // sleep(500);
    if worker.counter<10 then
      Synchronize(worker.count);
  until Terminated;
  WriteLn('Ending MyThread.Execute');
end;

procedure Tester.count;
begin
  Inc(counter);
  WriteLn(counter);
end;

procedure Tester.run;
var
  thread: MyThread;
begin
  thread := MyThread.Create(Self);
  While counter<10 do
  begin
    CheckSynchronize(1000);
    //WriteLn('Loop forever inside Tester.run when compiled by FPC 1.9.8');
  end;
  thread.Terminate;
  thread.waitfor;
end;

var
  t: Tester;
begin
  t:=Tester.Create;
  t.run;
  t.free;
end.
