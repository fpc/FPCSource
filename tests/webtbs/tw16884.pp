{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  sysutils, classes;

type
 TThreadChild = class(TThread)
  private
    FThreadState: Integer;
  public
    constructor CreateRace(const ForceFail: Boolean);
    procedure Execute; override;
  end; 

constructor TThreadChild.CreateRace(const ForceFail: Boolean);
begin
  FThreadState := 1;
  inherited Create(False {not suspended}); { the bug is that the inherited call will actually cause execute to be run before the rest of the constructor - serious problem as the thread state may not be initialised properly }

  if ForceFail then
    Sleep(1000); { This will force the issue. -
                   it may not be easily reproducable depending on your OS, CPU thread scheduling.

                   I discovered this on my OSX macbook but my collegue never had the problem on his computer OSX mac mini}

  FThreadState := 2; { this is the final state that we should see in the execute, if we get a 1 in the TThreadChild.Execute, then we know that the execute won the race with the constructor }

  Sleep(200);
end;

var
  ATestFailed: boolean;

procedure TThreadChild.Execute;
var
  ThreadState: Integer;
begin
  ThreadState := FThreadState;

  if ThreadState = 1 then
    begin
      writeln(Format('ThreadState = %d - constructor race condition occured (should be 2)', [FThreadState])); { we should get the Value 2 here every time, not 1. }
      ATestFailed := True;
      readwritebarrier;
    end
  else if ThreadSTate = 2 then
    begin
      writeln(Format('ThreadState = %d - constructor race condition did not occur (should be 2)', [FThreadState]));
    end;
end;

var
  t1, t2, t3: tthread;
begin
  ATestFailed:=false;
  t1:=TThreadChild.createrace(true);
  t2:=TThreadChild.createrace(true);
  t3:=TThreadChild.createrace(true);
  t1.waitfor;
  t1.free;
  t2.waitfor;
  t2.free;
  t3.waitfor;
  t3.free;
  readwritebarrier;
  if ATestFailed then
    halt(1);
end.
