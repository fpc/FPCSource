program test_compiler_async;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch anonymousfunctions}

uses
  cthreads, SysUtils, System.Threading, async.fiber, fclasync;

{ Test async procedure directive }
procedure DoBackgroundWork; async;
begin
  Writeln('Async procedure running');
end;

{ Test async function returning IFuture<Integer> }
function ComputeValueAsync: specialize IFuture<Integer>; async;
begin
  Result := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Result := 42;
    end);
end;

function AddTenAsync(const AFuture: specialize IFuture<Integer>): specialize IFuture<Integer>; async;
var
  Val: Integer;
begin
  Val := await AFuture;
  Result := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Result := Val + 10;
    end);
end;

var
  F1, F2: specialize IFuture<Integer>;
  FinalVal: Integer;
  T: ITask;
begin
  Writeln('=== Testing Compiler async & await ===');

  { Test async procedure }
  DoBackgroundWork;

  { Test async function returning future }
  F1 := ComputeValueAsync;

  { Test await operator }
  F2 := AddTenAsync(F1);

  { Await F2 directly }
  FinalVal := await F2;
  Writeln('Final value from await: ', FinalVal);

  if FinalVal <> 52 then
  begin
    Writeln('FAILED: Expected 52, got ', FinalVal);
    Halt(1);
  end;

  { Test await on ITask }
  T := TTask.Run(
    procedure
    begin
      Writeln('Task finished');
    end);
  await T;

  Writeln('SUCCESS: All async/await compiler tests passed!');
end.

