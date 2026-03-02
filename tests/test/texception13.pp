{$mode objfpc}
program texception13;

{ Minimal test to trace exactly what RtlUnwindEx does }

uses
  SysUtils;

var
  GFinallyExecuted: Boolean = False;

procedure TestSimple;
begin
  WriteLn('1. Entering try block');
  try
    WriteLn('2. Inside try, about to Exit');
    Exit;
    WriteLn('X. ERROR: After Exit (should not print)');
  finally
    WriteLn('3. FINALLY BLOCK');
    GFinallyExecuted := True;
  end;
  WriteLn('X. ERROR: After try/finally (should not print)');
end;

begin
  WriteLn('=== Minimal Local Unwind Test ===');
  WriteLn('Starting test...');

  TestSimple;

  WriteLn('4. After TestSimple returned');
  if GFinallyExecuted then
    WriteLn('SUCCESS: Finally was executed')
  else
    WriteLn('FAILURE: Finally was NOT executed');

  WriteLn('5. Program ending normally');
end.
