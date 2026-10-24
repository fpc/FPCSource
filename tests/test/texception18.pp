{%FAIL}

{
 Check that exit statement inside 
 finally block is not allowed
}

program texception18;
{$mode objfpc}{$H+}
uses SysUtils;

type
  TTestRecord = record
    A: Integer;
    B: Int64;
    C: Double;
    D: array[0..3] of Byte;
  end;

var
  // Global variables to test interaction with locals
  GlobalInt: Integer = 100;
  GlobalInt64: Int64 = 200;
  GlobalStr: string = 'GlobalString';
  GlobalArray: array[0..9] of Integer;
  GlobalRecord: TTestRecord;

  TestNum: Integer = 0;
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure StartTest(const Name: string);
begin
  Inc(TestNum);
  Write('Test ', TestNum, ': ', Name, ' ... ');
end;

procedure Pass;
begin
  Inc(TestsPassed);
  WriteLn('PASS');
end;

procedure Fail(const Msg: string);
begin
  Inc(TestsFailed);
  WriteLn('FAIL - ', Msg);
end;

{ CRITICAL TEST: Exit inside finally block itself (not in try block) }
{ This is the case that exposed the infinite loop bug! }
function TestExitInFinallyHelper: Boolean;
var
  FinallyRan: Boolean = False;
begin
  Result := False;

  try
    Result := True;  // Set to True before finally
    // Do nothing in try block
  finally
    FinallyRan := True;
    // CRITICAL: Exit is in the finally block itself!
    // Before fix: would infinite loop
    // With fix: should exit cleanly from this function
    Exit;
  end;

  // Should NOT reach here after finally's Exit
  Result := False;
end;

function RunTest_ExitInFinally: Boolean;
begin
  // Helper returns False (because of Exit in finally)
  // But if we can call it without hanging, the test passes
  TestExitInFinallyHelper;
  Result := True;  // If we got here without hanging, success
end;

{ Nested variant: exit in nested finally blocks }
function TestNestedExitInFinallyHelper: Boolean;
begin
  Result := False;
  try
    try
      try
        Result := True;
      finally
        // Exit from innermost finally
        Exit;
      end;
    finally
      Result := False;  // Should not execute if inner finally exits
    end;
  finally
    Result := False;  // Should not execute
  end;
end;

function RunTest_NestedExitInFinally: Boolean;
begin
  // Helper has exit in innermost finally
  // If we can call it without hanging, test passes
  TestNestedExitInFinallyHelper;
  Result := True;  // If we got here without hanging, success
end;

begin
  WriteLn('=== SEH Test Suite (Extended with Many Variables) ===');
  WriteLn;

  WriteLn;
  WriteLn('--- Critical Regression Tests (Exit in Finally Block) ---');
  WriteLn;

  StartTest('Exit inside finally block (not try block)');
  if RunTest_ExitInFinally then Pass else Fail('Finally exit failed');

  StartTest('Nested exit in finally blocks');
  if RunTest_NestedExitInFinally then Pass else Fail('Nested finally exit failed');

  WriteLn;
  WriteLn('=== Results ===');
  WriteLn('Passed: ', TestsPassed);
  WriteLn('Failed: ', TestsFailed);
  WriteLn;

  if TestsFailed = 0 then
    WriteLn('All tests passed!')
  else
    WriteLn('Some tests failed!');

  Halt(TestsFailed);
end.
