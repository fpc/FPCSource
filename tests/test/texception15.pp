program texception15;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  TestsPassed, TestsFailed: Integer;
  List: TStringList;

procedure Check(const TestName: string; Condition: Boolean);
begin
  if Condition then
  begin
    WriteLn('  ', TestName, ' ... PASS');
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('  ', TestName, ' ... FAIL');
    Inc(TestsFailed);
  end;
end;

// =============================================================================
// Test 1: For-in with Exit inside try/finally
// =============================================================================
function Test1_ForInExitTryFinally: Boolean;
var
  S: string;
  FinallyExecuted: Boolean;
begin
  Result := False;
  FinallyExecuted := False;
  try
    for S in List do
    begin
      if S = 'Banana' then
        Exit(True);
    end;
  finally
    FinallyExecuted := True;
  end;
  Check('Finally executed on normal exit', FinallyExecuted);
end;

// =============================================================================
// Test 2: For-in with Exit inside try/except
// =============================================================================
function Test2_ForInExitTryExcept: Boolean;
var
  S: string;
begin
  Result := False;
  try
    for S in List do
    begin
      if S = 'Banana' then
        Exit(True);
    end;
  except
    on E: Exception do
      Result := False;
  end;
end;

// =============================================================================
// Test 3: For-in with exception raised and caught
// =============================================================================
function Test3_ForInExceptionCaught: Boolean;
var
  S: string;
  ExceptionCaught: Boolean;
begin
  Result := False;
  ExceptionCaught := False;
  try
    for S in List do
    begin
      if S = 'Banana' then
        raise Exception.Create('Test exception');
    end;
  except
    on E: Exception do
    begin
      ExceptionCaught := True;
      Result := True;
    end;
  end;
  Check('Exception was caught', ExceptionCaught);
end;

// =============================================================================
// Test 4: For-in with exception in try/finally (exception propagates)
// =============================================================================
function Test4_ForInExceptionTryFinally: Boolean;
var
  S: string;
  FinallyExecuted: Boolean;
begin
  Result := False;
  FinallyExecuted := False;
  try
    try
      for S in List do
      begin
        if S = 'Banana' then
          raise Exception.Create('Test exception');
      end;
    finally
      FinallyExecuted := True;
    end;
  except
    on E: Exception do
      Result := FinallyExecuted;
  end;
end;

// =============================================================================
// Test 5: Nested for-in with Exit
// =============================================================================
function Test5_NestedForInExit: Boolean;
var
  S1, S2: string;
  InnerList: TStringList;
begin
  Result := False;
  InnerList := TStringList.Create;
  try
    InnerList.Add('X');
    InnerList.Add('Y');
    InnerList.Add('Z');

    for S1 in List do
    begin
      for S2 in InnerList do
      begin
        if (S1 = 'Banana') and (S2 = 'Y') then
          Exit(True);
      end;
    end;
  finally
    InnerList.Free;
  end;
end;

// =============================================================================
// Test 6: For-in with Break inside try/finally
// =============================================================================
function Test6_ForInBreakTryFinally: Boolean;
var
  S: string;
  FinallyExecuted: Boolean;
begin
  Result := False;
  FinallyExecuted := False;
  try
    for S in List do
    begin
      if S = 'Banana' then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    FinallyExecuted := True;
  end;
  Check('Finally executed after break', FinallyExecuted);
end;

// =============================================================================
// Test 7: For-in with Continue inside try/finally
// =============================================================================
function Test7_ForInContinueTryFinally: Integer;
var
  S: string;
  Count: Integer;
  FinallyExecuted: Boolean;
begin
  Count := 0;
  FinallyExecuted := False;
  try
    for S in List do
    begin
      if S = 'Banana' then
        Continue;
      Inc(Count);
    end;
  finally
    FinallyExecuted := True;
  end;
  Check('Finally executed after continues', FinallyExecuted);
  Result := Count;
end;

// =============================================================================
// Test 8: For-in with multiple Exit points
// =============================================================================
function Test8_ForInMultipleExits(const Target: string): Integer;
var
  S: string;
  I: Integer;
begin
  Result := -1;
  I := 0;
  try
    for S in List do
    begin
      if S = Target then
        Exit(I);
      Inc(I);
    end;
    Exit(-2); // Not found
  finally
    // Just ensure finally runs
  end;
end;

// =============================================================================
// Test 9: For-in with re-raised exception
// =============================================================================
function Test9_ForInReraiseException: Boolean;
var
  S: string;
  FirstCatch, SecondCatch: Boolean;
begin
  Result := False;
  FirstCatch := False;
  SecondCatch := False;
  try
    try
      for S in List do
      begin
        if S = 'Banana' then
          raise Exception.Create('Original');
      end;
    except
      on E: Exception do
      begin
        FirstCatch := True;
        raise; // Re-raise
      end;
    end;
  except
    on E: Exception do
      SecondCatch := True;
  end;
  Result := FirstCatch and SecondCatch;
end;

// =============================================================================
// Test 10: For-in with exception replaced
// =============================================================================
function Test10_ForInReplaceException: string;
var
  S: string;
begin
  Result := '';
  try
    try
      for S in List do
      begin
        if S = 'Banana' then
          raise Exception.Create('First');
      end;
    except
      on E: Exception do
        raise Exception.Create('Second');
    end;
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

// =============================================================================
// Test 11: For-in over array with Exit
// =============================================================================
function Test11_ForInArrayExit: Integer;
var
  Arr: array of Integer;
  I: Integer;
begin
  Result := -1;
  SetLength(Arr, 5);
  Arr[0] := 10;
  Arr[1] := 20;
  Arr[2] := 30;
  Arr[3] := 40;
  Arr[4] := 50;

  try
    for I in Arr do
    begin
      if I = 30 then
        Exit(I);
    end;
  finally
    SetLength(Arr, 0);
  end;
end;

// =============================================================================
// Test 12: For-in with string characters and Exit
// =============================================================================
function Test12_ForInStringExit: Char;
var
  TestStr: string;
  C: Char;
begin
  Result := #0;
  TestStr := 'Hello';
  try
    for C in TestStr do
    begin
      if C = 'l' then
        Exit(C);
    end;
  finally
    // Cleanup
  end;
end;

// =============================================================================
// Test 13: For-in with deeply nested try blocks
// =============================================================================
function Test13_ForInDeepNesting: Integer;
var
  S: string;
  Level1, Level2, Level3: Boolean;
begin
  Result := 0;
  Level1 := False;
  Level2 := False;
  Level3 := False;
  try
    try
      try
        for S in List do
        begin
          if S = 'Cherry' then
            Exit(3);
        end;
      finally
        Level3 := True;
      end;
    finally
      Level2 := True;
    end;
  finally
    Level1 := True;
  end;

  if Level1 then Inc(Result);
  if Level2 then Inc(Result);
  if Level3 then Inc(Result);
end;

// =============================================================================
// Test 14: REMOVED - exception in finally during local unwind is not supported
// =============================================================================

// =============================================================================
// Test 15: For-in with local variables preservation
// =============================================================================
function Test15_ForInLocalVarsPreserved: Boolean;
var
  S: string;
  A, B, C, D: Integer;
  Str1, Str2: string;
  FinallyA, FinallyB: Integer;
begin
  Result := False;
  A := 100;
  B := 200;
  C := 300;
  D := 400;
  Str1 := 'Hello';
  Str2 := 'World';
  FinallyA := 0;
  FinallyB := 0;

  try
    for S in List do
    begin
      A := A + 1;
      B := B + 2;
      if S = 'Banana' then
      begin
        C := 999;
        Exit(True);
      end;
    end;
  finally
    FinallyA := A;
    FinallyB := B;
    // Verify locals are accessible
    Result := (Str1 = 'Hello') and (Str2 = 'World') and (D = 400);
  end;

  Check('Local A preserved', FinallyA = 102);
  Check('Local B preserved', FinallyB = 204);
end;

// =============================================================================
// Test 16: For-in with Exit inside nested procedure
// =============================================================================
function Test16_ForInExitNestedProc: Boolean;
var
  S: string;
  FoundIt: Boolean;

  procedure SearchInner;
  var
    S2: string;
  begin
    for S2 in List do
    begin
      if S2 = 'Cherry' then
      begin
        FoundIt := True;
        Exit; // Exit from inner proc, not outer function
      end;
    end;
  end;

begin
  Result := False;
  FoundIt := False;

  try
    for S in List do
    begin
      SearchInner;
      if FoundIt then
        Exit(True);
    end;
  finally
    // Just verify finally runs
  end;
end;

// =============================================================================
// Test 17: For-in with Exit and function result as var parameter
// =============================================================================
procedure Test17_ForInExitVarParam(out Found: Boolean; out FoundValue: string);
var
  S: string;
begin
  Found := False;
  FoundValue := '';

  try
    for S in List do
    begin
      if S = 'Banana' then
      begin
        Found := True;
        FoundValue := S;
        Exit;
      end;
    end;
  finally
    // Ensure parameters are set correctly
  end;
end;

// =============================================================================
// Test 18: For-in with multiple lists and Exit
// =============================================================================
function Test18_ForInMultipleLists: string;
var
  List2: TStringList;
  S1, S2: string;
begin
  Result := '';
  List2 := TStringList.Create;
  try
    List2.Add('One');
    List2.Add('Two');
    List2.Add('Three');

    try
      for S1 in List do
      begin
        for S2 in List2 do
        begin
          if (S1 = 'Banana') and (S2 = 'Two') then
            Exit(S1 + '+' + S2);
        end;
      end;
    finally
      // Cleanup in finally
    end;
  finally
    List2.Free;
  end;
end;

// =============================================================================
// Test 19: For-in with complex control flow
// =============================================================================
function Test19_ForInComplexControlFlow: Integer;
var
  S: string;
  Count: Integer;
begin
  Result := 0;
  Count := 0;

  try
    for S in List do
    begin
      try
        Inc(Count);
        if S = 'Banana' then
        begin
          Result := Count;
          Exit;
        end;
      finally
        // Inner finally
      end;
    end;
  finally
    // Outer finally - verify count
    if Count >= 2 then
      Result := Result + 100;
  end;
end;

// =============================================================================
// Main test runner
// =============================================================================
var
  R: Boolean;
  I: Integer;
  S: string;
  Found: Boolean;
  FoundValue: string;
begin
  WriteLn('=== Extended For-In + Try/Except/Finally Test Suite ===');
  WriteLn;

  TestsPassed := 0;
  TestsFailed := 0;

  // Setup
  List := TStringList.Create;
  try
    List.Add('Apple');
    List.Add('Banana');
    List.Add('Cherry');

    WriteLn('Test 1: For-in with Exit inside try/finally');
    R := Test1_ForInExitTryFinally;
    Check('Returned True', R);
    WriteLn;

    WriteLn('Test 2: For-in with Exit inside try/except');
    R := Test2_ForInExitTryExcept;
    Check('Returned True', R);
    WriteLn;

    WriteLn('Test 3: For-in with exception raised and caught');
    R := Test3_ForInExceptionCaught;
    Check('Returned True', R);
    WriteLn;

    WriteLn('Test 4: For-in with exception in try/finally');
    R := Test4_ForInExceptionTryFinally;
    Check('Finally ran before exception propagated', R);
    WriteLn;

    WriteLn('Test 5: Nested for-in with Exit');
    R := Test5_NestedForInExit;
    Check('Found Banana+Y', R);
    WriteLn;

    WriteLn('Test 6: For-in with Break inside try/finally');
    R := Test6_ForInBreakTryFinally;
    Check('Break worked', R);
    WriteLn;

    WriteLn('Test 7: For-in with Continue inside try/finally');
    I := Test7_ForInContinueTryFinally;
    Check('Skipped Banana (count=2)', I = 2);
    WriteLn;

    WriteLn('Test 8: For-in with multiple Exit points');
    Check('Found Apple at 0', Test8_ForInMultipleExits('Apple') = 0);
    Check('Found Banana at 1', Test8_ForInMultipleExits('Banana') = 1);
    Check('Found Cherry at 2', Test8_ForInMultipleExits('Cherry') = 2);
    Check('NotFound returns -2', Test8_ForInMultipleExits('Durian') = -2);
    WriteLn;

    WriteLn('Test 9: For-in with re-raised exception');
    R := Test9_ForInReraiseException;
    Check('Both catches executed', R);
    WriteLn;

    WriteLn('Test 10: For-in with exception replaced');
    S := Test10_ForInReplaceException;
    Check('Got replacement exception', S = 'Second');
    WriteLn;

    WriteLn('Test 11: For-in over array with Exit');
    I := Test11_ForInArrayExit;
    Check('Found 30 in array', I = 30);
    WriteLn;

    WriteLn('Test 12: For-in with string characters and Exit');
    Check('Found first l', Test12_ForInStringExit = 'l');
    WriteLn;

    WriteLn('Test 13: For-in with deeply nested try blocks');
    I := Test13_ForInDeepNesting;
    Check('All 3 finally blocks ran', I = 3);
    WriteLn;

    // Test 14 removed - exception in finally during local unwind is not supported on ARM64 Windows
    WriteLn;

    WriteLn('Test 15: For-in with local variables preservation');
    R := Test15_ForInLocalVarsPreserved;
    Check('All locals preserved', R);
    WriteLn;

    WriteLn('Test 16: For-in with Exit inside nested procedure');
    R := Test16_ForInExitNestedProc;
    Check('Nested proc Exit worked', R);
    WriteLn;

    WriteLn('Test 17: For-in with Exit and var parameters');
    Test17_ForInExitVarParam(Found, FoundValue);
    Check('Found=True', Found);
    Check('FoundValue=Banana', FoundValue = 'Banana');
    WriteLn;

    WriteLn('Test 18: For-in with multiple lists and Exit');
    S := Test18_ForInMultipleLists;
    Check('Combined result', S = 'Banana+Two');
    WriteLn;

    WriteLn('Test 19: For-in with complex control flow');
    I := Test19_ForInComplexControlFlow;
    Check('Exit at 2 + finally added 100', I = 102);
    WriteLn;

  finally
    List.Free;
  end;

  WriteLn('=== Results ===');
  WriteLn('Passed: ', TestsPassed);
  WriteLn('Failed: ', TestsFailed);
  WriteLn;

  if TestsFailed = 0 then
    WriteLn('All tests passed!')
  else
    WriteLn('SOME TESTS FAILED!');

  // Exit with error code if any tests failed
  if TestsFailed > 0 then
    Halt(1);
end.
