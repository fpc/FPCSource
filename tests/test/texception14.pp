program texception14;
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

// Test 1: Basic exception handling
procedure Test_BasicException;
var
  FinallyRan: Boolean = False;
  ExceptRan: Boolean = False;
begin
  StartTest('Basic exception with try/except/finally');
  try
    try
      raise Exception.Create('Test exception');
    except
      ExceptRan := True;
    end;
  finally
    FinallyRan := True;
  end;

  if ExceptRan and FinallyRan then
    Pass
  else
    Fail('ExceptRan=' + BoolToStr(ExceptRan, True) + ' FinallyRan=' + BoolToStr(FinallyRan, True));
end;

// Test 2: Exit inside try/finally
procedure Test_ExitInTryFinally;
var
  FinallyRan: Boolean = False;
begin
  StartTest('Exit inside try/finally');
  try
    FinallyRan := False;
    Exit;
  finally
    FinallyRan := True;
  end;
end;

function RunTest_ExitInTryFinally: Boolean;
begin
  Result := False;
  try
    try
      Exit;
    finally
      Result := True;
    end;
  except
    Result := False;
  end;
end;

// Test 3: Break inside try/finally in loop
function RunTest_BreakInTryFinally: Boolean;
var
  I: Integer;
  FinallyCount: Integer = 0;
begin
  Result := False;
  for I := 1 to 5 do
  begin
    try
      if I = 3 then
        Break;
    finally
      Inc(FinallyCount);
    end;
  end;
  Result := (FinallyCount = 3);  // Finally should run for iterations 1, 2, 3
end;

// Test 4: Continue inside try/finally in loop
function RunTest_ContinueInTryFinally: Boolean;
var
  I: Integer;
  FinallyCount: Integer = 0;
  BodyCount: Integer = 0;
begin
  Result := False;
  for I := 1 to 5 do
  begin
    try
      if I mod 2 = 0 then
        Continue;
      Inc(BodyCount);
    finally
      Inc(FinallyCount);
    end;
  end;
  // Continue should run finally before continuing
  // BodyCount should be 3 (iterations 1, 3, 5)
  // FinallyCount should be 5 (all iterations)
  Result := (FinallyCount = 5) and (BodyCount = 3);
end;

// Test 5: Nested try/finally with exit
function RunTest_NestedTryFinallyExit: Boolean;
var
  Outer: Boolean = False;
  Inner: Boolean = False;
begin
  Result := False;
  try
    try
      Exit;
    finally
      Inner := True;
    end;
  finally
    Outer := True;
    Result := Inner and Outer;
  end;
end;

// Test 6: Exception in nested try blocks
function RunTest_NestedExceptions: Boolean;
var
  OuterFinally: Boolean = False;
  InnerFinally: Boolean = False;
  ExceptRan: Boolean = False;
begin
  Result := False;
  try
    try
      try
        raise Exception.Create('Inner');
      finally
        InnerFinally := True;
      end;
    except
      ExceptRan := True;
    end;
  finally
    OuterFinally := True;
  end;
  Result := InnerFinally and ExceptRan and OuterFinally;
end;

// Test 7: Re-raise exception
function RunTest_ReraiseException: Boolean;
var
  FirstCatch: Boolean = False;
  SecondCatch: Boolean = False;
  FinallyRan: Boolean = False;
begin
  Result := False;
  try
    try
      try
        raise Exception.Create('Original');
      except
        FirstCatch := True;
        raise;  // Re-raise
      end;
    finally
      FinallyRan := True;
    end;
  except
    SecondCatch := True;
  end;
  Result := FirstCatch and SecondCatch and FinallyRan;
end;

// Test 8: Exit with result in try/finally
function GetValueWithExit: Integer;
begin
  Result := 0;
  try
    Result := 42;
    Exit;
    Result := 99;  // Should not execute
  finally
    // Result should still be 42
  end;
end;

function RunTest_ExitWithResult: Boolean;
begin
  Result := (GetValueWithExit = 42);
end;

// Test 9: Multiple exits in try/finally
function MultipleExits(N: Integer): Integer;
begin
  Result := 0;
  try
    if N = 1 then
    begin
      Result := 10;
      Exit;
    end;
    if N = 2 then
    begin
      Result := 20;
      Exit;
    end;
    Result := 30;
  finally
    Result := Result + 1;  // Should always add 1
  end;
end;

function RunTest_MultipleExits: Boolean;
begin
  Result := (MultipleExits(1) = 11) and (MultipleExits(2) = 21) and (MultipleExits(3) = 31);
end;

// Test 10: Deep nesting with exit
function RunTest_DeepNesting: Boolean;
var
  Level1, Level2, Level3: Boolean;
begin
  Level1 := False;
  Level2 := False;
  Level3 := False;
  Result := False;
  try
    try
      try
        Exit;
      finally
        Level3 := True;
      end;
    finally
      Level2 := True;
    end;
  finally
    Level1 := True;
    Result := Level1 and Level2 and Level3;
  end;
end;

// ============================================================================
// NEW TESTS: Many local variables to stress-test stack layout
// ============================================================================

// Test 11: Exit with MANY local variables (different sizes)
function RunTest_ManyLocalsExit: Boolean;
var
  // Various sized local variables to create complex stack layout
  LocalByte1: Byte;
  LocalByte2: Byte;
  LocalByte3: Byte;
  LocalWord1: Word;
  LocalWord2: Word;
  LocalInt1: Integer;
  LocalInt2: Integer;
  LocalInt3: Integer;
  LocalInt4: Integer;
  LocalInt64_1: Int64;
  LocalInt64_2: Int64;
  LocalDouble1: Double;
  LocalDouble2: Double;
  LocalPtr1: Pointer;
  LocalPtr2: Pointer;
  LocalArray: array[0..15] of Byte;
  LocalRecord: TTestRecord;
  FinallyRan: Boolean;
  I: Integer;
begin
  Result := False;
  FinallyRan := False;

  // Initialize all variables with known values
  LocalByte1 := 11;
  LocalByte2 := 22;
  LocalByte3 := 33;
  LocalWord1 := 1111;
  LocalWord2 := 2222;
  LocalInt1 := 100001;
  LocalInt2 := 100002;
  LocalInt3 := 100003;
  LocalInt4 := 100004;
  LocalInt64_1 := Int64($123456789ABCDEF0);
  LocalInt64_2 := Int64($FEDCBA9876543210);
  LocalDouble1 := 3.14159265358979;
  LocalDouble2 := 2.71828182845904;
  LocalPtr1 := Pointer($DEADBEEF);
  LocalPtr2 := Pointer($CAFEBABE);
  for I := 0 to 15 do
    LocalArray[I] := I * 10;
  LocalRecord.A := 999;
  LocalRecord.B := Int64($AABBCCDD11223344);
  LocalRecord.C := 1.41421356237;
  LocalRecord.D[0] := $AA;
  LocalRecord.D[1] := $BB;
  LocalRecord.D[2] := $CC;
  LocalRecord.D[3] := $DD;

  try
    // Verify values before exit
    if LocalInt1 <> 100001 then Exit;
    if LocalInt64_1 <> Int64($123456789ABCDEF0) then Exit;

    // Now exit - finally should run and preserve all values
    Exit;
  finally
    FinallyRan := True;

    // Verify all values are still correct in finally block
    if LocalByte1 <> 11 then FinallyRan := False;
    if LocalByte2 <> 22 then FinallyRan := False;
    if LocalByte3 <> 33 then FinallyRan := False;
    if LocalWord1 <> 1111 then FinallyRan := False;
    if LocalWord2 <> 2222 then FinallyRan := False;
    if LocalInt1 <> 100001 then FinallyRan := False;
    if LocalInt2 <> 100002 then FinallyRan := False;
    if LocalInt3 <> 100003 then FinallyRan := False;
    if LocalInt4 <> 100004 then FinallyRan := False;
    if LocalInt64_1 <> Int64($123456789ABCDEF0) then FinallyRan := False;
    if LocalInt64_2 <> Int64($FEDCBA9876543210) then FinallyRan := False;
    if LocalArray[0] <> 0 then FinallyRan := False;
    if LocalArray[15] <> 150 then FinallyRan := False;
    if LocalRecord.A <> 999 then FinallyRan := False;
    if LocalRecord.B <> Int64($AABBCCDD11223344) then FinallyRan := False;
    if LocalRecord.D[0] <> $AA then FinallyRan := False;

    Result := FinallyRan;
  end;
end;

// Test 12: Break with many locals and global interaction
function RunTest_ManyLocalsBreak: Boolean;
var
  LocalArr1: array[0..7] of Integer;
  LocalArr2: array[0..7] of Int64;
  LocalStr: string;
  LocalInt1, LocalInt2, LocalInt3, LocalInt4: Integer;
  LocalInt5, LocalInt6, LocalInt7, LocalInt8: Integer;
  FinallyCount: Integer;
  I, J: Integer;
  ExpectedSum: Integer;
begin
  Result := False;
  FinallyCount := 0;

  // Initialize local arrays
  for I := 0 to 7 do
  begin
    LocalArr1[I] := I * 100;
    LocalArr2[I] := Int64(I) * Int64(1000000000);
  end;
  LocalStr := 'TestString123';
  LocalInt1 := 1; LocalInt2 := 2; LocalInt3 := 3; LocalInt4 := 4;
  LocalInt5 := 5; LocalInt6 := 6; LocalInt7 := 7; LocalInt8 := 8;

  // Also modify global to verify it's not corrupted
  GlobalInt := 12345;
  GlobalInt64 := Int64($1122334455667788);

  for I := 1 to 5 do
  begin
    try
      if I = 3 then
      begin
        // Verify locals before break
        if LocalArr1[5] <> 500 then Exit;
        if LocalInt4 <> 4 then Exit;
        if LocalStr <> 'TestString123' then Exit;
        Break;
      end;
    finally
      Inc(FinallyCount);
      // Verify locals in finally
      if LocalArr1[7] <> 700 then FinallyCount := -100;
      if LocalInt8 <> 8 then FinallyCount := -100;
      // Verify globals
      if GlobalInt <> 12345 then FinallyCount := -100;
      if GlobalInt64 <> Int64($1122334455667788) then FinallyCount := -100;
    end;
  end;

  // Verify everything after loop
  ExpectedSum := LocalInt1 + LocalInt2 + LocalInt3 + LocalInt4 +
                 LocalInt5 + LocalInt6 + LocalInt7 + LocalInt8;
  if ExpectedSum <> 36 then Exit;
  if LocalArr2[3] <> Int64(3000000000) then Exit;

  Result := (FinallyCount = 3);
end;

// Test 13: Continue with many locals
function RunTest_ManyLocalsContinue: Boolean;
var
  LocalRec1, LocalRec2: TTestRecord;
  LocalArr: array[0..31] of Byte;
  LocalD1, LocalD2, LocalD3: Double;
  FinallyCount: Integer;
  BodyCount: Integer;
  I: Integer;
begin
  Result := False;
  FinallyCount := 0;
  BodyCount := 0;

  // Initialize records
  LocalRec1.A := 111;
  LocalRec1.B := 222;
  LocalRec1.C := 333.333;
  LocalRec2.A := 444;
  LocalRec2.B := 555;
  LocalRec2.C := 666.666;

  // Initialize array
  for I := 0 to 31 do
    LocalArr[I] := I;

  LocalD1 := 1.1;
  LocalD2 := 2.2;
  LocalD3 := 3.3;

  for I := 1 to 5 do
  begin
    try
      if I mod 2 = 0 then
      begin
        // Verify before continue
        if LocalRec1.A <> 111 then Exit;
        if LocalArr[20] <> 20 then Exit;
        Continue;
      end;
      Inc(BodyCount);
      // Modify something to ensure we're really in the loop
      LocalD1 := LocalD1 + 0.1;
    finally
      Inc(FinallyCount);
      // Verify in finally
      if LocalRec2.A <> 444 then FinallyCount := -100;
      if LocalArr[31] <> 31 then FinallyCount := -100;
      if LocalD2 < 2.0 then FinallyCount := -100;
    end;
  end;

  Result := (FinallyCount = 5) and (BodyCount = 3);
end;

// Test 14: Multiple exits with many locals and Result modification in finally
function RunTest_ManyLocalsMultipleExits(N: Integer): Integer;
var
  Pad1: array[0..63] of Byte;  // Padding to push Result further on stack
  LocalInt1, LocalInt2, LocalInt3: Integer;
  LocalInt64: Int64;
  LocalStr: string;
  Pad2: array[0..31] of Integer;  // More padding
  I: Integer;
begin
  Result := 0;

  // Initialize padding and locals
  for I := 0 to 63 do Pad1[I] := I;
  for I := 0 to 31 do Pad2[I] := I * 1000;

  LocalInt1 := 10000;
  LocalInt2 := 20000;
  LocalInt3 := 30000;
  LocalInt64 := Int64($FFFFFFFFFFFFFFFF);
  LocalStr := 'ExitTest';

  try
    if N = 1 then
    begin
      Result := 100;
      // Verify locals before exit
      if LocalInt1 <> 10000 then Result := -1;
      if Pad1[32] <> 32 then Result := -1;
      Exit;
    end;
    if N = 2 then
    begin
      Result := 200;
      if LocalInt2 <> 20000 then Result := -1;
      if Pad2[16] <> 16000 then Result := -1;
      Exit;
    end;
    Result := 300;
  finally
    // Verify locals in finally
    if LocalInt3 <> 30000 then Result := -1000;
    if LocalInt64 <> Int64($FFFFFFFFFFFFFFFF) then Result := -1000;
    if LocalStr <> 'ExitTest' then Result := -1000;
    if Pad1[63] <> 63 then Result := -1000;
    if Pad2[31] <> 31000 then Result := -1000;

    // Modify Result in finally
    if Result > 0 then
      Result := Result + 1;
  end;
end;

function RunTest_ManyLocalsMultipleExitsWrapper: Boolean;
var
  R1, R2, R3: Integer;
begin
  R1 := RunTest_ManyLocalsMultipleExits(1);
  R2 := RunTest_ManyLocalsMultipleExits(2);
  R3 := RunTest_ManyLocalsMultipleExits(3);
  Result := (R1 = 101) and (R2 = 201) and (R3 = 301);
  if not Result then
    WriteLn('  Values: R1=', R1, ' R2=', R2, ' R3=', R3);
end;

// Test 15: Deep nesting with many locals at each level
function RunTest_DeepNestingManyLocals: Boolean;
var
  OuterArr: array[0..15] of Integer;
  MiddleArr: array[0..15] of Integer;
  InnerArr: array[0..15] of Integer;
  OuterInt1, OuterInt2: Integer;
  MiddleInt1, MiddleInt2: Integer;
  InnerInt1, InnerInt2: Integer;
  OuterRan, MiddleRan, InnerRan: Boolean;
  I: Integer;
begin
  Result := False;
  OuterRan := False;
  MiddleRan := False;
  InnerRan := False;

  for I := 0 to 15 do OuterArr[I] := I * 11;
  OuterInt1 := 11111;
  OuterInt2 := 22222;

  try
    // Initialize middle level locals
    for I := 0 to 15 do MiddleArr[I] := I * 22;
    MiddleInt1 := 33333;
    MiddleInt2 := 44444;

    try
      // Initialize inner level locals
      for I := 0 to 15 do InnerArr[I] := I * 33;
      InnerInt1 := 55555;
      InnerInt2 := 66666;

      try
        // Verify all levels before exit
        if OuterArr[10] <> 110 then Exit;
        if MiddleArr[10] <> 220 then Exit;
        if InnerArr[10] <> 330 then Exit;
        if OuterInt1 <> 11111 then Exit;
        if MiddleInt1 <> 33333 then Exit;
        if InnerInt1 <> 55555 then Exit;

        Exit;
      finally
        InnerRan := True;
        // Verify inner locals
        if InnerArr[15] <> 495 then InnerRan := False;
        if InnerInt2 <> 66666 then InnerRan := False;
        // Also verify we can see outer locals
        if OuterInt2 <> 22222 then InnerRan := False;
      end;
    finally
      MiddleRan := InnerRan;
      // Verify middle locals
      if MiddleArr[15] <> 330 then MiddleRan := False;
      if MiddleInt2 <> 44444 then MiddleRan := False;
    end;
  finally
    OuterRan := MiddleRan;
    // Verify outer locals
    if OuterArr[15] <> 165 then OuterRan := False;
    if OuterInt1 <> 11111 then OuterRan := False;
    Result := OuterRan;
  end;
end;

// Test 16: String manipulation in finally with many locals
function RunTest_StringsInFinally: Boolean;
var
  LocalStr1, LocalStr2, LocalStr3: string;
  LocalArr: array[0..99] of Byte;
  LocalInt: Integer;
  FinallyRan: Boolean;
  I: Integer;
begin
  Result := False;
  FinallyRan := False;

  LocalStr1 := 'First';
  LocalStr2 := 'Second';
  LocalStr3 := 'Third';
  for I := 0 to 99 do LocalArr[I] := I mod 256;
  LocalInt := 777;

  try
    LocalStr1 := LocalStr1 + '_Modified';
    if LocalStr1 <> 'First_Modified' then Exit;
    Exit;
  finally
    FinallyRan := True;
    // Modify strings in finally
    LocalStr2 := LocalStr2 + '_InFinally';
    LocalStr3 := 'Replaced';

    // Verify everything
    if LocalStr1 <> 'First_Modified' then FinallyRan := False;
    if LocalStr2 <> 'Second_InFinally' then FinallyRan := False;
    if LocalStr3 <> 'Replaced' then FinallyRan := False;
    if LocalArr[50] <> 50 then FinallyRan := False;
    if LocalInt <> 777 then FinallyRan := False;

    Result := FinallyRan;
  end;
end;

// Test 17: Global variable modification in finally
function RunTest_GlobalsInFinally: Boolean;
var
  LocalInt: Integer;
  I: Integer;
begin
  Result := False;

  // Set globals to known values
  GlobalInt := 50000;
  GlobalInt64 := Int64($ABCDEF0123456789);
  GlobalStr := 'BeforeFinally';
  for I := 0 to 9 do GlobalArray[I] := I * 100;
  GlobalRecord.A := 12345;
  GlobalRecord.B := Int64($1111222233334444);

  LocalInt := 99999;

  try
    // Verify globals before exit
    if GlobalInt <> 50000 then Exit;
    if GlobalArray[5] <> 500 then Exit;
    Exit;
  finally
    // Modify globals in finally
    GlobalInt := GlobalInt + 1;
    GlobalInt64 := GlobalInt64 + 1;
    GlobalStr := 'AfterFinally';
    GlobalArray[9] := 9999;
    GlobalRecord.A := GlobalRecord.A + 1;

    // Verify local is still accessible
    if LocalInt <> 99999 then
      GlobalInt := -1;

    Result := (GlobalInt = 50001) and
              (GlobalInt64 = Int64($ABCDEF012345678A)) and
              (GlobalStr = 'AfterFinally') and
              (GlobalArray[9] = 9999) and
              (GlobalRecord.A = 12346);
  end;
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

  // Original tests
  Test_BasicException;

  StartTest('Exit inside try/finally');
  if RunTest_ExitInTryFinally then Pass else Fail('Finally did not run');

  StartTest('Break inside try/finally in loop');
  if RunTest_BreakInTryFinally then Pass else Fail('Finally count mismatch');

  StartTest('Continue inside try/finally in loop');
  if RunTest_ContinueInTryFinally then Pass else Fail('Finally/body count mismatch');

  StartTest('Nested try/finally with exit');
  if RunTest_NestedTryFinallyExit then Pass else Fail('Not all finally blocks ran');

  StartTest('Nested exceptions');
  if RunTest_NestedExceptions then Pass else Fail('Exception handling failed');

  StartTest('Re-raise exception');
  if RunTest_ReraiseException then Pass else Fail('Re-raise failed');

  StartTest('Exit with result value');
  if RunTest_ExitWithResult then Pass else Fail('Result was modified incorrectly');

  StartTest('Multiple exits in try/finally');
  if RunTest_MultipleExits then Pass else Fail('Result values incorrect');

  StartTest('Deep nesting with exit');
  if RunTest_DeepNesting then Pass else Fail('Not all finally blocks ran');

  WriteLn;
  WriteLn('--- Extended Tests with Many Local Variables ---');
  WriteLn;

  StartTest('Exit with MANY local variables');
  if RunTest_ManyLocalsExit then Pass else Fail('Local variables corrupted');

  StartTest('Break with many locals and globals');
  if RunTest_ManyLocalsBreak then Pass else Fail('Variables corrupted in break');

  StartTest('Continue with many locals');
  if RunTest_ManyLocalsContinue then Pass else Fail('Variables corrupted in continue');

  StartTest('Multiple exits with padding arrays');
  if RunTest_ManyLocalsMultipleExitsWrapper then Pass else Fail('Result values incorrect');

  StartTest('Deep nesting with locals at each level');
  if RunTest_DeepNestingManyLocals then Pass else Fail('Nested locals corrupted');

  StartTest('String manipulation in finally');
  if RunTest_StringsInFinally then Pass else Fail('Strings corrupted');

  StartTest('Global variable modification in finally');
  if RunTest_GlobalsInFinally then Pass else Fail('Globals corrupted');

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
