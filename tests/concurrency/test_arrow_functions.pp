{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    Test suite for Section 2.2:
    - {$modeswitch arrowfunctions}
    - Short lambda syntax (=>)
    - TParallel.&For and TTask.Future integration
}
{$mode objfpc}{$H+}
{$modeswitch arrowfunctions}
{$modeswitch implicitgenerics}

program test_arrow_functions;

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Classes,
  System.Threading;

procedure AssertTrue(const ADesc: string; ACond: Boolean);
begin
  if not ACond then
  begin
    WriteLn('FAIL: ', ADesc);
    Halt(1);
  end
  else
    WriteLn('PASS: ', ADesc);
end;

procedure AssertEquals(const ADesc: string; AExpected, AActual: Integer);
begin
  if AExpected <> AActual then
  begin
    WriteLn('FAIL: ', ADesc, ' - Expected ', AExpected, ', got ', AActual);
    Halt(1);
  end
  else
    WriteLn('PASS: ', ADesc, ' = ', AActual);
end;

procedure AssertEqualsStr(const ADesc: string; const AExpected, AActual: string);
begin
  if AExpected <> AActual then
  begin
    WriteLn('FAIL: ', ADesc, ' - Expected "', AExpected, '", got "', AActual, '"');
    Halt(1);
  end
  else
    WriteLn('PASS: ', ADesc, ' = "', AActual, '"');
end;

function Work(const X: Integer): Integer;
begin
  Result := X * 10;
end;

function ComputeResult(const X: Integer): Integer;
begin
  Result := X * 2;
end;

procedure TestParallelFor;
var
  Results: array[0..9] of Integer;
  I: Integer;
begin
  for I := 0 to 9 do
    Results[I] := 0;

  { Target syntax 1a: TParallel.&For with arrow function with parens (I) => ... }
  TParallel.&For(0, 9, (I) => Results[I] := Work(I));

  for I := 0 to 9 do
    AssertEquals('Parallel.For (I) index ' + IntToStr(I), I * 10, Results[I]);

  for I := 0 to 9 do
    Results[I] := 0;

  { Target syntax 1b: TParallel.&For with arrow function without parens I => ... }
  TParallel.&For(0, 9, I => Results[I] := Work(I) + 1);

  for I := 0 to 9 do
    AssertEquals('Parallel.For I index ' + IntToStr(I), I * 10 + 1, Results[I]);
end;

procedure TestTaskFuture;
var
  R1: IFuture<Integer>;
  R2: IFuture<Integer>;
begin
  { Target syntax 2a: TTask.Future with explicit generic type argument }
  R1 := TTask.Future<Integer>(() => ComputeResult(42));
  AssertEquals('TTask.Future<Integer> result', 84, R1.Value);

  { Target syntax 2b: TTask.Future with inferred generic type argument }
  R2 := TTask.Future(() => ComputeResult(50));
  AssertEquals('TTask.Future inferred result', 100, R2.Value);
end;

type
  generic TFunc1<T, R> = reference to function(Arg: T): R;
  generic TFunc2<T1, T2, R> = reference to function(Arg1: T1; Arg2: T2): R;

procedure TestDirectDelegates;
var
  Proc1: TProc<Integer>;
  Func0: TFunc<Integer>;
  Func1: TFunc1<Integer, Integer>;
  FuncNoParen: TFunc1<Integer, Integer>;
  Func2Typed: TFunc2<Integer, Integer, Integer>;
  Func2Untyped: TFunc2<Integer, Integer, Integer>;
  FuncDiffTypes: TFunc2<Integer, string, string>;
  CapturedSum: Integer;
begin
  CapturedSum := 0;
  Proc1 := (X: Integer) => Inc(CapturedSum, X);
  Proc1(15);
  AssertEquals('Proc1 lambda execution', 15, CapturedSum);

  Func0 := () => 100;
  AssertEquals('Func0 parameterless lambda execution', 100, Func0());

  Func1 := (X: Integer) => X * 3;
  AssertEquals('Func1 lambda execution', 21, Func1(7));

  FuncNoParen := X => X * 4;
  AssertEquals('FuncNoParen lambda execution', 28, FuncNoParen(7));

  Func2Typed := (A: Integer, B: Integer) => A + B;
  AssertEquals('Func2Typed lambda execution', 30, Func2Typed(12, 18));

  Func2Untyped := (A, B) => A * B;
  AssertEquals('Func2Untyped lambda execution', 56, Func2Untyped(7, 8));

  FuncDiffTypes := (Count, Msg) => Msg + ': ' + IntToStr(Count);
  AssertEqualsStr('FuncDiffTypes lambda execution', 'Items: 42', FuncDiffTypes(42, 'Items'));
end;

procedure TestBlockBody;
var
  ProcBlock: TProc<Integer>;
  Total: Integer;
begin
  Total := 0;
  ProcBlock := (V) =>
    begin
      Inc(Total, V);
      Inc(Total, 1);
    end;
  ProcBlock(10);
  AssertEquals('ProcBlock execution', 11, Total);
end;

begin
  WriteLn('=== Running Arrow Functions Test Suite ===');
  TestParallelFor;
  TestTaskFuture;
  TestDirectDelegates;
  TestBlockBody;
  WriteLn('=== All Arrow Function Tests Passed Successfully! ===');
end.
