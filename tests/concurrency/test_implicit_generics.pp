{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    Test suite for Section 2.1:
    - {$modeswitch implicitgenerics} in objfpc mode
    - Omitting 'specialize' for generic types and calls
    - Generic Type Inference for functions, methods, anonymous functions and function references
}
{$mode objfpc}{$H+}
{$modeswitch implicitgenerics}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

program test_implicit_generics;

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Classes,
  fgl,
  System.Threading;

generic function GenericId<T>(const AVal: T): T;
begin
  Result := AVal;
end;

type
  generic TBox<T> = class
  private
    FVal: T;
  public
    constructor Create(const AVal: T);
    property Val: T read FVal;
  end;

  TBoxFactory = class
  public
    generic class function Wrap<U>(const AVal: U): specialize TBox<U>;
  end;

  TIntList = specialize TFPGList<Integer>;
  { Testing omission of specialize in type declaration }
  TStringListGeneric = TFPGList<string>;

constructor TBox.Create(const AVal: T);
begin
  FVal := AVal;
end;

generic class function TBoxFactory.Wrap<U>(const AVal: U): specialize TBox<U>;
begin
  Result := specialize TBox<U>.Create(AVal);
end;

procedure AssertTrue(const ADesc: string; ACond: Boolean);
begin
  if not ACond then
  begin
    WriteLn('ASSERTION FAILED: ', ADesc);
    Halt(1);
  end;
  WriteLn('PASS: ', ADesc);
end;

procedure TestOmitSpecialize;
var
  s: string;
  i: Integer;
  box1: TBox<string>;
  box2: specialize TBox<Integer>;
  futStr: IFuture<string>;
  futInt: specialize IFuture<Integer>;
begin
  { Omit specialize on generic functions }
  s := GenericId<string>('hello_explicit');
  AssertTrue('GenericId<string>', s = 'hello_explicit');

  i := GenericId<Integer>(12345);
  AssertTrue('GenericId<Integer>', i = 12345);

  { Omit specialize on generic class method }
  box1 := TBoxFactory.Wrap<string>('boxed');
  AssertTrue('TBoxFactory.Wrap<string>', box1.Val = 'boxed');
  box1.Free;

  box2 := TBoxFactory.Wrap<Integer>(999);
  AssertTrue('TBoxFactory.Wrap<Integer>', box2.Val = 999);
  box2.Free;

  { Omit specialize on interface type }
  futStr := TTask.Future<string>(
    function: string
    begin
      Result := 'future_explicit';
    end
  );
  AssertTrue('futStr.Value explicit', futStr.Value = 'future_explicit');

  futInt := TTask.Future<Integer>(
    function: Integer
    begin
      Result := 777;
    end
  );
  AssertTrue('futInt.Value explicit', futInt.Value = 777);
end;

procedure TestGenericInferenceFunctionsAndMethods;
var
  s: string;
  i: Integer;
  box: TBox<string>;
begin
  { Inferred generic function call }
  s := GenericId('inferred_hello');
  AssertTrue('GenericId inferred string', s = 'inferred_hello');

  i := GenericId(54321);
  AssertTrue('GenericId inferred integer', i = 54321);

  { Inferred generic method call }
  box := TBoxFactory.Wrap('inferred_method');
  AssertTrue('TBoxFactory.Wrap inferred', box.Val = 'inferred_method');
  box.Free;
end;

procedure TestGenericInferenceFuturesAndTasks;
var
  futStr: IFuture<string>;
  futInt: IFuture<Integer>;
  contInt: IFuture<Integer>;
begin
  { TTask.Future with inferred return type from anonymous function }
  futStr := TTask.Future(
    function: string
    begin
      Result := 'inferred_task_result';
    end
  );
  AssertTrue('TTask.Future inferred string', futStr.Value = 'inferred_task_result');

  futInt := TTask.Future(
    function: Integer
    begin
      Result := 42;
    end
  );
  AssertTrue('TTask.Future inferred integer', futInt.Value = 42);

  { Continuation ThenBy with inferred type }
  contInt := futStr.ThenBy(
    function(const APrev: ITask): Integer
    begin
      Result := Length(IFuture<string>(APrev).Value);
    end
  );
  AssertTrue('futStr.ThenBy inferred integer', contInt.Value = Length('inferred_task_result'));
end;

begin
  WriteLn('=== Running Test Suite for Section 2.1: Implicit Generics & Type Inference ===');
  TestOmitSpecialize;
  TestGenericInferenceFunctionsAndMethods;
  TestGenericInferenceFuturesAndTasks;
  WriteLn('ALL TESTS PASSED SUCCESSFULLY.');
end.
