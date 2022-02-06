program tfuncref25;

{$mode objfpc}
{$modeswitch functionreferences}

type
  TTestFuncRef = reference to function: LongInt;
  TTestFunc = function: LongInt;
  TTestMethod = function: LongInt of object;

type
  TTest = class
    f: LongInt;
    function Test: LongInt;
  end;

function TTest.Test: LongInt;
begin
  Result := f;
end;

function Test1: LongInt;
begin
  Result := 1;
end;

function Test2: LongInt;
begin
  Result := 2;
end;

function GetFunc: TTestFuncRef;
var
  func: TTestFunc;
begin
  func := @Test1;
  Result := func;
  func := @Test2;
end;

function GetMethod(t1, t2: TTest): TTestFuncRef;
var
  method: TTestMethod;
begin
  method := @t1.Test;
  Result := method;
  method := @t2.Test;
end;

var
  f: TTestFuncRef;
  t1, t2: TTest;
begin
  f := GetFunc;
  if f() <> 1 then
    Halt(1);
  t1 := TTest.Create;
  t1.f := 2;
  t2 := TTest.Create;
  t2.f := 3;
  f := GetMethod(t1, t2);
  if f() <> 2 then
    Halt(2);
  t1.Free;
  t2.Free;
end.
