unit utcStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

Function TStack_TestCreate : TTestString;
var
  S: TStack;
begin
  Result:='';
  S := TStack.Create;
  try
    AssertNotNull('Stack should be created', S);
    AssertEquals('Count should be 0 on creation', 0, S.Count);
  finally
    S.Free;
  end;
end;

Function TStack_TestPush : TTestString;
var
  S: TStack;
  P1, P2, Res: Pointer;
begin
  Result:='';
  S := TStack.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    Res := S.Push(P1);
    AssertEquals('Push should return the pushed item', P1, Res);
    AssertEquals('Count should be 1', 1, S.Count);
    AssertEquals('Peek should return the pushed item', P1, S.Peek);
    Res := S.Push(P2);
    AssertEquals('Push should return the pushed item', P2, Res);
    AssertEquals('Count should be 2', 2, S.Count);
    AssertEquals('Peek should return the last pushed item', P2, S.Peek);
  finally
    S.Free;
  end;
end;

Function TStack_TestPop : TTestString;
var
  S: TStack;
  P1, P2, Res: Pointer;
begin
  Result:='';
  S := TStack.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    S.Push(P1);
    S.Push(P2);
    Res := S.Pop;
    AssertEquals('Pop should return the last pushed item (LIFO)', P2, Res);
    AssertEquals('Count should be 1', 1, S.Count);
    Res := S.Pop;
    AssertEquals('Pop should return the first pushed item', P1, Res);
    AssertEquals('Count should be 0', 0, S.Count);
    Res := S.Pop;
    AssertEquals('Pop on an empty stack should return nil', nil, Res);
  finally
    S.Free;
  end;
end;

Function TStack_TestPeek : TTestString;
var
  S: TStack;
  P1, P2: Pointer;
begin
  Result:='';
  S := TStack.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    S.Push(P1);
    AssertEquals('Peek should return the pushed item', P1, S.Peek);
    AssertEquals('Count should still be 1 after Peek', 1, S.Count);
    S.Push(P2);
    AssertEquals('Peek should return the last pushed item', P2, S.Peek);
    AssertEquals('Count should still be 2 after Peek', 2, S.Count);
    S.Pop;
    AssertEquals('Peek should return the remaining item', P1, S.Peek);
    S.Pop;
    AssertEquals('Peek on an empty stack should return nil', nil, S.Peek);
  finally
    S.Free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TStackTests');
  AddTest('TestCreate', @TStack_TestCreate, 'TStackTests');
  AddTest('TestPush', @TStack_TestPush, 'TStackTests');
  AddTest('TestPop', @TStack_TestPop, 'TStackTests');
  AddTest('TestPeek', @TStack_TestPeek, 'TStackTests');
end;

end.
