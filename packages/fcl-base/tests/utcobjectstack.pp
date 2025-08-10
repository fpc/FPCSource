unit utcObjectStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

Function TObjectStack_TestCreate : TTestString;
var
  S: TObjectStack;
begin
  Result:='';
  S := TObjectStack.Create;
  try
    AssertNotNull('Stack should be created', S);
    AssertEquals('Count should be 0 on creation', 0, S.Count);
  finally
    S.Free;
  end;
end;

Function TObjectStack_TestPush : TTestString;
var
  S: TObjectStack;
  O1, O2, Res: TObject;
begin
  Result:='';
  S := TObjectStack.Create;
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    Res := S.Push(O1);
    AssertSame('Push should return the pushed object', O1, Res);
    AssertEquals('Count should be 1', 1, S.Count);
    AssertSame('Peek should return the pushed object', O1, S.Peek);
    Res := S.Push(O2);
    AssertSame('Push should return the pushed object', O2, Res);
    AssertEquals('Count should be 2', 2, S.Count);
    AssertSame('Peek should return the last pushed object', O2, S.Peek);
  finally
    S.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TObjectStack_TestPop : TTestString;
var
  S: TObjectStack;
  O1, O2, Res: TObject;
begin
  Result:='';
  S := TObjectStack.Create;
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    S.Push(O1);
    S.Push(O2);
    Res := S.Pop;
    AssertSame('Pop should return the last pushed object (LIFO)', O2, Res);
    AssertEquals('Count should be 1', 1, S.Count);
    Res := S.Pop;
    AssertSame('Pop should return the first pushed object', O1, Res);
    AssertEquals('Count should be 0', 0, S.Count);
    Res := S.Pop;
    AssertEquals('Pop on an empty stack should return nil', nil, Res);
  finally
    S.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TObjectStack_TestPeek : TTestString;
var
  S: TObjectStack;
  O1, O2: TObject;
begin
  Result:='';
  S := TObjectStack.Create;
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    S.Push(O1);
    AssertSame('Peek should return the pushed object', O1, S.Peek);
    AssertEquals('Count should still be 1 after Peek', 1, S.Count);
    S.Push(O2);
    AssertSame('Peek should return the last pushed object', O2, S.Peek);
    AssertEquals('Count should still be 2 after Peek', 2, S.Count);
    S.Pop;
    AssertSame('Peek should return the remaining object', O1, S.Peek);
    S.Pop;
    AssertEquals('Peek on an empty stack should return nil', nil, S.Peek);
  finally
    S.Free;
    O1.Free;
    O2.Free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TObjectStackTests');
  AddTest('TestCreate', @TObjectStack_TestCreate, 'TObjectStackTests');
  AddTest('TestPush', @TObjectStack_TestPush, 'TObjectStackTests');
  AddTest('TestPop', @TObjectStack_TestPop, 'TObjectStackTests');
  AddTest('TestPeek', @TObjectStack_TestPeek, 'TObjectStackTests');
end;

end.
