unit utcObjectQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

Function TObjectQueue_TestCreate : TTestString;
var
  Q: TObjectQueue;
begin
  Result:='';
  Q := TObjectQueue.Create;
  try
    AssertNotNull('Queue should be created', Q);
    AssertEquals('Count should be 0 on creation', 0, Q.Count);
  finally
    Q.Free;
  end;
end;

Function TObjectQueue_TestPush : TTestString;
var
  Q: TObjectQueue;
  O1, O2, Res: TObject;
begin
  Result:='';
  Q := TObjectQueue.Create;
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    Res := Q.Push(O1);
    AssertSame('Push should return the pushed object', O1, Res);
    AssertEquals('Count should be 1', 1, Q.Count);
    AssertSame('Peek should return the pushed object', O1, Q.Peek);
    Res := Q.Push(O2);
    AssertSame('Push should return the pushed object', O2, Res);
    AssertEquals('Count should be 2', 2, Q.Count);
    AssertSame('Peek should return the first pushed object', O1, Q.Peek);
  finally
    Q.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TObjectQueue_TestPop : TTestString;
var
  Q: TObjectQueue;
  O1, O2, Res: TObject;
begin
  Result:='';
  Q := TObjectQueue.Create;
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    Q.Push(O1);
    Q.Push(O2);
    Res := Q.Pop;
    AssertSame('Pop should return the first pushed object (FIFO)', O1, Res);
    AssertEquals('Count should be 1', 1, Q.Count);
    Res := Q.Pop;
    AssertSame('Pop should return the second pushed object', O2, Res);
    AssertEquals('Count should be 0', 0, Q.Count);
    Res := Q.Pop;
    AssertEquals('Pop on an empty queue should return nil', nil, Res);
  finally
    Q.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TObjectQueue_TestPeek : TTestString;
var
  Q: TObjectQueue;
  O1, O2: TObject;
begin
  Result:='';
  Q := TObjectQueue.Create;
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    Q.Push(O1);
    AssertSame('Peek should return the pushed object', O1, Q.Peek);
    AssertEquals('Count should still be 1 after Peek', 1, Q.Count);
    Q.Push(O2);
    AssertSame('Peek should return the first pushed object', O1, Q.Peek);
    AssertEquals('Count should still be 2 after Peek', 2, Q.Count);
    Q.Pop;
    AssertSame('Peek should return the remaining object', O2, Q.Peek);
    Q.Pop;
    AssertEquals('Peek on an empty queue should return nil', nil, Q.Peek);
  finally
    Q.Free;
    O1.Free;
    O2.Free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TObjectQueueTests');
  AddTest('TestCreate', @TObjectQueue_TestCreate, 'TObjectQueueTests');
  AddTest('TestPush', @TObjectQueue_TestPush, 'TObjectQueueTests');
  AddTest('TestPop', @TObjectQueue_TestPop, 'TObjectQueueTests');
  AddTest('TestPeek', @TObjectQueue_TestPeek, 'TObjectQueueTests');
end;

end.
