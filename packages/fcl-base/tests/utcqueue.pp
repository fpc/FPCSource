unit utcQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

Function TQueue_TestCreate : TTestString;
var
  Q: TQueue;
begin
  Result:='';
  Q := TQueue.Create;
  try
    AssertNotNull('Queue should be created', Q);
    AssertEquals('Count should be 0 on creation', 0, Q.Count);
  finally
    Q.Free;
  end;
end;

Function TQueue_TestPush : TTestString;
var
  Q: TQueue;
  P1, P2, Res: Pointer;
begin
  Result:='';
  Q := TQueue.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    Res := Q.Push(P1);
    AssertEquals('Push should return the pushed item', P1, Res);
    AssertEquals('Count should be 1', 1, Q.Count);
    AssertEquals('Peek should return the pushed item', P1, Q.Peek);
    Res := Q.Push(P2);
    AssertEquals('Push should return the pushed item', P2, Res);
    AssertEquals('Count should be 2', 2, Q.Count);
    AssertEquals('Peek should return the first pushed item', P1, Q.Peek);
  finally
    Q.Free;
  end;
end;

Function TQueue_TestPop : TTestString;
var
  Q: TQueue;
  P1, P2, Res: Pointer;
begin
  Result:='';
  Q := TQueue.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    Q.Push(P1);
    Q.Push(P2);
    Res := Q.Pop;
    AssertEquals('Pop should return the first pushed item (FIFO)', P1, Res);
    AssertEquals('Count should be 1', 1, Q.Count);
    Res := Q.Pop;
    AssertEquals('Pop should return the second pushed item', P2, Res);
    AssertEquals('Count should be 0', 0, Q.Count);
    Res := Q.Pop;
    AssertEquals('Pop on an empty queue should return nil', nil, Res);
  finally
    Q.Free;
  end;
end;

Function TQueue_TestPeek : TTestString;
var
  Q: TQueue;
  P1, P2: Pointer;
begin
  Result:='';
  Q := TQueue.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    Q.Push(P1);
    AssertEquals('Peek should return the pushed item', P1, Q.Peek);
    AssertEquals('Count should still be 1 after Peek', 1, Q.Count);
    Q.Push(P2);
    AssertEquals('Peek should return the first pushed item', P1, Q.Peek);
    AssertEquals('Count should still be 2 after Peek', 2, Q.Count);
    Q.Pop;
    AssertEquals('Peek should return the remaining item', P2, Q.Peek);
    Q.Pop;
    AssertEquals('Peek on an empty queue should return nil', nil, Q.Peek);
  finally
    Q.Free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TQueueTests');
  AddTest('TestCreate', @TQueue_TestCreate, 'TQueueTests');
  AddTest('TestPush', @TQueue_TestPush, 'TQueueTests');
  AddTest('TestPop', @TQueue_TestPop, 'TQueueTests');
  AddTest('TestPeek', @TQueue_TestPeek, 'TQueueTests');
end;

end.
