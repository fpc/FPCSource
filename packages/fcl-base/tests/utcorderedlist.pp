unit utcOrderedList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

type
  TConcreteOrderedList = class(TOrderedList)
  protected
    procedure PushItem(AItem: Pointer); override;
  end;

procedure TConcreteOrderedList.PushItem(AItem: Pointer);
begin
  List.Add(AItem);
end;

Function TOrderedList_TestCreate : TTestString;
var
  L: TConcreteOrderedList;
begin
  Result:='';
  L := TConcreteOrderedList.Create;
  try
    AssertNotNull('List should be created', L);
    AssertEquals('Count should be 0 on creation', 0, L.Count);
  finally
    L.Free;
  end;
end;

Function TOrderedList_TestCount : TTestString;
var
  L: TConcreteOrderedList;
  P: Pointer;
begin
  Result:='';
  L := TConcreteOrderedList.Create;
  try
    AssertEquals('Count should be 0 initially', 0, L.Count);
    P := Pointer(1);
    L.Push(P);
    AssertEquals('Count should be 1 after pushing one item', 1, L.Count);
    P := Pointer(2);
    L.Push(P);
    AssertEquals('Count should be 2 after pushing another item', 2, L.Count);
    L.Pop;
    AssertEquals('Count should be 1 after popping an item', 1, L.Count);
  finally
    L.Free;
  end;
end;

Function TOrderedList_TestAtLeast : TTestString;
var
  L: TConcreteOrderedList;
  P: Pointer;
begin
  Result:='';
  L := TConcreteOrderedList.Create;
  try
    AssertTrue('AtLeast(0) should be true for an empty list', L.AtLeast(0));
    AssertFalse('AtLeast(1) should be false for an empty list', L.AtLeast(1));
    P := Pointer(1);
    L.Push(P);
    AssertTrue('AtLeast(1) should be true for a list with one item', L.AtLeast(1));
  finally
    L.Free;
  end;
end;

Function TOrderedList_TestPush : TTestString;
var
  L: TConcreteOrderedList;
  P1, P2, Res: Pointer;
begin
  Result:='';
  L := TConcreteOrderedList.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    Res := L.Push(P1);
    AssertEquals('Push should return the pushed item', P1, Res);
    AssertEquals('Count should be 1', 1, L.Count);
    AssertEquals('Peek should return the pushed item', P1, L.Peek);
    Res := L.Push(P2);
    AssertEquals('Push should return the pushed item', P2, Res);
    AssertEquals('Count should be 2', 2, L.Count);
    AssertEquals('Peek should return the last pushed item', P2, L.Peek);
  finally
    L.Free;
  end;
end;

Function TOrderedList_TestPop : TTestString;
var
  L: TConcreteOrderedList;
  P1, P2, Res: Pointer;
begin
  Result:='';
  L := TConcreteOrderedList.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    L.Push(P1);
    L.Push(P2);
    Res := L.Pop;
    AssertEquals('Pop should return the last pushed item', P2, Res);
    AssertEquals('Count should be 1', 1, L.Count);
    Res := L.Pop;
    AssertEquals('Pop should return the first pushed item', P1, Res);
    AssertEquals('Count should be 0', 0, L.Count);
    Res := L.Pop;
    AssertEquals('Pop on an empty list should return nil', nil, Res);
  finally
    L.Free;
  end;
end;

Function TOrderedList_TestPeek : TTestString;
var
  L: TConcreteOrderedList;
  P1, P2: Pointer;
begin
  Result:='';
  L := TConcreteOrderedList.Create;
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    L.Push(P1);
    AssertEquals('Peek should return the pushed item', P1, L.Peek);
    AssertEquals('Count should still be 1 after Peek', 1, L.Count);
    L.Push(P2);
    AssertEquals('Peek should return the last pushed item', P2, L.Peek);
    AssertEquals('Count should still be 2 after Peek', 2, L.Count);
    L.Pop;
    AssertEquals('Peek should return the remaining item', P1, L.Peek);
    L.Pop;
    AssertEquals('Peek on an empty list should return nil', nil, L.Peek);
  finally
    L.Free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TOrderedListTests');
  AddTest('TestCreate', @TOrderedList_TestCreate, 'TOrderedListTests');
  AddTest('TestCount', @TOrderedList_TestCount, 'TOrderedListTests');
  AddTest('TestAtLeast', @TOrderedList_TestAtLeast, 'TOrderedListTests');
  AddTest('TestPush', @TOrderedList_TestPush, 'TOrderedListTests');
  AddTest('TestPop', @TOrderedList_TestPop, 'TOrderedListTests');
  AddTest('TestPeek', @TOrderedList_TestPeek, 'TOrderedListTests');
end;

end.
