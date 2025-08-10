unit utcFPHashObjectList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

type
  TMyObject = class(TObject)
    IsFreed: ^Boolean;
    destructor Destroy; override;
  end;

destructor TMyObject.Destroy;
begin
  if Assigned(IsFreed) then
    IsFreed^ := True;
  inherited Destroy;
end;

Function TFPHashObjectList_TestCreate : TTestString;
var
  L: TFPHashObjectList;
begin
  Result:='';
  L := TFPHashObjectList.Create;
  try
    AssertNotNull('List should be created', L);
    AssertEquals('Count should be 0 on creation', 0, L.Count);
    AssertTrue('OwnsObjects should be true by default', L.OwnsObjects);
  finally
    L.Free;
  end;
end;

Function TFPHashObjectList_TestAdd : TTestString;
var
  L: TFPHashObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPHashObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add('O1', O1);
    AssertEquals('Count should be 1 after adding one object', 1, L.Count);
    AssertSame('First item should be O1', O1, L.Items[0]);
    L.Add('O2', O2);
    AssertEquals('Count should be 2 after adding a second object', 2, L.Count);
    AssertSame('Second item should be O2', O2, L.Items[1]);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPHashObjectList_TestDelete : TTestString;
var
  L: TFPHashObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPHashObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add('O1', O1);
    L.Add('O2', O2);
    L.Delete(0);
    AssertEquals('Count should be 1 after deleting an object', 1, L.Count);
    AssertSame('First item should now be O2', O2, L.Items[0]);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPHashObjectList_TestClear : TTestString;
var
  L: TFPHashObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPHashObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add('O1', O1);
    L.Add('O2', O2);
    L.Clear;
    AssertEquals('Count should be 0 after clearing the list', 0, L.Count);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPHashObjectList_TestIndexOf : TTestString;
var
  L: TFPHashObjectList;
  O1, O2, O3: TObject;
begin
  Result:='';
  L := TFPHashObjectList.Create(False);
  O3 := TObject.Create;
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add('O1', O1);
    L.Add('O2', O2);
    AssertEquals('Index of O1 should be 0', 0, L.IndexOf(O1));
    AssertEquals('Index of O2 should be 1', 1, L.IndexOf(O2));
    AssertEquals('Index of a non-existent object should be -1', -1, L.IndexOf(O3));
  finally
    L.Free;
    O1.Free;
    O2.Free;
    O3.Free;
  end;
end;

Function TFPHashObjectList_TestRemove : TTestString;
var
  L: TFPHashObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPHashObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add('O1', O1);
    L.Add('O2', O2);
    L.Remove(O1);
    AssertEquals('Count should be 1 after removing an object', 1, L.Count);
    AssertSame('First item should now be O2', O2, L.Items[0]);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPHashObjectList_TestOwnsObjects : TTestString;
var
  L: TFPHashObjectList;
  O1: TMyObject;
  Freed: Boolean;
begin
  Result:='';
  L := TFPHashObjectList.Create(True);
  Freed := False;
  O1 := TMyObject.Create;
  O1.IsFreed := @Freed;
  L.Add('O1', O1);
  L.Free; // This should free O1 as well
  AssertTrue('Object should be freed when OwnsObjects is true and list is freed', Freed);
end;

Function TFPHashObjectList_TestFind : TTestString;
var
  L: TFPHashObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPHashObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add('O1', O1);
    L.Add('O2', O2);
    AssertSame('Find should return O1', O1, L.Find('O1'));
    AssertSame('Find should return O2', O2, L.Find('O2'));
    AssertEquals('Find for a non-existent object should return nil', nil, L.Find('O3'));
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPHashObjectList_TestFindLong : TTestString;
var
  L: TFPHashObjectList;
  O0, O1, O2: TObject;
  S : String;
begin
  Result:='';
  O0:=Nil;
  O1:=Nil;
  O2:=Nil;
  L := TFPHashObjectList.Create(False);
  try
    O0 := TObject.Create;
    O1 := TObject.Create;
    O2 := TObject.Create;
    S:=StringOfChar('A',333);
    L.Add('x', O0);
    L.Add(S, O1);
    L.Add(S+'2', O2);
    AssertSame('Find should return O1', O1, L.Find(S));
    AssertSame('Find should return O2', O2, L.Find(S+'2'));
    AssertEquals('Find for a non-existent object should return nil', nil, L.Find('O3'));
  finally
    L.Free;
    O0.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPHashObjectList_TestFindIndexOf : TTestString;
var
  L: TFPHashObjectList;
  O0, O1, O2: TObject;
begin
  Result:='';
  O0:=Nil;
  O1:=Nil;
  O2:=Nil;
  L := TFPHashObjectList.Create(False);
  try
    O0 := TObject.Create;
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add('O1', O1);
    L.Add('O2', O2);
    AssertEquals('FindIndexOf for O1 should be 0', 0, L.FindIndexOf('O1'));
    AssertEquals('FindIndexOf for O2 should be 1', 1, L.FindIndexOf('O2'));
    AssertEquals('FindIndexOf for a non-existent object should be -1', -1, L.FindIndexOf('O3'));
  finally
    L.Free;
    O0.Free;
    O1.Free;
    O2.Free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TFPHashObjectListTests');
  AddTest('TestCreate', @TFPHashObjectList_TestCreate, 'TFPHashObjectListTests');
  AddTest('TestAdd', @TFPHashObjectList_TestAdd, 'TFPHashObjectListTests');
  AddTest('TestDelete', @TFPHashObjectList_TestDelete, 'TFPHashObjectListTests');
  AddTest('TestClear', @TFPHashObjectList_TestClear, 'TFPHashObjectListTests');
  AddTest('TestIndexOf', @TFPHashObjectList_TestIndexOf, 'TFPHashObjectListTests');
  AddTest('TestRemove', @TFPHashObjectList_TestRemove, 'TFPHashObjectListTests');
  AddTest('TestOwnsObjects', @TFPHashObjectList_TestOwnsObjects, 'TFPHashObjectListTests');
  AddTest('TestFind', @TFPHashObjectList_TestFind, 'TFPHashObjectListTests');
  AddTest('TestFindLong', @TFPHashObjectList_TestFindLong, 'TFPHashObjectListTests');
  AddTest('TestFindIndexOf', @TFPHashObjectList_TestFindIndexOf, 'TFPHashObjectListTests');
end;

end.
