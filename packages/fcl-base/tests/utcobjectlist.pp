unit utcObjectList;

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

Function TObjectList_TestCreate : TTestString;
var
  L: TObjectList;
begin
  Result:='';
  L := TObjectList.Create;
  try
    AssertNotNull('List should be created', L);
    AssertEquals('Count should be 0 on creation', 0, L.Count);
    AssertTrue('OwnsObjects should be true by default', L.OwnsObjects);
  finally
    L.Free;
  end;
end;

Function TObjectList_TestAdd : TTestString;
var
  L: TObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add(O1);
    AssertEquals('Count should be 1 after adding one object', 1, L.Count);
    AssertSame('First item should be O1', O1, L.Items[0]);
    L.Add(O2);
    AssertEquals('Count should be 2 after adding a second object', 2, L.Count);
    AssertSame('Second item should be O2', O2, L.Items[1]);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TObjectList_TestExtract : TTestString;
var
  L: TObjectList;
  O1, O2, Extracted: TObject;
begin
  Result:='';
  L := TObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add(O1);
    L.Add(O2);
    Extracted := L.Extract(O1);
    AssertSame('Extracted object should be O1', O1, Extracted);
    AssertEquals('Count should be 1 after extracting an object', 1, L.Count);
    AssertSame('First item should now be O2', O2, L.Items[0]);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TObjectList_TestRemove : TTestString;
var
  L: TObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add(O1);
    L.Add(O2);
    L.Remove(O1);
    AssertEquals('Count should be 1 after removing an object', 1, L.Count);
    AssertSame('First item should now be O2', O2, L.Items[0]);
  finally
    L.Free;
    O2.Free;
  end;
end;

Function TObjectList_TestIndexOf : TTestString;
var
  L: TObjectList;
  O1, O2, O3: TObject;
begin
  Result:='';
  L := TObjectList.Create(False);
  O3 := TObject.Create;
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add(O1);
    L.Add(O2);
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

Function TObjectList_TestFindInstanceOf : TTestString;
var
  L: TObjectList;
  O1: TObject;
  C1: TMyObject;
begin
  Result:='';
  L := TObjectList.Create(False);
  try
    O1 := TObject.Create;
    C1 := TMyObject.Create;
    L.Add(O1);
    L.Add(C1);
    AssertEquals('Find TObject exact', 0, L.FindInstanceOf(TObject, True, 0));
    AssertEquals('Find TMyObject exact', 1, L.FindInstanceOf(TMyObject, True, 0));
    AssertEquals('Find TObject inexact', 0, L.FindInstanceOf(TObject, False, 0));
    AssertEquals('Find TMyObject inexact from start', 1, L.FindInstanceOf(TMyObject, False, 0));
  finally
    L.Free;
    O1.Free;
    C1.Free;
  end;
end;

Function TObjectList_TestInsert : TTestString;
var
  L: TObjectList;
  O1, O2, O3: TObject;
begin
  Result:='';
  L := TObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    O3 := TObject.Create;
    L.Add(O1);
    L.Add(O2);
    L.Insert(1, O3);
    AssertEquals('Count should be 3 after inserting an object', 3, L.Count);
    AssertSame('Item at index 1 should be O3', O3, L.Items[1]);
    AssertSame('Item at index 2 should be O2', O2, L.Items[2]);
  finally
    L.Free;
    O1.Free;
    O2.Free;
    O3.Free;
  end;
end;

Function TObjectList_TestFirstLast : TTestString;
var
  L: TObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add(O1);
    L.Add(O2);
    AssertSame('First object should be O1', O1, L.First);
    AssertSame('Last object should be O2', O2, L.Last);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TObjectList_TestOwnsObjects : TTestString;
var
  L: TObjectList;
  O1: TMyObject;
  Freed: Boolean;
begin
  Result:='';
  L := TObjectList.Create(True);
  Freed := False;
  O1 := TMyObject.Create;
  O1.IsFreed := @Freed;
  L.Add(O1);
  L.Free; // This should free O1 as well
  AssertTrue('Object should be freed when OwnsObjects is true and list is freed', Freed);
end;

procedure RegisterTests;
begin
  AddSuite('TObjectListTests');
  AddTest('TestCreate', @TObjectList_TestCreate, 'TObjectListTests');
  AddTest('TestAdd', @TObjectList_TestAdd, 'TObjectListTests');
  AddTest('TestExtract', @TObjectList_TestExtract, 'TObjectListTests');
  AddTest('TestRemove', @TObjectList_TestRemove, 'TObjectListTests');
  AddTest('TestIndexOf', @TObjectList_TestIndexOf, 'TObjectListTests');
  AddTest('TestFindInstanceOf', @TObjectList_TestFindInstanceOf, 'TObjectListTests');
  AddTest('TestInsert', @TObjectList_TestInsert, 'TObjectListTests');
  AddTest('TestFirstLast', @TObjectList_TestFirstLast, 'TObjectListTests');
  AddTest('TestOwnsObjects', @TObjectList_TestOwnsObjects, 'TObjectListTests');
end;

end.
