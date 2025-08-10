unit utcfpobjectlist;

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

Function TFPObjectList_TestCreate : TTestString;
var
  L: TFPObjectList;
begin
  Result:='';
  L := TFPObjectList.Create;
  try
    AssertNotNull('List should be created', L);
    AssertEquals('Count should be 0 on creation', 0, L.Count);
    AssertTrue('OwnsObjects should be true by default', L.OwnsObjects);
  finally
    L.Free;
  end;
end;

Function TFPObjectList_TestAdd : TTestString;
var
  L: TFPObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPObjectList.Create(False);
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

Function TFPObjectList_TestDelete : TTestString;
var
  L: TFPObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add(O1);
    L.Add(O2);
    L.Delete(0);
    AssertEquals('Count should be 1 after deleting an object', 1, L.Count);
    AssertSame('First item should now be O2', O2, L.Items[0]);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPObjectList_TestClear : TTestString;
var
  L: TFPObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    L.Add(O1);
    L.Add(O2);
    L.Clear;
    AssertEquals('Count should be 0 after clearing the list', 0, L.Count);
  finally
    L.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPObjectList_TestIndexOf : TTestString;
var
  L: TFPObjectList;
  O1, O2, O3: TObject;
begin
  Result:='';
  L := TFPObjectList.Create(False);
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

Function TFPObjectList_TestRemove : TTestString;
var
  L: TFPObjectList;
  O1, O2: TObject;
begin
  Result:='';
  L := TFPObjectList.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    try
      L.Add(O1);
      L.Add(O2);
      L.Remove(O1);
      AssertEquals('Count should be 1 after removing an object', 1, L.Count);
      AssertSame('First item should now be O2', O2, L.Items[0]);
    finally
      O1.Free;
      O2.Free;
    end;
  finally
    L.Free;
  end;
end;

Function TFPObjectList_TestOwnsObjects : TTestString;
var
  L: TFPObjectList;
  O1: TMyObject;
  Freed: Boolean;
begin
  Result:='';
  L := TFPObjectList.Create(True);
  Freed := False;
  O1 := TMyObject.Create;
  O1.IsFreed := @Freed;
  L.Add(O1);
  L.Free; // This should free O1 as well
  AssertTrue('Object should be freed when OwnsObjects is true and list is freed', Freed);
end;

procedure RegisterTests;
begin
  AddSuite('TFPObjectListTests');
  AddTest('TestCreate', @TFPObjectList_TestCreate, 'TFPObjectListTests');
  AddTest('TestAdd', @TFPObjectList_TestAdd, 'TFPObjectListTests');
  AddTest('TestDelete', @TFPObjectList_TestDelete, 'TFPObjectListTests');
  AddTest('TestClear', @TFPObjectList_TestClear, 'TFPObjectListTests');
  AddTest('TestIndexOf', @TFPObjectList_TestIndexOf, 'TFPObjectListTests');
  AddTest('TestRemove', @TFPObjectList_TestRemove, 'TFPObjectListTests');
  AddTest('TestOwnsObjects', @TFPObjectList_TestOwnsObjects, 'TFPObjectListTests');
end;

end.
