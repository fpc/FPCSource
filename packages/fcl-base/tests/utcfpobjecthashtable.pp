unit utcFPObjectHashTable;

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

Function TFPObjectHashTable_TestCreate : TTestString;
var
  HT: TFPObjectHashTable;
begin
  Result:='';
  HT := TFPObjectHashTable.Create;
  try
    AssertNotNull('Hash table should be created', HT);
    AssertEquals('Count should be 0 on creation', 0, HT.Count);
    AssertTrue('IsEmpty should be true on creation', HT.IsEmpty);
    AssertTrue('OwnsObjects should be true by default', HT.OwnsObjects);
  finally
    HT.Free;
  end;
end;

Function TFPObjectHashTable_TestAdd : TTestString;
var
  HT: TFPObjectHashTable;
  O1, O2: TObject;
begin
  Result:='';
  HT := TFPObjectHashTable.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    HT.Add('Key1', O1);
    AssertEquals('Count should be 1 after adding one item', 1, HT.Count);
    AssertFalse('IsEmpty should be false after adding an item', HT.IsEmpty);
    AssertSame('Items property should return correct value', O1, HT.Items['Key1']);

    HT.Add('Key2', O2);
    AssertEquals('Count should be 2 after adding a second item', 2, HT.Count);
    AssertSame('Items property should return correct value for second item', O2, HT.Items['Key2']);
  finally
    HT.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPObjectHashTable_TestDelete : TTestString;
var
  HT: TFPObjectHashTable;
  O1, O2: TObject;
begin
  Result:='';
  HT := TFPObjectHashTable.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    HT.Add('Key1', O1);
    HT.Add('Key2', O2);
    HT.Delete('Key1');
    AssertEquals('Count should be 1 after deleting an item', 1, HT.Count);
    AssertNull('Accessing deleted key should return nil', HT.Items['Key1']);
    AssertSame('Other item should still exist', O2, HT.Items['Key2']);
  finally
    HT.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPObjectHashTable_TestClear : TTestString;
var
  HT: TFPObjectHashTable;
  O1, O2: TObject;
begin
  Result:='';
  HT := TFPObjectHashTable.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    HT.Add('Key1', O1);
    HT.Add('Key2', O2);
    HT.Clear;
    AssertEquals('Count should be 0 after clearing', 0, HT.Count);
    AssertTrue('IsEmpty should be true after clearing', HT.IsEmpty);
  finally
    HT.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPObjectHashTable_TestItemsProperty : TTestString;
var
  HT: TFPObjectHashTable;
  O1, O2: TObject;
begin
  Result:='';
  HT := TFPObjectHashTable.Create(False);
  try
    O1 := TObject.Create;
    O2 := TObject.Create;
    HT.Items['Key1'] := O1;
    AssertEquals('Count should be 1 after setting item', 1, HT.Count);
    AssertSame('Items property should return correct value', O1, HT.Items['Key1']);
    HT.Items['Key1'] := O2;
    AssertEquals('Count should still be 1 after updating item', 1, HT.Count);
    AssertSame('Items property should return updated value', O2, HT.Items['Key1']);
  finally
    HT.Free;
    O1.Free;
    O2.Free;
  end;
end;

Function TFPObjectHashTable_TestFind : TTestString;
var
  HT: TFPObjectHashTable;
  O1: TObject;
  Node: THTCustomNode;
begin
  Result:='';
  HT := TFPObjectHashTable.Create(False);
  try
    O1 := TObject.Create;
    HT.Add('Key1', O1);
    Node := HT.Find('Key1');
    AssertNotNull('Find should return a node for an existing key', Node);
    if Node <> nil then
    begin
      AssertEquals('Node should have the correct key', 'Key1', Node.Key);
      AssertSame('Node data should be correct', O1, THTObjectNode(Node).Data);
    end;

    Node := HT.Find('NonExistentKey');
    AssertNull('Find should return nil for a non-existent key', Node);
  finally
    HT.Free;
    O1.Free;
  end;
end;

Function TFPObjectHashTable_TestOwnsObjects : TTestString;
var
  HT: TFPObjectHashTable;
  O1: TMyObject;
  Freed: Boolean;
begin
  Result:='';
  HT := TFPObjectHashTable.Create(True);
  Freed := False;
  O1 := TMyObject.Create;
  O1.IsFreed := @Freed;
  HT.Add('Key1', O1);
  HT.Free; // This should free O1 as well
  AssertTrue('Object should be freed when OwnsObjects is true and hash table is freed', Freed);
end;

procedure RegisterTests;
begin
  AddSuite('TFPObjectHashTableTests');
  AddTest('TestCreate', @TFPObjectHashTable_TestCreate, 'TFPObjectHashTableTests');
  AddTest('TestAdd', @TFPObjectHashTable_TestAdd, 'TFPObjectHashTableTests');
  AddTest('TestDelete', @TFPObjectHashTable_TestDelete, 'TFPObjectHashTableTests');
  AddTest('TestClear', @TFPObjectHashTable_TestClear, 'TFPObjectHashTableTests');
  AddTest('TestItemsProperty', @TFPObjectHashTable_TestItemsProperty, 'TFPObjectHashTableTests');
  AddTest('TestFind', @TFPObjectHashTable_TestFind, 'TFPObjectHashTableTests');
  AddTest('TestOwnsObjects', @TFPObjectHashTable_TestOwnsObjects, 'TFPObjectHashTableTests');
end;

end.
