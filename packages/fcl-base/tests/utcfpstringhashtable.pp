unit utcFPStringHashTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

Function TFPStringHashTable_TestCreate : TTestString;
var
  HT: TFPStringHashTable;
begin
  Result:='';
  HT := TFPStringHashTable.Create;
  try
    AssertNotNull('Hash table should be created', HT);
    AssertEquals('Count should be 0 on creation', 0, HT.Count);
    AssertTrue('IsEmpty should be true on creation', HT.IsEmpty);
  finally
    HT.Free;
  end;
end;

Function TFPStringHashTable_TestAdd : TTestString;
var
  HT: TFPStringHashTable;
begin
  Result:='';
  HT := TFPStringHashTable.Create;
  try
    HT.Add('Key1', 'Value1');
    AssertEquals('Count should be 1 after adding one item', 1, HT.Count);
    AssertFalse('IsEmpty should be false after adding an item', HT.IsEmpty);
    AssertEquals('Items property should return correct value', 'Value1', HT.Items['Key1']);

    HT.Add('Key2', 'Value2');
    AssertEquals('Count should be 2 after adding a second item', 2, HT.Count);
    AssertEquals('Items property should return correct value for second item', 'Value2', HT.Items['Key2']);
  finally
    HT.Free;
  end;
end;

Function TFPStringHashTable_TestDelete : TTestString;
var
  HT: TFPStringHashTable;
begin
  Result:='';
  HT := TFPStringHashTable.Create;
  try
    HT.Add('Key1', 'Value1');
    HT.Add('Key2', 'Value2');
    HT.Delete('Key1');
    AssertEquals('Count should be 1 after deleting an item', 1, HT.Count);
    AssertEquals('Accessing deleted key should return empty string', '', HT.Items['Key1']);
    AssertEquals('Other item should still exist', 'Value2', HT.Items['Key2']);
  finally
    HT.Free;
  end;
end;

Function TFPStringHashTable_TestClear : TTestString;
var
  HT: TFPStringHashTable;
begin
  Result:='';
  HT := TFPStringHashTable.Create;
  try
    HT.Add('Key1', 'Value1');
    HT.Add('Key2', 'Value2');
    HT.Clear;
    AssertEquals('Count should be 0 after clearing', 0, HT.Count);
    AssertTrue('IsEmpty should be true after clearing', HT.IsEmpty);
  finally
    HT.Free;
  end;
end;

Function TFPStringHashTable_TestItemsProperty : TTestString;
var
  HT: TFPStringHashTable;
begin
  Result:='';
  HT := TFPStringHashTable.Create;
  try
    HT.Items['Key1'] := 'Value1';
    AssertEquals('Count should be 1 after setting item', 1, HT.Count);
    AssertEquals('Items property should return correct value', 'Value1', HT.Items['Key1']);
    HT.Items['Key1'] := 'NewValue1';
    AssertEquals('Count should still be 1 after updating item', 1, HT.Count);
    AssertEquals('Items property should return updated value', 'NewValue1', HT.Items['Key1']);
  finally
    HT.Free;
  end;
end;

Function TFPStringHashTable_TestFind : TTestString;
var
  HT: TFPStringHashTable;
  Node: THTCustomNode;
begin
  Result:='';
  HT := TFPStringHashTable.Create;
  try
    HT.Add('Key1', 'Value1');
    Node := HT.Find('Key1');
    AssertNotNull('Find should return a node for an existing key', Node);
    if Node <> nil then
      AssertEquals('Node should have the correct key', 'Key1', Node.Key);

    Node := HT.Find('NonExistentKey');
    AssertNull('Find should return nil for a non-existent key', Node);
  finally
    HT.Free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TFPStringHashTableTests');
  AddTest('TestCreate', @TFPStringHashTable_TestCreate, 'TFPStringHashTableTests');
  AddTest('TestAdd', @TFPStringHashTable_TestAdd, 'TFPStringHashTableTests');
  AddTest('TestDelete', @TFPStringHashTable_TestDelete, 'TFPStringHashTableTests');
  AddTest('TestClear', @TFPStringHashTable_TestClear, 'TFPStringHashTableTests');
  AddTest('TestItemsProperty', @TFPStringHashTable_TestItemsProperty, 'TFPStringHashTableTests');
  AddTest('TestFind', @TFPStringHashTable_TestFind, 'TFPStringHashTableTests');
end;

end.
