unit utcClassList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

Function TClassList_TestCreate : TTestString;
var
  L: TClassList;
begin
  Result:='';
  L := TClassList.Create;
  try
    AssertNotNull('List should be created', L);
    AssertEquals('Count should be 0 on creation', 0, L.Count);
  finally
    L.Free;
  end;
end;

Function TClassList_TestAdd : TTestString;
var
  L: TClassList;
begin
  Result:='';
  L := TClassList.Create;
  try
    L.Add(TObject);
    AssertEquals('Count should be 1 after adding one class', 1, L.Count);
    AssertEquals('First item should be TObject', TObject, L.Items[0]);
    L.Add(TList);
    AssertEquals('Count should be 2 after adding a second class', 2, L.Count);
    AssertEquals('Second item should be TList', TList, L.Items[1]);
  finally
    L.Free;
  end;
end;

Function TClassList_TestExtract : TTestString;
var
  L: TClassList;
  Extracted: TClass;
begin
  Result:='';
  L := TClassList.Create;
  try
    L.Add(TObject);
    L.Add(TList);
    Extracted := L.Extract(TObject);
    AssertEquals('Extracted class should be TObject', TObject, Extracted);
    AssertEquals('Count should be 1 after extracting a class', 1, L.Count);
    AssertEquals('First item should now be TList', TList, L.Items[0]);
  finally
    L.Free;
  end;
end;

Function TClassList_TestRemove : TTestString;
var
  L: TClassList;
begin
  Result:='';
  L := TClassList.Create;
  try
    L.Add(TObject);
    L.Add(TList);
    L.Remove(TObject);
    AssertEquals('Count should be 1 after removing a class', 1, L.Count);
    AssertEquals('First item should now be TList', TList, L.Items[0]);
  finally
    L.Free;
  end;
end;

Function TClassList_TestIndexOf : TTestString;
var
  L: TClassList;
begin
  Result:='';
  L := TClassList.Create;
  try
    L.Add(TObject);
    L.Add(TList);
    AssertEquals('Index of TObject should be 0', 0, L.IndexOf(TObject));
    AssertEquals('Index of TList should be 1', 1, L.IndexOf(TList));
    AssertEquals('Index of a non-existent class should be -1', -1, L.IndexOf(TComponent));
  finally
    L.Free;
  end;
end;

Function TClassList_TestInsert : TTestString;
var
  L: TClassList;
begin
  Result:='';
  L := TClassList.Create;
  try
    L.Add(TObject);
    L.Add(TList);
    L.Insert(1, TComponent);
    AssertEquals('Count should be 3 after inserting a class', 3, L.Count);
    AssertEquals('Item at index 1 should be TComponent', TComponent, L.Items[1]);
    AssertEquals('Item at index 2 should be TList', TList, L.Items[2]);
  finally
    L.Free;
  end;
end;

Function TClassList_TestFirstLast : TTestString;
var
  L: TClassList;
begin
  Result:='';
  L := TClassList.Create;
  try
    L.Add(TObject);
    L.Add(TList);
    AssertEquals('First class should be TObject', TObject, L.First);
    AssertEquals('Last class should be TList', TList, L.Last);
  finally
    L.Free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TClassListTests');
  AddTest('TestCreate', @TClassList_TestCreate, 'TClassListTests');
  AddTest('TestAdd', @TClassList_TestAdd, 'TClassListTests');
  AddTest('TestExtract', @TClassList_TestExtract, 'TClassListTests');
  AddTest('TestRemove', @TClassList_TestRemove, 'TClassListTests');
  AddTest('TestIndexOf', @TClassList_TestIndexOf, 'TClassListTests');
  AddTest('TestInsert', @TClassList_TestInsert, 'TClassListTests');
  AddTest('TestFirstLast', @TClassList_TestFirstLast, 'TClassListTests');
end;

end.
