unit utcComponentList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, punit;

procedure RegisterTests;

implementation

type
  TMyComponent = class(TComponent)
  public
    IsFreed: ^Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TMyComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsFreed := nil;
end;

destructor TMyComponent.Destroy;
begin
  if Assigned(IsFreed) then
    IsFreed^ := True;
  inherited Destroy;
end;

Function TComponentList_TestCreate : TTestString;
var
  L: TComponentList;
begin
  Result:='';
  L := TComponentList.Create;
  try
    AssertNotNull('List should be created', L);
    AssertEquals('Count should be 0 on creation', 0, L.Count);
    AssertTrue('OwnsObjects should be true by default', L.OwnsObjects);
  finally
    L.Free;
  end;
end;

Function TComponentList_TestAdd : TTestString;
var
  L: TComponentList;
  C1, C2: TComponent;
begin
  Result:='';
  L := TComponentList.Create(False);
  try
    C1 := TComponent.Create(nil);
    C2 := TComponent.Create(nil);
    L.Add(C1);
    AssertEquals('Count should be 1 after adding one component', 1, L.Count);
    AssertSame('First item should be C1', C1, L.Items[0]);
    L.Add(C2);
    AssertEquals('Count should be 2 after adding a second component', 2, L.Count);
    AssertSame('Second item should be C2', C2, L.Items[1]);
  finally
    L.Free;
    C1.Free;
    C2.Free;
  end;
end;

Function TComponentList_TestExtract : TTestString;
var
  L: TComponentList;
  C1, C2, Extracted: TComponent;
begin
  Result:='';
  L := TComponentList.Create(False);
  try
    C1 := TComponent.Create(nil);
    C2 := TComponent.Create(nil);
    L.Add(C1);
    L.Add(C2);
    Extracted := L.Extract(C1);
    AssertSame('Extracted component should be C1', C1, Extracted);
    AssertEquals('Count should be 1 after extracting a component', 1, L.Count);
    AssertSame('First item should now be C2', C2, L.Items[0]);
  finally
    L.Free;
    C1.Free;
    C2.Free;
  end;
end;

Function TComponentList_TestRemove : TTestString;
var
  L: TComponentList;
  C1, C2: TComponent;
begin
  Result:='';
  L := TComponentList.Create(False);
  try
    C1 := TComponent.Create(nil);
    C2 := TComponent.Create(nil);
    L.Add(C1);
    L.Add(C2);
    L.Remove(C1);
    AssertEquals('Count should be 1 after removing a component', 1, L.Count);
    AssertSame('First item should now be C2', C2, L.Items[0]);
  finally
    L.Free;
    C2.Free;
  end;
end;

Function TComponentList_TestIndexOf : TTestString;
var
  L: TComponentList;
  C1, C2, C3: TComponent;
begin
  Result:='';
  L := TComponentList.Create(False);
  C3 := TComponent.Create(nil);
  try
    C1 := TComponent.Create(nil);
    C2 := TComponent.Create(nil);
    L.Add(C1);
    L.Add(C2);
    AssertEquals('Index of C1 should be 0', 0, L.IndexOf(C1));
    AssertEquals('Index of C2 should be 1', 1, L.IndexOf(C2));
    AssertEquals('Index of a non-existent component should be -1', -1, L.IndexOf(C3));
  finally
    L.Free;
    C1.Free;
    C2.Free;
    C3.Free;
  end;
end;

Function TComponentList_TestInsert : TTestString;
var
  L: TComponentList;
  C1, C2, C3: TComponent;
begin
  Result:='';
  L := TComponentList.Create(False);
  try
    C1 := TComponent.Create(nil);
    C2 := TComponent.Create(nil);
    C3 := TComponent.Create(nil);
    L.Add(C1);
    L.Add(C2);
    L.Insert(1, C3);
    AssertEquals('Count should be 3 after inserting a component', 3, L.Count);
    AssertSame('Item at index 1 should be C3', C3, L.Items[1]);
    AssertSame('Item at index 2 should be C2', C2, L.Items[2]);
  finally
    L.Free;
    C1.Free;
    C2.Free;
    C3.Free;
  end;
end;

Function TComponentList_TestFirstLast : TTestString;
var
  L: TComponentList;
  C1, C2: TComponent;
begin
  Result:='';
  L := TComponentList.Create(False);
  try
    C1 := TComponent.Create(nil);
    C2 := TComponent.Create(nil);
    L.Add(C1);
    L.Add(C2);
    AssertSame('First component should be C1', C1, L.First);
    AssertSame('Last component should be C2', C2, L.Last);
  finally
    L.Free;
    C1.Free;
    C2.Free;
  end;
end;

Function TComponentList_TestOwnsObjects : TTestString;
var
  L: TComponentList;
  C1: TMyComponent;
  Freed: Boolean;
begin
  Result:='';
  L := TComponentList.Create(True);
  Freed := False;
  C1 := TMyComponent.Create(nil);
  C1.IsFreed := @Freed;
  L.Add(C1);
  L.Free; // This should free C1 as well
  AssertTrue('Component should be freed when OwnsObjects is true and list is freed', Freed);
end;

procedure RegisterTests;
begin
  AddSuite('TComponentListTests');
  AddTest('TestCreate', @TComponentList_TestCreate, 'TComponentListTests');
  AddTest('TestAdd', @TComponentList_TestAdd, 'TComponentListTests');
  AddTest('TestExtract', @TComponentList_TestExtract, 'TComponentListTests');
  AddTest('TestRemove', @TComponentList_TestRemove, 'TComponentListTests');
  AddTest('TestIndexOf', @TComponentList_TestIndexOf, 'TComponentListTests');
  AddTest('TestInsert', @TComponentList_TestInsert, 'TComponentListTests');
  AddTest('TestFirstLast', @TComponentList_TestFirstLast, 'TComponentListTests');
  AddTest('TestOwnsObjects', @TComponentList_TestOwnsObjects, 'TComponentListTests');
end;

end.
