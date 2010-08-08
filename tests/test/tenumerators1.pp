program tenumerators1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}
{$apptype console}
{$x-}
uses
  Classes;

{$ifndef fpc}
type
  PtrInt = integer;
{$endif}

{$ifdef fpc}
procedure CheckFPListEnumerator;
var
  Item: Pointer;
  List: TFPList;
  Enumerator: TFPListEnumerator;
  i: integer;
begin
  // check TFPList enumerator
  List := TFPList.Create;
  i:=List.Add(Pointer(1));
  i:=List.Add(Pointer(2));
  i:=List.Add(Pointer(3));

  Enumerator := List.GetEnumerator;
  while Enumerator.MoveNext do
  begin
    Item := Enumerator.Current;
    WriteLn(PtrInt(Item));
  end;
  Enumerator.Free;
  List.Free;
end;
{$endif}

procedure CheckListEnumerator;
var
  Item: Pointer;
  List: TList;
  Enumerator: TListEnumerator;
  i: integer;
begin
  // check TList enumerator
  List := TList.Create;
  i:=List.Add(Pointer(1));
  i:=List.Add(Pointer(2));
  i:=List.Add(Pointer(3));

  Enumerator := List.GetEnumerator;
  while Enumerator.MoveNext do
  begin
    Item := Enumerator.Current;
    WriteLn(PtrInt(Item));
  end;
  Enumerator.Free;
  List.Free;
end;

procedure CheckCollectionEnumerator;
var
  Item: TCollectionItem;
  Collection: TCollection;
  Enumerator: TCollectionEnumerator;
begin
  // check TCollection enumerator
  Collection := TCollection.Create(TCollectionItem);
  item:=Collection.Add;
  item:=Collection.Add;
  item:=Collection.Add;

  Enumerator := Collection.GetEnumerator;
  while Enumerator.MoveNext do
  begin
    Item := Enumerator.Current;
    WriteLn(PtrInt(Item));
  end;
  Enumerator.Free;
  Collection.Free;
end;

procedure CheckStringsEnumerator;
var
  Item: String;
  Strings: TStrings;
  Enumerator: TStringsEnumerator;
  i: integer;
begin
  // check TStrings enumerator
  Strings := TStringList.Create;
  i:=Strings.Add('1');
  i:=Strings.Add('2');
  i:=Strings.Add('3');

  Enumerator := Strings.GetEnumerator;
  while Enumerator.MoveNext do
  begin
    Item := Enumerator.Current;
    WriteLn(Item);
  end;
  Enumerator.Free;
  Strings.Free;
end;

procedure CheckComponentEnumerator;
var
  Item: TComponent;
  Component: TComponent;
  Enumerator: TComponentEnumerator;
begin
  // check TComponent enumerator
  Component := TComponent.Create(nil);
  item:=TComponent.Create(Component);
  item:=TComponent.Create(Component);
  item:=TComponent.Create(Component);

  Enumerator := Component.GetEnumerator;
  while Enumerator.MoveNext do
  begin
    Item := Enumerator.Current;
    WriteLn(PtrInt(Item));
  end;
  Enumerator.Free;
  Component.Free;
end;

procedure CheckInterfaceListEnumerator;
var
  Item: IUnknown;
  List: TInterfaceList;
  Enumerator: TInterfaceListEnumerator;
  i: integer;
begin
  // check TInterfaceList enumerator
  List := TInterfaceList.Create;
  Item := TInterfacedObject.Create;
  i:=List.Add(Item);
  Item := TInterfacedObject.Create;
  i:=List.Add(Item);
  Item := TInterfacedObject.Create;
  i:=List.Add(Item);

  Enumerator := List.GetEnumerator;
  while Enumerator.MoveNext do
  begin
    Item := Enumerator.Current;
    WriteLn(PtrInt(Item));
  end;
  Enumerator.Free;
  Item := nil;
  List.Free;
end;

begin
{$ifdef fpc}
  WriteLn('Testing FPC');
  CheckFPListEnumerator;
{$else}
  WriteLn('Testing Delphi');
{$endif}
  CheckListEnumerator;
  CheckCollectionEnumerator;
  CheckStringsEnumerator;
  CheckComponentEnumerator;
  CheckInterfaceListEnumerator;
end.

