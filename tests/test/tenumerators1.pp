program tenumerators1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}
{$apptype console}
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
begin
  // check TFPList enumerator
  List := TFPList.Create;
  List.Add(Pointer(1));
  List.Add(Pointer(2));
  List.Add(Pointer(3));

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
begin
  // check TList enumerator
  List := TList.Create;
  List.Add(Pointer(1));
  List.Add(Pointer(2));
  List.Add(Pointer(3));

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
  Collection.Add;
  Collection.Add;
  Collection.Add;

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
begin
  // check TStrings enumerator
  Strings := TStringList.Create;
  Strings.Add('1');
  Strings.Add('2');
  Strings.Add('3');

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
  TComponent.Create(Component);
  TComponent.Create(Component);
  TComponent.Create(Component);

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
begin
  // check TInterfaceList enumerator
  List := TInterfaceList.Create;
  Item := TInterfacedObject.Create;
  List.Add(Item);
  Item := TInterfacedObject.Create;
  List.Add(Item);
  Item := TInterfacedObject.Create;
  List.Add(Item);

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

