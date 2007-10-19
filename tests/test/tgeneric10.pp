{$mode objfpc}

uses
  ugeneric10;

type
  TMyIntList = specialize TList<integer>;

function CompareInt(const Item1, Item2: Integer): Integer;
begin
  Result := Item2 - Item1;
end;

var
  ilist : TMyIntList;
  someInt : integer;
begin
  someInt:=10;
  ilist := TMyIntList.Create;
  ilist.add(someInt);
  ilist.sort(ilist.TCompareFunc(@CompareInt));
  writeln('ok');
end.
