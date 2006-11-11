{$mode objfpc}

type
   generic TList<_T>=class(TObject)
   type public
     TCompareFunc = function(const Item1, Item2: _T): Integer;
   var public 
     data : _T;
     compare : TCompareFunc;
     procedure Add(item: _T);
   end;

procedure TList.Add(item: _T);
begin
  data:=item;
  if compare(data, 20) <= 0 then
    halt(1);
end;

function CompareInt(Item1, Item2: Integer): Integer;
begin
  Result := Item2 - Item1;
end;

type
  TMyIntList = specialize TList<integer>;

var
  ilist : TMyIntList;
  someInt : integer;
begin
  someInt:=10;
  ilist := TMyIntList.Create;
  ilist.compare := ilist.TCompareFunc(@CompareInt);
  ilist.add(someInt);
end.
