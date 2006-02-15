{$mode objfpc}

type
   generic PListItem<_T>=^specialize TListItem<_T>;
   generic TListItem<_T>=record
     data : _T;
     next : specialize PListItem<_T>;
   end;

   generic TList<_T>=class(TObject)
     first : specialize PListItem<_T>;
     procedure Add(item: _T);
   end;

procedure TList.Add(data: _T);
var
  newitem : specialize PListItem<_T>;
begin
  new(newitem);
  newitem.data:=data;
  newitem.next:=first;
end;

type
  TMyIntList = specialize TList<integer>;
  TMyStringList = specialize TList<string>;

var
  ilist : TMyIntList;
  slist : TMyStringList;
  someInt : integer;
begin
  someInt:=10;
  ilist := TMyIntList.Create;
  ilist.Add(someInt);
  ilist.Add(someInt+1);
  writeln(ilist.first^.data);
  if ilist.data<>10 then
    halt(1);
  writeln(ilist.first^.next^.data);
  if ilist.data<>11 then
    halt(1);

  slist := TMyStringList.Create;
  slist.Add('Test1');
  slist.Add('Test2');
  writeln(slist.data);
  if slist.data<>'Test1' then
    halt(1);
end.
