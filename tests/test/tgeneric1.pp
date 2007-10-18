{$mode objfpc}

type
   generic TList<_T>=class(TObject)
     data : _T;
     procedure Add(item: _T);
   end;

procedure TList.Add(item: _T);
begin
  data:=item;
end;

type
  TMyIntList = specialize TList<integer>;
  TMyIntList2 = specialize TList<integer>;
  TMyStringList = specialize TList<string>;

var
  ilist : TMyIntList;
  ilist2 : TMyIntList2;
  slist : TMyStringList;
  someInt : integer;
begin
  someInt:=10;
  ilist := TMyIntList.Create;
  ilist.Add(someInt);
  writeln(ilist.data);
  if ilist.data<>10 then
    halt(1);

  someInt:=20;
  ilist2 := TMyIntList2.Create;
  ilist2.Add(someInt);
  writeln(ilist2.data);
  if ilist2.data<>20 then
    halt(1);

  slist := TMyStringList.Create;
  slist.Add('Test');
  writeln(slist.data);
  if slist.data<>'Test' then
    halt(1);
end.
