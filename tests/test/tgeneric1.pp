{$mode objfpc}

type
   TList=generic(_T) class(TObject)
     data : _T;
     procedure Add(item: _T);
   end;

procedure TList.Add(item: _T);
begin
  data:=item;
end;

type
  TMyIntList = specialize TList(integer);
  TMyStringList = specialize TList(string);

var
  ilist : TMyIntList;
  slist : TMyStringList;
  someInt : integer;
begin
  someInt:=10;
  ilist := TMyIntList.Create;
  ilist.Add(someInt);
  writeln(ilist.data);
  if ilist.data<>10 then
    halt(1);

  slist := TMyStringList.Create;
  slist.Add('Test');
  writeln(slist.data);
  if slist.data<>'Test' then
    halt(1);
end.
