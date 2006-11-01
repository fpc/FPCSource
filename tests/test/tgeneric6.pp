{$mode objfpc}

type
   generic TList<_T>=class(TObject)
     type
       PListItem = ^TListItem;
       TListItem = record
         data : _T;
         next : PListItem;
       end;
     var
       first,last : PListItem;
     procedure Add(item: _T);
   end;

procedure TList.Add(item: _T);
var
  newitem : PListItem;
begin
  new(newitem);
  newitem^.data:=item;
  newitem^.next:=first;
  first:=newitem;
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
  if ilist.first^.data<>11 then
    halt(1);
  writeln(ilist.first^.next^.data);
  if ilist.first^.next^.data<>10 then
    halt(1);

  slist := TMyStringList.Create;
  slist.Add('Test1');
  slist.Add('Test2');
  writeln(slist.first^.data);
  if slist.first^.data<>'Test2' then
    halt(1);
  writeln('ok');
end.
