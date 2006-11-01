{$mode objfpc}

type
   generic TList<_T>=class(TObject)
     type
       PListItem = ^TListItem;
       TListItem = record
         data : _T;
         next : PListItem;
       end;
       TIterator = PListItem;
     var
       first : PListItem;

     function GetFirst : TIterator; inline;
     function GetNext(i : TIterator) : TIterator; inline;
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


function TList.GetFirst : TIterator; inline;
  begin
    result:=first;
  end;


function TList.GetNext(i : TIterator) : TIterator; inline;
  begin
    result:=i^.next;
  end;

type
  TMyIntList = specialize TList<integer>;
  TMyStringList = specialize TList<string>;
var
  ilist : TMyIntList;
  slist : TMyStringList;
  someInt : integer;
  iterator : TMyIntList.TIterator;
begin
  someInt:=10;
  ilist := TMyIntList.Create;
  ilist.Add(someInt);
  ilist.Add(someInt+1);
  iterator:=ilist.GetFirst;
  writeln(iterator^.data);
  if iterator^.data<>11 then
    halt(1);
  iterator:=ilist.GetNext(iterator);
  writeln(iterator^.data);
  if iterator^.data<>10 then
    halt(1);

  slist := TMyStringList.Create;
  slist.Add('Test1');
  slist.Add('Test2');
  writeln(slist.first^.data);
  if slist.first^.data<>'Test2' then
    halt(1);
  writeln('ok');
end.
