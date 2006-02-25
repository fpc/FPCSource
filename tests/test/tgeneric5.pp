{$mode objfpc}

uses
  typinfo;

type
   generic TList<_T>=class(TObject)
     data : _T;
     procedure Add(item: _T);
   end;

var
  err : boolean;

procedure TList.Add(item: _T);
var
  i : integer;
  p : pointer;
begin
  i:=item;
  data := item;
  p:=typeinfo(_T);
  if p<>typeinfo(integer) then
    begin
      writeln('Typeinfo error');
      err:=true;
    end;
  if sizeof(item)<>4 then
    begin
      writeln('Sizeof error');
      err:=true;
    end;
end;

type
  TMyIntList = specialize TList<integer>;

var
  ilist : TMyIntList;
  someInt : integer;
begin
  someInt:=10;
  ilist := TMyIntList.Create;
  ilist.Add(someInt);
  writeln(ilist.data);
  if ilist.data<>10 then
    err:=true;
  if err then
    begin
      writeln('ERROR!');
      halt(1);
    end;
end.
