{$mode objfpc}

type
  TSList = class(TObject)
    procedure Test; virtual;
  end;

   generic TList<_T> = class(TSList)
     data : _T;
     procedure Add(item: _T);
     procedure Test; override;
   end;

  TListCompareFunc = function(Item1, Item2: Integer): Integer;

procedure TSList.Test;
begin
  writeln('should call TList!');
  halt(1);
end;

procedure TList.Add(item: _T);
begin
  data:=item;
end;

procedure TList.Test;
begin
  if data <> 10 then
    halt(1);
  writeln('ok');
end;

type
  TMyIntList = specialize TList<Integer>;

var
  ilist: TMyIntList;
  list: TSList;
begin
  ilist := TMyIntList.Create;
  list := ilist;
  ilist.add(10);
  list.test;
end.
