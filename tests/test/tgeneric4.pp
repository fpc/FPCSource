uses ugeneric4;

procedure LocalFill;
begin
  globaldata:='Program';
end;

type
  TMyStringList = specialize TList<string>;

var
  slist : TMyStringList;
begin
  slist := TMyStringList.Create;
  slist.Fill;
  writeln(slist.data);
  if slist.data<>'Program' then
    halt(1);
end.
