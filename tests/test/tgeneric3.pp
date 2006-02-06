uses ugeneric3;

type
  TMyStringList = specialize TList<string>;

var
  slist : TMyStringList;
begin
  slist := TMyStringList.Create;
  slist.Add('Test');
  writeln(slist.data);
end.
