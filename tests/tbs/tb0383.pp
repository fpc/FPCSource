type
  enum1 = (one,two,three);
  enum2 = (een,twee,drie);

procedure p1(e:enum1);
begin
end;

var
  e2 : enum2;
begin
  e2:=een;
  p1(enum1(e2));
end.
