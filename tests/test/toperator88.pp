{ %NORUN }
program toperator88;

var
  c: TClass;
  b: Boolean;
begin
  c := Nil;
  b := c = Nil;
  b := c <> Nil;
  b := Nil = c;
  b := Nil <> c;
end.
