{ %FAIL }

procedure p(a:array of char);
begin
end;

var
  s1 : ansistring;
  s2 : shortstring;
begin
  p(s1);
  p(s2);
end.
