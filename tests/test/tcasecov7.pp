{ %opt=-Sew }
{ %norun }

var
  s: shortstring;
begin
  s:='abc';
  case s[1] of
    'b': writeln;
  end;
end.
