{ %fail }
{ %opt=-Sew }
{ %norun }

var
  l: 0..3;
begin
  l:=1;
  case l of
    2: writeln;
  end;
end.
