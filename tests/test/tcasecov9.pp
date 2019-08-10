{ %fail }
{ %opt=-Sew }
{ %norun }

var
  l: 0..1;
begin
  l:=1;
  case l of
    0: write('a');
    1: writeln;
    else
      writeln('unreachable');
  end;
end.
