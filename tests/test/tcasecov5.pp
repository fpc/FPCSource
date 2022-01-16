{ %opt=-Sew }
{ %norun }

{ should not print a warning }
var
  l: qword;
begin
  l:=1;
  case l of
    2: writeln;
  end;
end.
