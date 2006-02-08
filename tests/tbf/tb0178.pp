{ %fail }
{ %OPT=-Sew -veiw }

{$mode objfpc}

procedure t(out l: longint);
begin
  if (l > 0) then
    l := 1;
end;

var
  a: longint;
begin
  t(a);
  writeln(a);
end.
