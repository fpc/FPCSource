{ %norun }
{ %opt=-Sew }

procedure t;
var
  f:array[1..10] of text;
begin
  assign(f[1],paramstr(0));
end;

begin
  t;
end.
