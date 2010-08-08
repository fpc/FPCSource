{ %opt=-gttt }
{$mode objfpc}


procedure get(out s: string);
begin
end;

procedure test;
var
  s: string[1];
  a,b: byte;
begin
  a:=1;
  b:=2;
  get(s);
  if (a<>1) or
     (b<>2) then
    halt(1);
end;

begin
  test;
end.
