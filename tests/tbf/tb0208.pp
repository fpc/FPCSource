{ %fail }
{ %opt=-Sew -vw }

procedure test;
var
  b: byte;
begin
  case b of
    1: b:=1;
  end;
end;

begin
  test;
end.
