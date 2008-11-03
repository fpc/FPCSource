{ %fail }
{ %opt=-Sew -vw }

var
  a : array[0..32] of byte;

procedure p(const a:array of byte);
begin
end;

begin
  p(a);
end.
