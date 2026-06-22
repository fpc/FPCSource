{ %fail }
{ %opt=-Sew }

var
  l : longint;
begin
  l:=random(10);
  halt(unaligned(l+l));
end.
