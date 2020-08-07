{ %OPT=-O3 }
var a, b : integer;
begin
  b := 0;
  b := b - a - a { `b := b - a` won't trigger the error }
end.
