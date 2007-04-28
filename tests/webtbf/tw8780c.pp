{ %fail }
var c,c1,c2 : char;
begin
  c:=#1 or #2; // Error: Operation "or" not supported for types "Char" and "Char"
end.
