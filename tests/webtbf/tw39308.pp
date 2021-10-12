{ %fail }
{ %opt=-Sew }
program project1;
var
  a: byte;
begin
  a:=1;
  if 1 > 2 then
    a := 0;         // <= no warning
  writeln(a);
end. 
