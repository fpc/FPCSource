{ %fail% }
{ %OPT=-O3 -Oodeadstore -Cr }
var
  i : longint;
  b : byte;

begin
  i:=1234;
  b:=i;
end.
