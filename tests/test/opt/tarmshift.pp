{ %norun }
{ %opt=-O2 }
var
  i : longint;

begin
  i:=1234;
  i:=i shl 23;
  i:=i shl 23;
  if i<>0 then
    halt(1);
end.
