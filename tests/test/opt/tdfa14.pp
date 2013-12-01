{ %OPT=-Oodfa -Sew -vw -S2 }
{ %fail }
{ %norun }
var
  j,
  i : longint;
begin
  j:=paramcount;
  if (j=1) or (i=0) then
    halt(1)
  else
    halt(0);
end.
