{ %fail }

{ This should not be allowed, it creates an infinite loop.
  The loop can be solved using word() typecasts. But it is
  to dangerous code to be allowed }
operator := (b:byte) l:longint;
begin
  if b=0 then
   l:=-1
  else
   l:=0;
end;

var
  l : longint;
  b : byte;
begin
  b:=0;
  l:=b;
  writeln(l);
end.
