{ %CPU=i386 }

function x : longint;saveregisters;
begin
  x:=34;
end;

var
  y : longint;
begin
  asm
    movl $15,%eax
  end;
  y:=x;
  Writeln(y);
  if y<>34 then
    halt(1);
end.
