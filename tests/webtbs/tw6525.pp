{ %cpu=i386,x86_64 }

{$goto on}

label l1;
var
  err : boolean;
begin
  asm
   jmp l1
  end;
  writeln('oops');
  err:=true;
l1:
  writeln('hello');
  if err then
    halt(1);
end.


