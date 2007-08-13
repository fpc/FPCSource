{ %cpu=i386 }

function A: pointer; assembler; nostackframe;
asm
  pushl $A
  popl %eax
end;

begin
  if A <> pointer(@A) then
    halt(1);
end.

