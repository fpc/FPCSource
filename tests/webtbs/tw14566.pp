{ %cpu=x86_64 }
{ %norun }
procedure p(xmm0 : double);assembler;
asm
  movaps %xmm0,xmm0
end;

begin
end.
