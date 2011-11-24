{ %cpu=i386 }
{ %fail }
{ %opt=-Sew }
{ %norun }
program testasm;

begin
  {$asmmode intel}
  asm
    mov eax,es:[0]
  end;
  {$asmmode att}
  asm
    mov %es:0,%eax
    mov %es:(0),%eax
  end;
end.
