{ %cpu=i386 }
{ %fail }
{ %opt=-vw -Sew }
{ %norun }
{ %skiptarget=darwin }
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
