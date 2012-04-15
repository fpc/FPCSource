{ %cpu=i386 }
{ %norun }
program testasm;

begin
  {$asmmode intel}
  asm
    mov eax,gs:[0]
  end;
  {$asmmode att}
  asm
    mov %gs:0,%eax
    mov %gs:(0),%eax
  end;
end.
