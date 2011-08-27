{ %cpu=x86_64 }
{ %norun }
program testasm;

begin
  {$asmmode intel}
  asm
    mov rax,gs:[0]
  end;
  {$asmmode att}
  asm
    mov %gs:0,%rax
    mov %gs:(0),%rax
  end;
end.
