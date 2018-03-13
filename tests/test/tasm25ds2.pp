{ %CPU=x86_64 }
{ %NORUN }

program tasm25ds2;

{$asmmode att}

begin
  asm
    mov %ds:5(%rdi), %rax
  end;
end.
