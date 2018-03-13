{ %CPU=x86_64 }
{ %NORUN }

program tasm25es2;

{$asmmode att}

begin
  asm
    mov %es:5(%rdi), %rax
  end;
end.
