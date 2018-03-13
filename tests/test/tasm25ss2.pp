{ %CPU=x86_64 }
{ %NORUN }

program tasm25ss2;

{$asmmode att}

begin
  asm
    mov %ss:5(%rdi), %rax
  end;
end.
