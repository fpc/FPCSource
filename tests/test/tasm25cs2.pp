{ %CPU=x86_64 }
{ %NORUN }

program tasm25cs2;

{$asmmode att}

begin
  asm
    mov %cs:5(%rdi), %rax
  end;
end.
