{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %FAIL }

program tasm25ds3;

{$asmmode att}

begin
  asm
    mov %ds:5(%rdi), %rax
  end;
end.
