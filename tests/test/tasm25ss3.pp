{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %FAIL }

program tasm25ss3;

{$asmmode att}

begin
  asm
    mov %ss:5(%rdi), %rax
  end;
end.
