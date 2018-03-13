{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %FAIL }

program tasm25es3;

{$asmmode att}

begin
  asm
    mov %es:5(%rdi), %rax
  end;
end.
