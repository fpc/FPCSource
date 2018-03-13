{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %FAIL }

program tasm25cs3;

{$asmmode att}

begin
  asm
    mov %cs:5(%rdi), %rax
  end;
end.
