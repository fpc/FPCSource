{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %NORUN }

program tasm25gs1;

{$asmmode att}

begin
  asm
    mov %gs:5(%rdi), %rax
  end;
end.
