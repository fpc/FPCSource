{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %NORUN }

program tasm25fs1;

{$asmmode att}

begin
  asm
    mov %fs:5(%rdi), %rax
  end;
end.
