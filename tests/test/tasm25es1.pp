{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %FAIL }

program tasm25es1;

{$asmmode intel}

begin
  asm
    mov rax, es:[rdi+5]
  end;
end.
