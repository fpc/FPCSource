{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %NORUN }

program tasm25fs0;

{$asmmode intel}

begin
  asm
    mov rax, fs:[rdi+5]
  end;
end.
