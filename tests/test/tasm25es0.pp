{ %CPU=x86_64 }
{ %NORUN }

program tasm25es0;

{$asmmode intel}

begin
  asm
    mov rax, es:[rdi+5]
  end;
end.
