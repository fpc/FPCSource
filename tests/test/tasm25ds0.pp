{ %CPU=x86_64 }
{ %NORUN }

program tasm25ds0;

{$asmmode intel}

begin
  asm
    mov rax, ds:[rdi+5]
  end;
end.
