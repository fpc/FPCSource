{ %CPU=x86_64 }
{ %NORUN }

program tasm25ss0;

{$asmmode intel}

begin
  asm
    mov rax, ss:[rdi+5]
  end;
end.
