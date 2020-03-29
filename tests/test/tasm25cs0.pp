{ %CPU=x86_64 }
{ %NORUN }

program tasm25cs0;

{$asmmode intel}

begin
  asm
    mov rax, cs:[rdi+5]
  end;
end.
