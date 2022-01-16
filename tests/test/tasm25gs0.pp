{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %NORUN }

program tasm25gs0;

{$asmmode intel}

begin
  asm
    mov rax, gs:[rdi+5]
  end;
end.
