{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %FAIL }

program tasm25ss1;

{$asmmode intel}

begin
  asm
    mov rax, ss:[rdi+5]
  end;
end.
