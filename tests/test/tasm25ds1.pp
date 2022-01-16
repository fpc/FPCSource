{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %FAIL }

program tasm25ds1;

{$asmmode intel}

begin
  asm
    mov rax, ds:[rdi+5]
  end;
end.
