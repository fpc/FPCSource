{ %CPU=x86_64 }
{ %OPT=-Sew }
{ %FAIL }

program tasm25cs1;

{$asmmode intel}

begin
  asm
    mov rax, cs:[rdi+5]
  end;
end.
