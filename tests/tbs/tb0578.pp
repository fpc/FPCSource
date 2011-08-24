{ %OPT=-Sew }
{ %CPU=x86_64 }
{ %NORUN }

{$asmmode intel}
begin
  asm
    mov rax,cr0
    mov rax,cr4
    mov rax,tr3
    mov rax,cr3
    mov rax,dr0
  end;
end.
