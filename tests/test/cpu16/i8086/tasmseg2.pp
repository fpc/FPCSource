{ %cpu=i8086 }

program tasmseg2;

{ i8086 test for the SEG inline assembler directive, when used in 32-bit
  instructions }

{$asmmode intel}
{$asmcpu 80386}

var
  err, err2, err3: Boolean;
begin
  err := False;
  asm
    xor eax, eax
    mov ebx, 0ffffffffh
    mov ax, seg @data
    mov ebx, seg @data
    cmp eax, ebx
    je @@ok
    mov err, 1
@@ok:
  end;
  if err then
    Writeln('32-bit and 16-bit seg @data don''t match!');
  err2 := False;
  asm
    xor eax, eax
    mov ebx, 0ffffffffh
    mov ax, seg @code
    mov ebx, seg @code
    cmp eax, ebx
    je @@ok2
    mov err2, 1
@@ok2:
  end;
  if err2 then
    Writeln('32-bit and 16-bit seg @code don''t match!');
  err3 := False;
  asm
    xor eax, eax
    mov ebx, 0ffffffffh
    mov ax, seg err3
    mov ebx, seg err3
    cmp eax, ebx
    je @@ok3
    mov err3, 1
@@ok3:
  end;
  if err3 then
    Writeln('32-bit and 16-bit seg err3 don''t match!');
  if err or err2 or err3 then
    Halt(1);
end.
