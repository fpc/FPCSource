{ %cpu=x86_64 }

program tlea2;

{$ASMMODE intel}

procedure Fail;
begin
  Writeln('Error!');
  Halt(1);
end;

procedure TestO64A64;
var
  res: QWord;
begin
  Writeln('Testing LEA with 64-bit operand size and 64-bit address size...');
  asm
    mov rcx, 0deadbeefdeadbeefh
    mov rdi, 0abcdefedcbabcdefh
    mov rbx, 05566778899aabbcch
    lea rbx, [rcx + rdi * 8 + 12345678h]
    mov res, rbx
  end ['RBX', 'RCX', 'RDI'];
  if res <> $3D1D3E5E4E4084DF then
    Fail;
end;

procedure TestO32A64;
var
  res: QWord;
begin
  Writeln('Testing LEA with 32-bit operand size and 64-bit address size...');
  asm
    mov rcx, 0deadbeefdeadbeefh
    mov rdi, 0abcdefedcbabcdefh
    mov rbx, 05566778899aabbcch
    lea ebx, [rcx + rdi * 8 + 12345678h]
    mov res, rbx
  end ['RBX', 'RCX', 'RDI'];
  if res <> $4E4084DF then
    Fail;
end;

procedure TestO16A64;
var
  res: QWord;
begin
  Writeln('Testing LEA with 16-bit operand size and 64-bit address size...');
  asm
    mov rcx, 0deadbeefdeadbeefh
    mov rdi, 0abcdefedcbabcdefh
    mov rbx, 05566778899aabbcch
    lea bx, [rcx + rdi * 8 + 12345678h]
    mov res, rbx
  end ['RBX', 'RCX', 'RDI'];
  if res <> $5566778899AA84DF then
    Fail;
end;

begin
  TestO64A64;
  TestO32A64;
  TestO16A64;
  Writeln('Success!');
end.
