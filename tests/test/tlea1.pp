{ %cpu=i386 }

program tlea1;

{$ASMMODE intel}

procedure Fail;
begin
  Writeln('Error!');
  Halt(1);
end;

procedure TestO32A32;
var
  res: DWord;
begin
  Writeln('Testing LEA with 32-bit operand size and 32-bit address size...');
  asm
    mov ecx, 0deadbeefh
    mov edi, 0abcdefedh
    mov ebx, 055667788h
    lea ebx, [ecx + edi * 8 + 12345678h]
    mov res, ebx
  end ['EBX', 'ECX', 'EDI'];
  if res <> $4F5194CF then
    Fail;
end;

procedure TestO16A32;
var
  res: DWord;
begin
  Writeln('Testing LEA with 16-bit operand size and 32-bit address size...');
  asm
    mov ecx, 0deadbeefh
    mov edi, 0abcdefedh
    mov ebx, 055667788h
    lea bx, [ecx + edi * 8 + 12345678h]
    mov res, ebx
  end ['EBX', 'ECX', 'EDI'];
  if res <> $556694CF then
    Fail;
end;

{$ifdef 16BITADDRSUPPORT}
procedure TestO32A16;
var
  res: DWord;
begin
  Writeln('Testing LEA with 32-bit operand size and 16-bit address size...');
  asm
    mov ebx, 0deadbeefh
    mov edi, 0abcdefedh
    mov ecx, 055667788h
    lea ecx, [bx + di + 1234h]
    mov res, ecx
  end ['EBX', 'ECX', 'EDI'];
  if res <> $C110 then
    Fail;
end;

procedure TestO16A16;
var
  res: DWord;
begin
  Writeln('Testing LEA with 16-bit operand size and 16-bit address size...');
  asm
    mov ebx, 0deadbeefh
    mov edi, 0abcdefedh
    mov ecx, 055667788h
    lea cx, [bx + di + 1234h]
    mov res, ecx
  end ['EBX', 'ECX', 'EDI'];
  if res <> $5566C110 then
    Fail;
end;
{$endif 16BITADDRSUPPORT}

begin
  TestO32A32;
  TestO16A32;
{$ifdef 16BITADDRSUPPORT}
  TestO32A16;
  TestO16A16;
{$endif 16BITADDRSUPPORT}
  Writeln('Success!');
end.
