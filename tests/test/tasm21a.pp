{ %CPU=i386 }

{$MODE TP}
{$ifdef FPC}
  {$PIC OFF}
{$endif FPC}

program tasm21a;

var
  test2a_ofs, test2b_ofs: longword;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

function test1a: longword; assembler;
asm
  mov eax, offset test1a;
end;

function test1b: longword;
begin
  asm
    mov eax, offset test1b;
    mov @Result, eax
  end;
end;

procedure test2a; assembler;
asm
  mov eax, offset test2a;
  mov test2a_ofs, eax
end;

procedure test2b;
begin
  asm
    mov eax, offset test2b;
    mov test2b_ofs, eax
  end;
end;

begin
  if test1a <> Ofs(test1a) then
    Error;
  if test1b <> Ofs(test1b) then
    Error;
  test2a;
  if test2a_ofs <> Ofs(test2a) then
    Error;
  test2b;
  if test2b_ofs <> Ofs(test2b) then
    Error;

  Writeln('Ok!');
end.
