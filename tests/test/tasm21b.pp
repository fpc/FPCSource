{ %CPU=x86_64 }

{$MODE TP}
{$ASMMODE INTEL}
{$ifdef FPC}
  {$PIC OFF}
{$endif FPC}

program tasm21b;

var
  test2a_ofs, test2b_ofs: qword;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

function test1a: qword; assembler;
asm
  mov rax, offset test1a;
end;

function test1b: qword;
begin
  asm
    mov rax, offset test1b;
    mov @Result, rax
  end;
end;

procedure test2a; assembler;
asm
  mov rax, offset test2a;
  mov test2a_ofs, rax
end;

procedure test2b;
begin
  asm
    mov rax, offset test2b;
    mov test2b_ofs, rax
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
