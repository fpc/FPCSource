{ %CPU=i8086 }

{$IFDEF FPC}
{$MODE TP}
{$ENDIF}
program tasm21;

var
  test2a_ofs, test2b_ofs: word;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

function test1a: word; assembler;
asm
  mov ax, offset test1a;
end;

function test1b: word;
begin
  asm
    mov ax, offset test1b;
    mov @Result, ax
  end;
end;

procedure test2a; assembler;
asm
  mov ax, offset test2a;
  mov test2a_ofs, ax
end;

procedure test2b;
begin
  asm
    mov ax, offset test2b;
    mov test2b_ofs, ax
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
