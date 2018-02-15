{ %CPU=i8086 }

{$IFDEF FPC}
{$MODE TP}
{$ENDIF FPC}

program tasm20;

{$S-}

{ This test checks that assembler functions that return a value in register(s)
  do not get allocated an extra $result variable on the stack }

var
  res: integer;
  res2: longint;
{$ifdef FPC}
  res3: int64;
{$endif FPC}
  expect_sp: word;
  actual_sp: word;

procedure myproc; assembler;
asm
  mov expect_sp, sp
end;

function myfunc: integer; assembler;
asm
  mov actual_sp, sp
  mov ax, $1234
end;

function myfunc2: longint; assembler;
asm
  mov actual_sp, sp
  mov ax, $5678
  mov dx, $1234
end;

{$ifdef FPC}
function myfunc3: int64; assembler;
asm
  mov actual_sp, sp
  mov ax, $1234
  mov bx, $5678
  mov cx, $9ABC
  mov dx, $DEF0
end;
{$endif FPC}

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

begin
  myproc;
  res := myfunc;
  if (res <> $1234) or (expect_sp <> actual_sp) then
    Error;
  res2 := myfunc2;
  if (res2 <> $12345678) or (expect_sp <> actual_sp) then
    Error;
{$ifdef FPC}
  res3 := myfunc3;
  if (res3 <> $123456789ABCDEF0) or (expect_sp <> actual_sp) then
    Error;
{$endif FPC}
  Writeln('Ok!');
end.
