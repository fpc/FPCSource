{ %CPU=i8086 }

{$IFDEF FPC}
{$MODE TP}
{$ELSE}
{$DEFINE FPC_MM_LARGE}
{$ENDIF FPC}

{$IFDEF FPC_MM_TINY}
  {$DEFINE NEAR_CODE}
  {$DEFINE NEAR_DATA}
{$ENDIF}
{$IFDEF FPC_MM_SMALL}
  {$DEFINE NEAR_CODE}
  {$DEFINE NEAR_DATA}
{$ENDIF}
{$IFDEF FPC_MM_MEDIUM}
  {$DEFINE FAR_CODE}
  {$DEFINE NEAR_DATA}
{$ENDIF}
{$IFDEF FPC_MM_COMPACT}
  {$DEFINE NEAR_CODE}
  {$DEFINE FAR_DATA}
{$ENDIF}
{$IFDEF FPC_MM_LARGE}
  {$DEFINE FAR_CODE}
  {$DEFINE FAR_DATA}
{$ENDIF}
{$IFDEF FPC_MM_HUGE}
  {$DEFINE FAR_CODE}
  {$DEFINE FAR_DATA}
{$ENDIF}

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
  res4: string;
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

function myfunc4: string; assembler;
asm
  mov actual_sp, sp
  cld
{$IFDEF FAR_DATA}
  les di, @Result
{$ELSE}
  mov di, @Result
  push ds
  pop es
{$ENDIF}
  mov al, 7  { string length }
  stosb
  mov al, 'T'
  stosb
  mov al, 'r'
  stosb
  mov al, 'a'
  stosb
  mov al, 'l'
  stosb
  mov al, 'a'
  stosb
  mov al, 'l'
  stosb
  mov al, 'a'
  stosb
end;

procedure myfunc5(s: string); assembler;
asm
  mov actual_sp, sp
end;

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
  res4 := myfunc4;
  if (res4 <> 'Tralala') and ((expect_sp - actual_sp) <> (2 + SizeOf(Pointer))) then
    Error;
  myfunc5('Test');
  if (expect_sp - actual_sp) <> (2 + SizeOf(Pointer)) then
    Error;
  Writeln('Ok!');
end.
