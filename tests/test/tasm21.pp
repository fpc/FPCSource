{ %CPU=i8086 }

{$IFDEF FPC}
{$MODE TP}
{$ENDIF}
program tasm21;

function test1a: word; assembler;
asm
  mov ax, offset test1a;
end;

function test1b: word;
begin
  asm
    mov ax, offset test1b;
  end;
end;

procedure test2a; assembler;
asm
  mov ax, offset test2a;
end;

procedure test2b;
begin
  asm
    mov ax, offset test2b;
  end;
end;

begin
end.
