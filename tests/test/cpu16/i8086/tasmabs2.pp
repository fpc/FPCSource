{ %cpu=i8086 }

{ this test is Turbo Pascal 7 compatible }

program tasmabs2;

{$IFDEF FPC}
  {$ASMMODE INTEL}
  {$ASMCPU 80386}
{$ENDIF}

var
  l: longint absolute $B800:0;
  w: word absolute $B800:0;
  b: byte absolute $B800:0;
begin
{$IFDEF FPC}
  MemL[$B800:0] := MaxLongInt;
  MemL[$B800:4] := MaxLongInt;
  asm
    mov ax, 0b800h
    mov es, ax
    seges
    mov l, 4
  end;
  if (Mem[$B800:0] <> 4) or (Mem[$B800:1] <> 0) or (Mem[$B800:2] <> 0) or
     (Mem[$B800:3] <> 0) or (Mem[$B800:4] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
{$ENDIF}
  MemL[$B800:0] := MaxLongInt;
  MemL[$B800:4] := MaxLongInt;
  asm
    mov ax, 0b800h
    mov es, ax
    seges
    mov w, 2
  end;
  if (Mem[$B800:0] <> 2) or (Mem[$B800:1] <> 0) or (Mem[$B800:2] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  MemL[$B800:0] := MaxLongInt;
  MemL[$B800:4] := MaxLongInt;
  asm
    mov ax, 0b800h
    mov es, ax
    seges
    mov b, 1
  end;
  if (Mem[$B800:0] <> 1) or (Mem[$B800:1] <> 255) or (Mem[$B800:2] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  Writeln('Ok!');
end.
