{ %cpu=i8086 }

program tasmabs4;

{ NOT TP7 compatible (TP7 doesn't support absolute with an index) }

{$ASMMODE INTEL}
{$ASMCPU 80386}

var
  barr: array [8..12, -7..100] of byte;
  l: longint absolute barr[9];
  w: word absolute barr[10];
  b: byte absolute barr[11];
begin
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov l, 4
  end;
  if (barr[9,-7] <> 4) or (barr[9,-7+1] <> 0) or (barr[9,-7+2] <> 0) or
     (barr[9,-7+3] <> 0) or (barr[9,-7+4] <> 255) or (barr[8,100] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov w, 2
  end;
  if (barr[10,-7] <> 2) or (barr[10,-7+1] <> 0) or (barr[10,-7+2] <> 255) or
     (barr[9,100] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov b, 1
  end;
  if (barr[11,-7] <> 1) or (barr[11,-7+1] <> 255) or (barr[10,100] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  Writeln('Ok!');
end.
