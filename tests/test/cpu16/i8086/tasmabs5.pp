{ %cpu=i8086 }

program tasmabs5;

{ NOT TP7 compatible (TP7 doesn't support absolute with an index) }

{$ASMMODE INTEL}
{$ASMCPU 80386}

var
  barr: array [8..12, -7..100] of byte;
  l: longint absolute barr[9,17];
  w: word absolute barr[10,53];
  b: byte absolute barr[11,62];
begin
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov l, 4
  end;
  if (barr[9,17+0] <> 4) or (barr[9,17+1] <> 0) or (barr[9,17+2] <> 0) or
     (barr[9,17+3] <> 0) or (barr[9,17+4] <> 255) or (barr[9,17-1] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov w, 2
  end;
  if (barr[10,53] <> 2) or (barr[10,54] <> 0) or (barr[10,55] <> 255) or
     (barr[10,52] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov b, 1
  end;
  if (barr[11,62] <> 1) or (barr[11,63] <> 255) or (barr[11,61] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  Writeln('Ok!');
end.
