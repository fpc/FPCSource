{ %cpu=i8086 }

{ this test is Turbo Pascal 7 compatible }

program tasmabs1;

{$IFDEF FPC}
  {$ASMMODE INTEL}
  {$ASMCPU 80386}
{$ENDIF}

var
  barr: array [0..100] of byte;
  l: longint absolute barr;
  w: word absolute barr;
  b: byte absolute barr;
begin
{$IFDEF FPC}
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov l, 4
  end;
  if (barr[0] <> 4) or (barr[1] <> 0) or (barr[2] <> 0) or
     (barr[3] <> 0) or (barr[4] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
{$ENDIF}
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov w, 2
  end;
  if (barr[0] <> 2) or (barr[1] <> 0) or (barr[2] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  FillChar(barr, SizeOf(barr), $ff);
  asm
    mov b, 1
  end;
  if (barr[0] <> 1) or (barr[1] <> 255) or (barr[2] <> 255) then
  begin
    Writeln('Error!');
    Halt(1);
  end;
  Writeln('Ok!');
end.
