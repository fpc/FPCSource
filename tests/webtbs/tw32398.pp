{ %cpu=arm }

procedure p;assembler;
asm
  fmrrd r0, r1, d0
  fmdrr d0, r0, r1
end;

begin
end.

