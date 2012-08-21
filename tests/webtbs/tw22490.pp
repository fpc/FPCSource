{ %cpu=i386 }

{$mode objfpc}{$ASMMODE INTEL}
function Test():byte;assembler;nostackframe;
asm
  mov result,1
end;
begin
  if Test()<>1 then
    halt(1);
end.
