{ %cpu=i386}
{ %fail }

{$mode objfpc}{$ASMMODE INTEL}
function Test():qword;assembler;nostackframe;
asm
  mov result,1
end;
begin
  writeln(Test());
end.
