{ %CPU=avr }
{$mode objfpc}
program intmathtest;

// Pointer to caller supplied variable in R24:R25
procedure test(out x: byte); assembler; nostackframe;
asm
  movw R30, R24
  st Z, R1
end;

var a8: byte;

begin
  test(a8);
end.
