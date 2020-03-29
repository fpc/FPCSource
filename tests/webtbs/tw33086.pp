{ %cpu=avr }
program test;

begin
  asm
    ldd r20, z+0
    std y+0, r20
  end;
end.
