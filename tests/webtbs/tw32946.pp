{ %CPU=avr }
{ %norun }
program test;
begin
  asm
    rjmp -2
  end;
end.
