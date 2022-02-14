{ %norun }
{ %cpu=avr }
program indirect_absolute_asm;


var
  PORTB : byte absolute 1;
  TXPORT: byte absolute PORTB;

begin
  asm
    sbi TXPORT+(-32), 1
  end;
end.
